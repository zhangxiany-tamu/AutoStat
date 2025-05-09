#' Execute R code from LLM with enhanced safety and error handling
#'
#' @description
#' Executes R code safely and captures the results, including any objects created,
#' printed output, errors, and warnings. This function is used internally by `auto_stat()`
#' and `auto_stat_two_step()` but can also be used directly.
#'
#' @param data The dataset to be available in the execution environment.
#' @param code_input A single string of R code, or a list/vector of R code strings (chunks).
#'                   It's assumed this code has already been processed by `process_gemini_code`.
#'
#' @return A list containing the results of execution, including any created objects,
#'         printed output, errors, and warnings for each block.
#'
#' @examples
#' \dontrun{
#' # Execute a single block of code
#' results <- execute_code(mtcars, "
#'   model <- lm(mpg ~ wt + hp, data = data)
#'   summary(model)
#'   plot(model)
#' ")
#'
#' # Execute multiple code chunks
#' results <- execute_code(iris, c(
#'   "# Exploratory Analysis\nlibrary(ggplot2)\nstr(data)",
#'   "# Create scatter plot\nggplot(data, aes(x = Sepal.Length, y = Petal.Length, color = Species))
#'   + geom_point()"
#' ))
#' }
#' @keywords internal
execute_code <- function(data, code_input) {
  # Ensure code_input is not NULL or empty
  if (is.null(code_input) || length(code_input) == 0 || all(nchar(trimws(unlist(code_input))) == 0)) {
    return(list(execution_error = "No executable code provided to execute_code function."))
  }

  executable_chunks <- list()

  # Determine if code_input is a single string that needs splitting, or already a list/vector of chunks
  if (is.character(code_input) && length(code_input) == 1 && !is.list(code_input)) {
    # Input is a single string, attempt to split it into logical chunks
    code_text_to_split <- code_input

    # Try splitting by explicit section markers first (e.g., "# 1. Section Name", "# A. Analysis Step")
    # Regex looks for lines starting with #, then a number/letter and a dot/space, then text.
    section_markers <- gregexpr("^#+\\s*([0-9A-Za-z]+[.\\)]\\s*)?[A-Za-z].*", code_text_to_split, perl = TRUE, ignore.case = TRUE)

    if (length(section_markers[[1]]) > 0 && section_markers[[1]][1] != -1 && length(section_markers[[1]]) > 1) {
      positions <- section_markers[[1]]
      chunk_list <- character(length(positions))
      for (i in 1:length(positions)) {
        start_pos <- positions[i]
        # Find the end of the line for the marker to ensure the marker itself is part of the chunk
        line_end_pos <- gregexpr("\\n", substr(code_text_to_split, start_pos, nchar(code_text_to_split)))[[1]][1]
        if(is.na(line_end_pos) || line_end_pos == -1) line_end_pos <- nchar(code_text_to_split) else line_end_pos <- start_pos + line_end_pos -1

        end_pos <- if (i < length(positions)) positions[i+1] - 1 else nchar(code_text_to_split)
        chunk_list[i] <- trimws(substr(code_text_to_split, start_pos, end_pos))
      }
      executable_chunks <- as.list(Filter(function(x) nchar(trimws(x)) > 0, chunk_list))
    } else {
      # If no clear section markers, or only one marker (implying one section),
      # try to split by common logical breaks (e.g., before library(), model assignments, ggplot calls)
      # This is a more heuristic approach.
      lines <- strsplit(code_text_to_split, "\n")[[1]]
      current_block_lines <- character(0)
      temp_chunks <- list()

      for (line in lines) {
        # Start a new block if we see a likely section header or important function call,
        # AND the current block has content.
        if (grepl("^# |^library\\(|^require\\(|^model[0-9_a-zA-Z]*\\s*<-|^[a-zA-Z0-9_.]*plot\\s*<-|^ggplot\\(", line) &&
            length(current_block_lines) > 0) {
          temp_chunks <- c(temp_chunks, list(paste(current_block_lines, collapse = "\n")))
          current_block_lines <- character(0) # Reset for new block
        }
        current_block_lines <- c(current_block_lines, line)
      }
      # Add the last accumulated block
      if (length(current_block_lines) > 0) {
        temp_chunks <- c(temp_chunks, list(paste(current_block_lines, collapse = "\n")))
      }
      executable_chunks <- Filter(function(x) nchar(trimws(x)) > 0, temp_chunks)
    }

    # If splitting resulted in no chunks, or the original string was short, use the original string as one chunk
    if (length(executable_chunks) == 0 || (length(executable_chunks) == 1 && nchar(executable_chunks[[1]]) < nchar(code_text_to_split)*0.8) ) {
      if (nchar(trimws(code_text_to_split)) > 0) {
        executable_chunks <- list(code_text_to_split)
      } else {
        return(list(execution_error = "No executable code found after attempting to split."))
      }
    }

  } else if (is.list(code_input) || is.character(code_input)) {
    # Input is already a list/vector of code strings (potentially from extract_code_blocks)
    executable_chunks <- as.list(unlist(code_input)) # Ensure it's a list of strings
    executable_chunks <- Filter(function(x) nchar(trimws(x)) > 0, executable_chunks) # Remove empty blocks
  } else {
    return(list(execution_error = "Invalid 'code_input' format. Must be a single string or a list/vector of strings."))
  }

  if (length(executable_chunks) == 0) {
    return(list(execution_error = "No non-empty code chunks to execute."))
  }

  # Create a safe environment for execution
  exec_env <- new.env(parent = globalenv()) # Access to loaded packages
  exec_env$data <- data

  # Results container
  results <- list()

  # Execute each code chunk
  for (i in seq_along(executable_chunks)) {
    code_chunk <- executable_chunks[[i]]
    block_name <- paste0("block_", i) # Name for results from this chunk's direct evaluation

    # Skip comment-only chunks (unless they are the only content, which filter should handle)
    if (grepl("^\\s*#", code_chunk) && !grepl("[^#\\s]", code_chunk)) { # Check if it's ONLY comments and whitespace
      results[[paste0(block_name, "_status")]] <- "Comment block - not executed"
      next
    }

    block_eval_result <- NULL
    printed_output <- NULL

    tryCatch({
      # Capture printed output and the result of the last expression in the chunk
      # Note: Base R plots drawn to a device won't be in block_eval_result unless recordPlot() is used.
      # ggplot objects will be in block_eval_result if they are the last expression.
      printed_output <- utils::capture.output({
        block_eval_result <- eval(parse(text = code_chunk), envir = exec_env)
      })

      # Store the direct result of the chunk's evaluation
      # This could be a model, a data frame, a plot object (if returned), etc.
      if (!is.null(block_eval_result)) {
        results[[block_name]] <- block_eval_result
      } else {
        results[[paste0(block_name, "_status")]] <- "Chunk evaluated, returned NULL"
      }

      # Store any printed output
      if (length(printed_output) > 0) {
        results[[paste0(block_name, "_output")]] <- printed_output
      }

    }, error = function(e) {
      results[[paste0(block_name, "_error")]] <- paste("Error in '", block_name, "': ", e$message, sep="")
      # Also capture any partial printed output before the error
      if (length(printed_output) > 0) {
        results[[paste0(block_name, "_partial_output_before_error")]] <- printed_output
      }
    }, warning = function(w) {
      results[[paste0(block_name, "_warning")]] <- paste("Warning in '", block_name, "': ", w$message, sep="")
      # Warnings don't stop execution, so block_eval_result and printed_output should be captured before this.
      # Re-store them if they were captured before the warning handler potentially overwrote them (though less likely for warnings).
      if (!is.null(block_eval_result) && !paste0(block_name) %in% names(results)) {
        results[[block_name]] <- block_eval_result
      }
      if (length(printed_output) > 0 && !paste0(block_name, "_output") %in% names(results)) {
        results[[paste0(block_name, "_output")]] <- printed_output
      }
    })
  } # End loop over executable_chunks

  # After all chunks are executed, copy all objects created in the exec_env to the results list,
  # unless they were already captured as a block's direct evaluation result (e.g. results$block_1).
  # This ensures objects assigned to variables within chunks are also available.
  env_objects <- ls(exec_env)
  for (obj_name in env_objects) {
    if (obj_name == "data") next # Don't re-copy the input data

    # Check if this object name (e.g., "my_model") was already stored as a direct block result (e.g. results$my_model)
    # This is to avoid overwriting if a block was named e.g. `my_model <- ...` and block name was `block_X`
    # However, block names are `block_1`, `block_2` etc.
    # So, we only need to check if obj_name is one of `block_1`, `block_2` etc.
    # A simpler check: if `results[[obj_name]]` is NULL or doesn't exist, then add it.
    # This handles cases where an object `foo` is created, and a block `block_X` also happens to be named `foo`.

    # If the object from the environment is not already in results under its own name, add it.
    # This is important for objects created and assigned to variables within the executed code.
    if (!obj_name %in% names(results)) {
      results[[obj_name]] <- exec_env[[obj_name]]
    } else {
      # If obj_name is already in results (e.g. results$block_1 which is an object, and obj_name is also "block_1"),
      # we usually don't want to overwrite.
      # However, if `results[[obj_name]]` was just a status message like "Chunk evaluated, returned NULL",
      # and `exec_env[[obj_name]]` is a proper object, we might prefer the object.
      # For simplicity, the current logic is that if `block_X` returned an object, it's stored.
      # If `some_var` was created in `exec_env`, it will be copied as `results$some_var`.
      # This seems fine. The previous check `!obj %in% names(results)` was good.
    }
  }

  # Ensure data itself is not part of the environment dump if it was not modified (it shouldn't be by typical analysis)
  # results$data <- NULL # Or decide if the (potentially modified) data object should be returned

  return(results)
}

