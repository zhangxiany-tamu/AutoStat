#' Sanitize a string to be a valid and clean filename
#'
#' @description
#' Converts a string to a valid filename by replacing spaces with underscores,
#' removing most non-alphanumeric characters, and ensuring the result is clean
#' and consistent.
#'
#' @param input_string The string to sanitize.
#' @param replacement_char Character to replace spaces and invalid characters.
#'
#' @return A sanitized string suitable for filenames.
#'
#' @keywords internal
sanitize_filename <- function(input_string, replacement_char = "_") {
  if (is.null(input_string) || !is.character(input_string) || length(input_string) == 0) {
    return(paste0("plot_",as.numeric(Sys.time()))) # Fallback for NULL or empty
  }
  # Remove http/https schemas if present (can come from plot titles if URLs are involved)
  clean_string <- gsub("https?://[^\\s]+", "", input_string)
  # Convert to lowercase
  clean_string <- tolower(clean_string)
  # Replace spaces and common separators with replacement_char
  clean_string <- gsub("[[:space:]]|[.-]", replacement_char, clean_string)
  # Remove any characters that are not alphanumeric or the replacement_char
  clean_string <- gsub(paste0("[^a-z0-9", replacement_char, "]"), "", clean_string)
  # Replace multiple consecutive replacement_char with a single one
  clean_string <- gsub(paste0(replacement_char, "{2,}"), replacement_char, clean_string)
  # Remove leading or trailing replacement_char
  clean_string <- gsub(paste0("^", replacement_char, "|", replacement_char, "$"), "", clean_string)

  if (nchar(clean_string) == 0) {
    return(paste0("plot_",as.numeric(Sys.time()))) # Fallback if string becomes empty
  }
  return(clean_string)
}

#' Improved function to generate meaningful plot titles
#'
#' @description
#' Generates a meaningful title for a plot based on its content, aesthetics, and structure.
#' It prioritizes existing titles in ggplot objects and attempts to infer appropriate titles
#' for other plot types.
#'
#' @param name Original plot name from results list (e.g., "block_1", "my_scatter")
#' @param plot_obj The plot object to analyze
#' @param data Original dataset (currently not used here but kept for API consistency)
#'
#' @return A meaningful title string for the plot.
#'
#' @keywords internal
generate_plot_title <- function(name, plot_obj, data = NULL) {
  # 1. Prioritize existing title in ggplot objects
  if (inherits(plot_obj, "ggplot")) {
    if (!is.null(plot_obj$labels$title) && nzchar(plot_obj$labels$title)) {
      return(plot_obj$labels$title)
    }
    # Infer title if not explicitly set
    mappings <- plot_obj$mapping
    x_var <- if (!is.null(mappings$x)) rlang::as_label(mappings$x) else NULL
    y_var <- if (!is.null(mappings$y)) rlang::as_label(mappings$y) else NULL
    color_var <- if (!is.null(mappings$colour)) rlang::as_label(mappings$colour) else if (!is.null(mappings$color)) rlang::as_label(mappings$color) else NULL
    fill_var <- if (!is.null(mappings$fill)) rlang::as_label(mappings$fill) else NULL

    facet_vars <- character(0)
    if (!is.null(plot_obj$facet) && class(plot_obj$facet)[1] != "FacetNull") {
      facet_params <- plot_obj$facet$params
      if(!is.null(facet_params$facets) && length(facet_params$facets) > 0) { # For facet_wrap
        facet_vars <- names(facet_params$facets)
      } else if (!is.null(facet_params$rows) || !is.null(facet_params$cols)) { # For facet_grid
        row_vars <- unlist(lapply(facet_params$rows, rlang::as_label))
        col_vars <- unlist(lapply(facet_params$cols, rlang::as_label))
        facet_vars <- unique(c(row_vars[row_vars != "."], col_vars[col_vars != "."]))
      }
    }

    geom_types <- sapply(plot_obj$layers, function(l) class(l$geom)[1])
    main_geom <- if (length(geom_types) > 0) gsub("^Geom", "", geom_types[1]) else "Plot"

    title_parts <- c()
    if (main_geom == "Point" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Scatter Plot of", y_var, "vs.", x_var)
    else if (main_geom == "Bar" && !is.null(x_var)) title_parts <- c("Bar Chart of", x_var)
    else if (main_geom == "Col" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Bar Chart of", y_var, "by", x_var)
    else if (main_geom == "Histogram" && !is.null(x_var)) title_parts <- c("Histogram of", x_var)
    else if (main_geom == "Density" && !is.null(x_var)) title_parts <- c("Density Plot of", x_var)
    else if (main_geom == "Boxplot" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Boxplot of", y_var, "by", x_var)
    else if (main_geom == "Line" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Line Plot of", y_var, "vs.", x_var)
    else if (!is.null(x_var) && !is.null(y_var)) title_parts <- c(main_geom, "Plot of", y_var, "by", x_var)
    else if (!is.null(x_var)) title_parts <- c(main_geom, "Plot of", x_var)
    else title_parts <- c(tools::toTitleCase(gsub("_", " ", name))) # Fallback to original name

    if (!is.null(color_var)) title_parts <- c(title_parts, "by", color_var)
    if (!is.null(fill_var) && (is.null(color_var) || fill_var != color_var)) title_parts <- c(title_parts, "filled by", fill_var)
    if (length(facet_vars) > 0) title_parts <- c(title_parts, "faceted by", paste(facet_vars, collapse=" & "))

    return(paste(title_parts, collapse = " "))
  }

  # 2. For base R plots (or other types)
  if (is.list(plot_obj) && "call" %in% names(plot_obj)) {
    call_str <- deparse(plot_obj$call)
    # Basic inference from call (can be expanded)
    if (grepl("hist\\(", call_str)) return(paste("Histogram from", name))
    if (grepl("boxplot\\(", call_str)) return(paste("Boxplot from", name))
    # Add more base plot inferences if needed
  }

  # 3. Fallback: Clean up the original name from results list
  title <- gsub("[._]", " ", name)
  title <- gsub("([a-z])([A-Z])", "\\1 \\2", title) # Add space for camelCase
  title <- paste0(toupper(substr(title, 1, 1)), substr(title, 2, nchar(title))) # Title Case
  return(title)
}


#' Generate a structural signature for a plot object for duplicate detection
#'
#' @description
#' Creates a unique signature for a plot object based on its structure and aesthetics.
#' This is used to detect duplicate plots in the results list, avoiding redundant plots
#' in the output report.
#'
#' @param plot_obj The plot object.
#'
#' @return A character string signature.
#'
#' @keywords internal
generate_plot_signature <- function(plot_obj) {
  if (inherits(plot_obj, "ggplot")) {
    sig_parts <- character(0)
    # Mappings
    # Sort mapping names to ensure consistent order
    map_names <- sort(names(plot_obj$mapping))
    for (m_name in map_names) {
      sig_parts <- c(sig_parts, paste0(m_name, ":", rlang::as_label(plot_obj$mapping[[m_name]])))
    }
    # Layers (geom types and their specific aesthetics)
    # Sort layers by geom class name to ensure consistent order if multiple layers of same type exist (less likely to be an issue)
    layers_info <- sapply(plot_obj$layers, function(l) {
      geom_class <- class(l$geom)[1]
      layer_map_names <- sort(names(l$mapping))
      layer_aes <- paste(sapply(layer_map_names, function(lm_name) paste0(lm_name, ":", rlang::as_label(l$mapping[[lm_name]]))), collapse="|")
      paste0("geom:", geom_class, "(", layer_aes, ")")
    })
    sig_parts <- c(sig_parts, sort(layers_info)) # Sort layer info strings

    # Faceting
    if (!is.null(plot_obj$facet) && class(plot_obj$facet)[1] != "FacetNull") {
      facet_params <- plot_obj$facet$params
      facet_sig <- class(plot_obj$facet)[1] # Type of facet

      facet_vars_sig <- character(0)
      if(!is.null(facet_params$facets) && length(facet_params$facets) > 0) { # facet_wrap
        facet_vars_sig <- paste(sort(names(facet_params$facets)), collapse=",")
      } else if (!is.null(facet_params$rows) || !is.null(facet_params$cols)) { # facet_grid
        row_vars <- sort(unlist(lapply(facet_params$rows, rlang::as_label)))
        col_vars <- sort(unlist(lapply(facet_params$cols, rlang::as_label)))
        facet_vars_sig <- paste0("rows:", paste(row_vars, collapse=","), ";cols:", paste(col_vars, collapse=","))
      }
      if(nchar(facet_vars_sig) > 0) sig_parts <- c(sig_parts, paste0("facet:", facet_sig, "[", facet_vars_sig, "]"))
    }

    return(digest::digest(paste(sig_parts, collapse = "||"), algo = "md5"))
  } else if (inherits(plot_obj, "recordedplot")) {
    # For recordedplot, the object itself can be somewhat variable even if plots are visually identical.
    # Hashing the deparsed call might be an option, but `plot_obj$call` is often not present directly.
    # A more robust way would be to try and deparse the recorded plot, but this is complex.
    # As a fallback, we use a simple class-based signature. This might lead to over-saving base R plots if they are structurally similar but generated by different calls.
    # For now, we'll use a placeholder. True base R plot duplicate detection is harder without deeper inspection.
    return(paste0("recordedplot_", digest::digest(plot_obj, algo="md5"))) # Hash the object itself
  } else if (is.list(plot_obj) && "call" %in% names(plot_obj)) {
    # For base R plots stored as lists with a call
    return(digest::digest(deparse(plot_obj$call), algo = "md5"))
  }
  return(digest::digest(plot_obj, algo = "md5")) # Fallback: hash the object
}


#' Save plots to the output directory with improved duplicate detection.
#'
#' @param results List of analysis results from `execute_code`.
#' @param output_dir Directory to save images in.
#' @param data Original dataset (for context, passed to title generation).
#' @return Updated results list with `plot_files` (mapping original plot name to final filename)
#'         and `plot_titles` (mapping original plot name to final display title).
#' @keywords internal
save_plots_to_output_dir <- function(results, output_dir, data = NULL) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    warning("The 'digest' package is needed for robust plot duplicate detection but is not installed. Plots may be duplicated. Please install it.")
    # Fallback to a simpler non-digest signature if digest is not available
    generate_plot_signature_fallback <- function(plot_obj) {
      if(inherits(plot_obj, "ggplot")) return(paste(sapply(plot_obj$layers, function(l) class(l$geom)[1]), collapse="_"))
      return(class(plot_obj)[1])
    }
    current_generate_plot_signature <- generate_plot_signature_fallback
  } else {
    current_generate_plot_signature <- generate_plot_signature
  }

  # Identify all plot objects in the results
  plot_object_names <- character(0)
  for (name in names(results)) {
    # Skip names that are clearly not primary plot objects or are internal fields
    if (grepl("_plot$", name) || name %in% c("plot_files", "plot_titles", "image_folder")) next

    item <- results[[name]]
    if (inherits(item, "ggplot") || inherits(item, "trellis") || inherits(item, "recordedplot") ||
        (is.list(item) && "call" %in% names(item) && # Heuristic for base R plots from execute_code
         any(sapply(c("plot", "hist", "boxplot", "barplot"), function(p) grepl(p, deparse(item$call)[1]))))) {
      plot_object_names <- c(plot_object_names, name)
    }
  }

  if (length(plot_object_names) == 0) {
    results$plot_files <- list()
    results$plot_titles <- list()
    results$image_folder <- output_dir
    return(results)
  }

  saved_plot_info_by_signature <- list() # Stores: signature -> list(final_filename, final_title)

  # These will store the mapping from the original plot name in results to the final filename and title
  final_plot_files <- list()
  final_plot_titles <- list()

  for (original_name in plot_object_names) {
    plot_obj <- results[[original_name]]

    plot_signature <- current_generate_plot_signature(plot_obj)

    if (plot_signature %in% names(saved_plot_info_by_signature)) {
      # Duplicate plot based on signature
      existing_info <- saved_plot_info_by_signature[[plot_signature]]
      final_plot_files[[original_name]] <- existing_info$final_filename
      final_plot_titles[[original_name]] <- existing_info$final_title
      message(paste("Plot '", original_name, "' is a duplicate of an already processed plot (signature: ", substr(plot_signature,1,7),"). Using existing file: ", existing_info$final_filename, sep=""))
    } else {
      # New unique plot
      display_title <- generate_plot_title(original_name, plot_obj, data)

      # Determine filename: use original_name if not "block_X", else use sanitized title
      base_filename <- if (!grepl("^block_\\d+$", original_name)) {
        sanitize_filename(original_name)
      } else {
        sanitize_filename(display_title)
      }
      # Ensure filename uniqueness if multiple plots sanitize to the same name by appending a number
      proposed_filename_base <- file.path(output_dir, base_filename)
      actual_filename_png <- paste0(proposed_filename_base, ".png")

      # Simple uniqueness check by appending number if file exists (more robust needed for concurrent access)
      counter <- 1
      while(file.exists(actual_filename_png)) {
        actual_filename_png <- paste0(proposed_filename_base, "_", counter, ".png")
        counter <- counter + 1
      }

      # Relative filename for HTML report (basename)
      relative_filename_png <- basename(actual_filename_png)

      tryCatch({
        if (inherits(plot_obj, "ggplot")) {
          # Add the generated title to the plot object itself before saving
          # This ensures the title is embedded in the plot if it was inferred
          plot_to_save <- plot_obj + ggplot2::labs(title = display_title) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
          ggplot2::ggsave(filename = actual_filename_png, plot = plot_to_save, width = 10, height = 7, dpi = 300)
        } else if (inherits(plot_obj, "trellis")) {
          png(actual_filename_png, width = 3000, height = 2100, res = 300)
          print(plot_obj) # Trellis plots usually have titles handled internally or via main=
          dev.off()
        } else if (inherits(plot_obj, "recordedplot")) { # Base R plot
          png(actual_filename_png, width = 3000, height = 2100, res = 300)
          # It's good practice to set par(mar) before replaying for title space, but replayPlot should handle it.
          # The title for base R plots is often part of the original plot call.
          # If generate_plot_title inferred something different, we can add it.
          replayPlot(plot_obj)
          # Add title if it's not likely already there from the call (heuristic)
          # if(!is.null(display_title) && !grepl(display_title, deparse(plot_obj$call %||% ""))) {
          #    graphics::title(main = display_title)
          # }
          dev.off()
        } else if (is.list(plot_obj) && "call" %in% names(plot_obj)) { # Another heuristic for base R plots
          png(actual_filename_png, width = 3000, height = 2100, res = 300)
          # This re-evaluates the plot call. This is generally okay if the environment is right.
          # The `execute_code` function runs in a specific environment.
          # For simplicity, we assume the plot object `plot_obj` is self-contained enough or `eval(plot_obj$call)` works.
          # A safer way is if `execute_code` ensures recorded plots are always `recordedplot` objects.
          # For now, let's assume `plot_obj` itself can be printed or replayed if it's a base plot.
          # This part is tricky. `replayPlot` is for `recordedplot`.
          # If it's just a list with a call, we might need to eval it.
          # However, `execute_code` should ideally return `recordedplot` for base plots.
          # Let's assume if it's not ggplot/trellis/recordedplot, it's an error or unhandled type for now.
          warning(paste("Plot '", original_name, "' is of an unhandled list-call type for direct saving. Attempting to print it. Ensure base plots are captured as 'recordedplot'."))
          print(plot_obj) # Generic print attempt
          dev.off()
        } else {
          warning(paste("Plot '", original_name, "' is of an unrecognized type (", class(plot_obj)[1], ") and cannot be saved automatically."))
          relative_filename_png <- NA # Mark as not saved
        }

        if (!is.na(relative_filename_png)) {
          message(paste("Saved plot '", original_name, "' as '", relative_filename_png, "' with title '", display_title, "'", sep=""))
          # Store info for this new unique plot
          saved_plot_info_by_signature[[plot_signature]] <- list(
            final_filename = relative_filename_png,
            final_title = display_title
          )
          final_plot_files[[original_name]] <- relative_filename_png
          final_plot_titles[[original_name]] <- display_title
        }

      }, error = function(e) {
        warning(paste("Error saving plot '", original_name, "': ", e$message))
        final_plot_files[[original_name]] <- NA # Mark as not saved due to error
        final_plot_titles[[original_name]] <- paste("Error saving plot:", original_name)
      })
    }
  }

  results$plot_files <- final_plot_files
  results$plot_titles <- final_plot_titles
  results$image_folder <- output_dir # This is the absolute path to the folder

  return(results)
}

# Helper for null-coalescing (used in generate_plot_title for base R title)
`%||%` <- function(a, b) if (is.null(a)) b else a
