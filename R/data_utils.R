#' Summarize dataset with size-dependent approach and more detail
#'
#' Creates a text summary of the dataset structure and characteristics.
#' For small datasets, includes the entire dataset in the summary.
#'
#' @param data Dataset to summarize
#' @param include_full_data Whether to check and include the full dataset if small
#' @param max_rows Maximum number of rows to consider a dataset "small" for full inclusion
#' @param max_cols Maximum number of columns to consider a dataset "small" for full inclusion
#' @param max_factor_levels Maximum number of unique factor levels to list with frequencies
#' @return A character string describing the dataset
#' @keywords internal
summarize_dataset <- function(data, include_full_data = TRUE, max_rows = 50, max_cols = 20, max_factor_levels = 10) {
  # Number of rows and columns
  n_rows <- nrow(data)
  n_cols <- ncol(data)

  # Column types and basic stats
  col_summaries <- character(n_cols)
  for (i in 1:n_cols) {
    col_name <- names(data)[i]
    col_data <- data[[i]]
    col_type <- class(col_data)[1]
    missing_count <- sum(is.na(col_data))

    if (is.numeric(col_data)) {
      stats <- summary(col_data) # summary() gives Min, 1st Qu, Median, Mean, 3rd Qu, Max
      sd_val <- sd(col_data, na.rm = TRUE)
      col_summary <- sprintf(
        "- **%s** (numeric): Min=%.2f, Q1=%.2f, Median=%.2f, Mean=%.2f, Q3=%.2f, Max=%.2f, SD=%.2f, Missing=%d",
        col_name, stats["Min."], stats["1st Qu."], stats["Median"], stats["Mean"], stats["3rd Qu."], stats["Max."], sd_val, missing_count
      )
    } else if (is.factor(col_data)) {
      n_unique <- length(levels(col_data))
      col_summary <- sprintf(
        "- **%s** (factor): %d unique levels, Missing=%d.",
        col_name, n_unique, missing_count
      )
      if (n_unique > 0 && n_unique <= max_factor_levels) {
        level_counts <- table(col_data)
        level_text <- paste0(names(level_counts), ": ", level_counts, collapse = "; ")
        col_summary <- paste0(col_summary, " Levels (counts): ", level_text)
      }
    } else if (is.character(col_data)) {
      n_unique <- length(unique(col_data))
      col_summary <- sprintf(
        "- **%s** (character): %d unique values, Missing=%d.",
        col_name, n_unique, missing_count
      )
      if (n_unique > 0 && n_unique <= max_factor_levels) {
        # Convert to factor to easily get counts if it makes sense
        temp_factor <- factor(col_data)
        level_counts <- table(temp_factor)
        level_text <- paste0(names(level_counts), ": ", level_counts, collapse = "; ")
        col_summary <- paste0(col_summary, " Values (counts): ", level_text)
      }
    } else if (is.logical(col_data)) {
      counts <- table(col_data)
      level_text <- paste0(names(counts), ": ", counts, collapse = "; ")
      col_summary <- sprintf(
        "- **%s** (logical): Missing=%d. Counts: %s",
        col_name, missing_count, level_text
      )
    } else {
      col_summary <- sprintf(
        "- **%s** (%s): Missing=%d",
        col_name, col_type, missing_count
      )
    }
    col_summaries[i] <- col_summary
  }

  summary_text <- paste0(
    "Dataset with ", n_rows, " observations and ", n_cols, " variables:\n\n",
    paste(col_summaries, collapse = "\n")
  )

  # For small datasets, include the entire dataset
  if (include_full_data && n_rows <= max_rows && n_cols <= max_cols) {
    data_text <- capture.output(print(data, print.gap = 2))
    full_data_text <- paste(data_text, collapse = "\n")
    summary_text <- paste0(
      summary_text,
      "\n\nFull dataset (first ", nrow(data) ," rows shown if larger than print limit):\n\n",
      full_data_text
    )
  }
  return(summary_text)
}

#' Internal helper to describe a plot object textually
#' @param plot_obj The plot object
#' @param plot_name Name of the plot in results list
#' @param data The original dataset (for context, optional)
#' @return A string describing the plot
#' @keywords internal
describe_plot_object <- function(plot_obj, plot_name, data = NULL) {
  description_parts <- character(0)
  description_parts <- c(description_parts, paste0("### Textual Description of Plot: '", plot_name, "'"))

  if (inherits(plot_obj, "ggplot")) {
    description_parts <- c(description_parts, "Type: ggplot2 object.")

    # Titles and Labels
    plot_labels <- plot_obj$labels
    if (!is.null(plot_labels$title)) description_parts <- c(description_parts, paste0("- Title: ", plot_labels$title))
    if (!is.null(plot_labels$subtitle)) description_parts <- c(description_parts, paste0("- Subtitle: ", plot_labels$subtitle))
    if (!is.null(plot_labels$x)) description_parts <- c(description_parts, paste0("- X-axis Label: ", plot_labels$x))
    if (!is.null(plot_labels$y)) description_parts <- c(description_parts, paste0("- Y-axis Label: ", plot_labels$y))
    if (!is.null(plot_labels$caption)) description_parts <- c(description_parts, paste0("- Caption: ", plot_labels$caption))

    # Mappings (Aesthetics)
    mappings <- plot_obj$mapping
    aes_desc <- sapply(names(mappings), function(aes_name) {
      paste0(aes_name, ": ", rlang::as_label(mappings[[aes_name]]))
    })
    if (length(aes_desc) > 0) {
      description_parts <- c(description_parts, paste0("- Aesthetics: ", paste(aes_desc, collapse = ", ")))
    }

    # Layers (Geoms)
    layer_info <- sapply(plot_obj$layers, function(l) {
      geom_name <- class(l$geom)[1]
      stat_name <- class(l$stat)[1]
      aes_layer <- sapply(names(l$mapping), function(aes_name) {
        paste0(aes_name, ": ", rlang::as_label(l$mapping[[aes_name]]))
      })
      paste0(geom_name, " (using ", stat_name, ")", if(length(aes_layer)>0) paste0(" with layer aesthetics: ", paste(aes_layer, collapse=", ")) else "")
    })
    if (length(layer_info) > 0) {
      description_parts <- c(description_parts, paste0("- Layers (Geoms): ", paste(layer_info, collapse = "; ")))
    }

    # Faceting
    if (!is.null(plot_obj$facet) && class(plot_obj$facet)[1] != "FacetNull") {
      facet_vars <- names(plot_obj$facet$params$facets)
      facet_type <- class(plot_obj$facet)[1]
      if (length(facet_vars) > 0) {
        description_parts <- c(description_parts, paste0("- Faceting (", facet_type, "): by ", paste(facet_vars, collapse = ", ")))
      }
    }

    # Data (summary of data used in plot if possible, be careful with large data)
    # plot_data <- tryCatch(ggplot2::ggplot_build(plot_obj)$data[[1]], error = function(e) NULL)
    # if (!is.null(plot_data) && nrow(plot_data) > 0) {
    #   description_parts <- c(description_parts, paste0("- Plot Data Summary: The plot is built on data with ", nrow(plot_data), " data points (after transformations/stats). Key variables in plot data: ", paste(head(names(plot_data)), collapse=", ")))
    # }


    # Potential Inferred Patterns (very basic for now)
    # This part is highly experimental and would need significant expansion for true pattern detection.
    # For now, it focuses on identifying common plot types based on geoms.
    geom_types <- sapply(plot_obj$layers, function(l) class(l$geom)[1])

    if (any(c("GeomPoint") %in% geom_types)) {
      description_parts <- c(description_parts, "- Visual Interpretation Hint: This is likely a scatter plot, showing relationships between variables.")
    }
    if (any(c("GeomBar", "GeomCol") %in% geom_types)) {
      description_parts <- c(description_parts, "- Visual Interpretation Hint: This is likely a bar chart, used for comparing quantities across categories or groups.")
    }
    if (any(c("GeomHistogram", "GeomFreqpoly", "GeomDensity") %in% geom_types)) {
      description_parts <- c(description_parts, "- Visual Interpretation Hint: This plot likely shows the distribution of a single continuous variable.")
    }
    if (any(c("GeomBoxplot") %in% geom_types)) {
      description_parts <- c(description_parts, "- Visual Interpretation Hint: This is likely a boxplot, useful for comparing distributions across groups.")
    }
    if (any(c("GeomLine", "GeomPath") %in% geom_types)) {
      description_parts <- c(description_parts, "- Visual Interpretation Hint: This plot may show trends over time or ordered categories.")
    }

  } else if (inherits(plot_obj, "recordedplot") || (is.list(plot_obj) && "call" %in% names(plot_obj))) {
    description_parts <- c(description_parts, "Type: Base R plot (or lattice). Detailed introspection is limited.")
    if (is.list(plot_obj) && "call" %in% names(plot_obj)) {
      tryCatch({
        call_str <- deparse(plot_obj$call)
        description_parts <- c(description_parts, paste0("- Plot Call: ", call_str))
        # Try to infer type from call
        if(grepl("hist\\(", call_str)) description_parts <- c(description_parts, "- Visual Interpretation Hint: Likely a histogram.")
        if(grepl("boxplot\\(", call_str)) description_parts <- c(description_parts, "- Visual Interpretation Hint: Likely a boxplot.")
        if(grepl("barplot\\(", call_str)) description_parts <- c(description_parts, "- Visual Interpretation Hint: Likely a bar plot.")
        if(grepl("plot\\(", call_str) && !grepl("plot\\.default\\(", call_str) && !grepl("plot\\.ts\\(",call_str)) {
          # Check if it's plot(x,y) for scatter or plot(factor) for bar/box
          # This heuristic is very basic.
          description_parts <- c(description_parts, "- Visual Interpretation Hint: Could be a scatter plot or other general purpose plot.")
        }

      }, error = function(e) {
        description_parts <- c(description_parts, "- Could not deparse plot call.")
      })
    } else {
      description_parts <- c(description_parts, "- Plot object is a recordedplot, specific call details not readily available in this summary.")
    }
  } else {
    description_parts <- c(description_parts, paste0("Type: Unknown plot object (class: ", paste(class(plot_obj), collapse=", "), "). Cannot provide detailed textual description."))
  }

  return(paste(description_parts, collapse = "\n"))
}


#' Enhanced results summary function with detailed plot descriptions and improved error handling
#'
#' @description
#' Generates a detailed textual summary of analysis results, including model outputs,
#' statistical tests, and plot descriptions. This function is particularly useful for
#' understanding what visualizations show when you can't see them directly (e.g., when
#' working with an LLM).
#'
#' @param results List of analysis results from `execute_code()` or your own analysis.
#' @param data Original dataset used for analysis (for context).
#'
#' @return Text description of results including detailed plot analysis
#'
#' @examples
#' \dontrun{
#' # Execute some analysis code
#' analysis_results <- execute_code(mtcars, "
#'   model <- lm(mpg ~ wt + hp, data = data)
#'   summary_model <- summary(model)
#'   scatter_plot <- ggplot(data, aes(x = wt, y = mpg, color = hp)) +
#'     geom_point() +
#'     labs(title = 'MPG vs Weight, colored by Horsepower')
#' ")
#'
#' # Summarize the results
#' results_summary <- summarize_results(analysis_results, mtcars)
#' }
#' @export
summarize_results <- function(results, data) {
  summary_text_parts <- character(0) # Use a character vector to build the summary

  if (length(results) == 0) {
    return("No analysis results available.")
  }

  # --- Section for Errors and Warnings from Execution ---
  error_keys <- grep("_error$", names(results), value = TRUE)
  warning_keys <- grep("_warning$", names(results), value = TRUE)
  initial_error_key <- "initial_execution_error"
  fixed_code_error_key <- "fixed_code_final_error"

  if (length(error_keys) > 0 || length(warning_keys) > 0 ||
      !is.null(results[[initial_error_key]]) || !is.null(results[[fixed_code_error_key]])) {
    summary_text_parts <- c(summary_text_parts, "## Execution Issues Summary:")
    if(!is.null(results[[initial_error_key]])) {
      summary_text_parts <- c(summary_text_parts, paste0("- Initial Code Execution Error: ", results[[initial_error_key]]))
    }
    for (key in error_keys) {
      summary_text_parts <- c(summary_text_parts, paste0("- Error in ", sub("_error$", "", key), ": ", results[[key]]))
    }
    if(!is.null(results[[fixed_code_error_key]])) {
      summary_text_parts <- c(summary_text_parts, paste0("- Fixed Code Execution Error: ", results[[fixed_code_error_key]]))
    }
    for (key in warning_keys) {
      summary_text_parts <- c(summary_text_parts, paste0("- Warning in ", sub("_warning$", "", key), ": ", results[[key]]))
    }
    summary_text_parts <- c(summary_text_parts, "\n") # Add a newline for spacing
  }


  # --- Section for Non-Plot Objects ---
  summary_text_parts <- c(summary_text_parts, "# Statistical Outputs and Other Objects:")

  plot_objects_to_describe <- list() # Store plots here for later description

  for (name in names(results)) {
    # Skip internal/meta results, plot files/titles, and already handled errors/warnings
    if (name %in% c("prompt", "response", "code", "extracted_code", "code_response", "plan", "plan_and_code_response",
                    "plot_files", "plot_titles", "image_folder", initial_error_key, fixed_code_error_key) ||
        grepl("_error$", name) || grepl("_warning$", name) || grepl("_output$", name)) { # also skip raw block_X_output
      next
    }

    result_item <- results[[name]]
    if (is.null(result_item)) next

    # Check if it's a plot object
    is_plot <- inherits(result_item, "ggplot") ||
      inherits(result_item, "trellis") ||
      inherits(result_item, "recordedplot") ||
      (is.list(result_item) && "call" %in% names(result_item) && # Heuristic for base R plots
         any(sapply(c("plot", "hist", "boxplot", "barplot", "pie", "dotchart", "image", "contour", "persp"),
                    function(p_func) grepl(p_func, deparse(result_item$call)[1], fixed = TRUE))))

    if (is_plot) {
      plot_objects_to_describe[[name]] <- result_item
      next # Skip summarizing plots here, will do it in a dedicated section
    }

    # Process non-plot objects
    tryCatch({
      item_summary <- character(0)
      item_summary <- c(item_summary, paste0("## ", name, ":"))

      if (inherits(result_item, c("lm", "glm", "aov", "anova"))) {
        summary_obj <- summary(result_item)
        item_summary <- c(item_summary, "Model Summary:")
        item_summary <- c(item_summary, paste(capture.output(summary_obj), collapse = "\n"))
        if (inherits(result_item, "lm") && !is.null(summary_obj$r.squared)) {
          item_summary <- c(item_summary, paste0("R-squared: ", round(summary_obj$r.squared, 4),
                                                 ", Adjusted R-squared: ", round(summary_obj$adj.r.squared, 4)))
        }
      } else if (inherits(result_item, "htest")) {
        item_summary <- c(item_summary, paste0("Test: ", result_item$method))
        item_summary <- c(item_summary, paste0("Statistic (", names(result_item$statistic), "): ", round(result_item$statistic, 4)))
        item_summary <- c(item_summary, paste0("p-value: ", format.pval(result_item$p.value, digits = 4, eps = 0.0001)))
        if (!is.null(result_item$conf.int)) {
          item_summary <- c(item_summary, paste0("Confidence Interval (", attr(result_item$conf.int, "conf.level")*100, "%): [",
                                                 round(result_item$conf.int[1], 4), ", ", round(result_item$conf.int[2], 4), "]"))
        }
        if (!is.null(result_item$estimate)) {
          estimate_names <- names(result_item$estimate)
          estimate_values <- round(as.numeric(result_item$estimate), 4)
          if(length(estimate_names) == length(estimate_values) && length(estimate_values) > 0) {
            item_summary <- c(item_summary, paste0("Estimates: ", paste(paste0(estimate_names, ": ", estimate_values), collapse=", ")))
          } else if (length(estimate_values) > 0) {
            item_summary <- c(item_summary, paste0("Estimate(s): ", paste(estimate_values, collapse=", ")))
          }
        }


      } else if (is.data.frame(result_item) || is.matrix(result_item)) {
        item_summary <- c(item_summary, paste0("Data Frame/Matrix with ", nrow(result_item), " rows and ", ncol(result_item), " columns."))
        item_summary <- c(item_summary, paste0("Column names: ", paste(colnames(result_item), collapse = ", ")))
        if (nrow(result_item) <= 10 && ncol(result_item) <= 10) { # Show small data frames
          item_summary <- c(item_summary, "Content:")
          item_summary <- c(item_summary, paste(capture.output(print(result_item)), collapse = "\n"))
        } else {
          item_summary <- c(item_summary, "First few rows (max 5):")
          item_summary <- c(item_summary, paste(capture.output(print(head(result_item, 5))), collapse = "\n"))
        }
      } else if (is.numeric(result_item) && length(result_item) == 1) {
        item_summary <- c(item_summary, paste0("Value: ", round(result_item, 4)))
      } else if (is.numeric(result_item) || is.character(result_item) || is.logical(result_item)) {
        if (length(result_item) <= 20) { # Show short vectors
          item_summary <- c(item_summary, paste0("Vector (length ", length(result_item), "):"))
          if (!is.null(names(result_item))) {
            item_summary <- c(item_summary, paste(names(result_item), result_item, sep = ": ", collapse = ", "))
          } else {
            item_summary <- c(item_summary, paste(result_item, collapse = ", "))
          }
        } else {
          item_summary <- c(item_summary, paste0("Vector (class: ", class(result_item)[1] ,", length ", length(result_item), "). First 5 elements: ", paste(head(result_item, 5), collapse=", ")))
        }
      } else if (is.list(result_item)) {
        item_summary <- c(item_summary, paste0("List with ", length(result_item), " elements. Names: ", paste(names(result_item), collapse=", ")))
        # Optionally, could summarize first few elements of the list if they are simple
      }
      else {
        item_summary <- c(item_summary, paste0("Result of class: ", paste(class(result_item), collapse = ", "), ". Content not specifically formatted."))
        # Fallback: try to capture generic print output for unhandled types if it's not too long
        # output_capture <- capture.output(print(result_item))
        # if(length(output_capture) < 20) { # Limit length of generic output
        #    item_summary <- c(item_summary, paste(output_capture, collapse="\n"))
        # }
      }
      summary_text_parts <- c(summary_text_parts, item_summary, "\n") # Add a newline
    }, error = function(e) {
      summary_text_parts <<- c(summary_text_parts, paste0("## ", name, ":\nCould not summarize result due to an error: ", e$message, "\n"))
    })
  }

  # --- Section for Detailed Plot Descriptions ---
  if (length(plot_objects_to_describe) > 0) {
    summary_text_parts <- c(summary_text_parts, "# Detailed Plot Descriptions:")
    for (plot_name in names(plot_objects_to_describe)) {
      plot_obj <- plot_objects_to_describe[[plot_name]]
      tryCatch({
        plot_desc_text <- describe_plot_object(plot_obj, plot_name, data)
        summary_text_parts <- c(summary_text_parts, plot_desc_text, "\n")
      }, error = function(e) {
        summary_text_parts <<- c(summary_text_parts, paste0("### Textual Description of Plot: '", plot_name, "'\nError generating description: ", e$message, "\n"))
      })
    }
  } else {
    summary_text_parts <- c(summary_text_parts, "No plot objects found in results to describe textually.")
  }

  return(paste(summary_text_parts, collapse = "\n"))
}
