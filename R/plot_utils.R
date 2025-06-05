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
#' for other plot types, including plotly objects.
#'
#' @param name Original plot name from results list (e.g., "block_1", "my_scatter")
#' @param plot_obj The plot object to analyze
#' @param data Original dataset (currently not used here but kept for API consistency)
#'
#' @return A meaningful title string for the plot.
#'
#' @keywords internal
generate_plot_title <- function(name, plot_obj, data = NULL) {
  # 1. Handle plotly objects
  if (inherits(plot_obj, "plotly")) {
    # Try to extract title from plotly object
    if (!is.null(plot_obj$x$layout$title$text) && nzchar(plot_obj$x$layout$title$text)) {
      return(plot_obj$x$layout$title$text)
    } else if (!is.null(plot_obj$x$layout$title) && is.character(plot_obj$x$layout$title) && nzchar(plot_obj$x$layout$title)) {
      return(plot_obj$x$layout$title)
    }

    # Infer title from plotly data structure
    plot_data <- plot_obj$x$data
    if (length(plot_data) > 0 && !is.null(plot_data[[1]])) {
      trace <- plot_data[[1]]
      x_var <- if (!is.null(trace$name)) trace$name else "X"
      y_var <- if (!is.null(plot_obj$x$layout$yaxis$title$text)) plot_obj$x$layout$yaxis$title$text else "Y"
      z_var <- if (!is.null(plot_obj$x$layout$scene$zaxis$title$text)) plot_obj$x$layout$scene$zaxis$title$text else NULL

      # Get axis labels
      x_label <- if (!is.null(plot_obj$x$layout$xaxis$title$text)) plot_obj$x$layout$xaxis$title$text else
        if (!is.null(plot_obj$x$layout$scene$xaxis$title$text)) plot_obj$x$layout$scene$xaxis$title$text else "X"
      y_label <- if (!is.null(plot_obj$x$layout$yaxis$title$text)) plot_obj$x$layout$yaxis$title$text else
        if (!is.null(plot_obj$x$layout$scene$yaxis$title$text)) plot_obj$x$layout$scene$yaxis$title$text else "Y"
      z_label <- if (!is.null(plot_obj$x$layout$scene$zaxis$title$text)) plot_obj$x$layout$scene$zaxis$title$text else NULL

      # Determine plot type and create appropriate title
      if (!is.null(trace$type)) {
        if (trace$type == "scatter3d" && !is.null(z_label)) {
          return(paste("3D Scatter Plot:", z_label, "vs", x_label, "and", y_label))
        } else if (trace$type %in% c("scatter", "scattergl")) {
          return(paste("Scatter Plot:", y_label, "vs", x_label))
        } else if (trace$type == "bar") {
          return(paste("Bar Chart:", y_label, "by", x_label))
        } else if (trace$type == "histogram") {
          return(paste("Histogram of", x_label))
        }
      }

      # Fallback for plotly
      if (!is.null(z_label)) {
        return(paste("3D Plot:", z_label, "vs", x_label, "and", y_label))
      } else {
        return(paste("Interactive Plot:", y_label, "vs", x_label))
      }
    }

    # Final fallback for plotly
    if (grepl("^block_\\d+$", name)) {
      return("Interactive 3D Visualization")
    } else {
      return(paste("Interactive Plot:", tools::toTitleCase(gsub("_", " ", name))))
    }
  }

  # 2. Prioritize existing title in ggplot objects
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
    if (main_geom == "Point" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Scatter Plot of", y_var, "vs", x_var)
    else if (main_geom == "Bar" && !is.null(x_var)) title_parts <- c("Bar Chart of", x_var)
    else if (main_geom == "Col" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Bar Chart of", y_var, "by", x_var)
    else if (main_geom == "Histogram" && !is.null(x_var)) title_parts <- c("Histogram of", x_var)
    else if (main_geom == "Density" && !is.null(x_var)) title_parts <- c("Density Plot of", x_var)
    else if (main_geom == "Boxplot" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Boxplot of", y_var, "by", x_var)
    else if (main_geom == "Line" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Line Plot of", y_var, "vs", x_var)
    else if (main_geom == "Smooth" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Smoothed Trend of", y_var, "vs", x_var)
    else if (main_geom == "Errorbar" && !is.null(x_var) && !is.null(y_var)) title_parts <- c("Error Bar Plot of", y_var, "by", x_var)
    else if (!is.null(x_var) && !is.null(y_var)) title_parts <- c(main_geom, "Plot of", y_var, "vs", x_var)
    else if (!is.null(x_var)) title_parts <- c(main_geom, "Plot of", x_var)
    else title_parts <- c(tools::toTitleCase(gsub("_", " ", name))) # Fallback to original name

    if (!is.null(color_var)) title_parts <- c(title_parts, "by", color_var)
    if (!is.null(fill_var) && (is.null(color_var) || fill_var != color_var)) title_parts <- c(title_parts, "filled by", fill_var)
    if (length(facet_vars) > 0) title_parts <- c(title_parts, "faceted by", paste(facet_vars, collapse=" & "))

    return(paste(title_parts, collapse = " "))
  }

  # 3. For base R plots (or other types)
  if (is.list(plot_obj) && "call" %in% names(plot_obj)) {
    call_str <- deparse(plot_obj$call)
    # Basic inference from call (can be expanded)
    if (grepl("hist\\(", call_str)) return(paste("Histogram from", name))
    if (grepl("boxplot\\(", call_str)) return(paste("Boxplot from", name))
    if (grepl("barplot\\(", call_str)) return(paste("Bar Plot from", name))
    # Add more base plot inferences if needed
  }

  # 4. Fallback: Clean up the original name from results list
  # Special handling for block names to make them more descriptive
  if (grepl("^block_\\d+$", name)) {
    block_num <- gsub("^block_(\\d+)$", "\\1", name)
    return(paste("Analysis Result", block_num))
  }

  # Regular name cleanup
  title <- gsub("[._]", " ", name)
  title <- gsub("([a-z])([A-Z])", "\\1 \\2", title) # Add space for camelCase

  # Handle common plot naming patterns
  if (grepl("scatter", title, ignore.case = TRUE)) title <- gsub("scatter", "Scatter Plot", title, ignore.case = TRUE)
  if (grepl("histogram", title, ignore.case = TRUE)) title <- gsub("histogram", "Histogram", title, ignore.case = TRUE)
  if (grepl("boxplot", title, ignore.case = TRUE)) title <- gsub("boxplot", "Boxplot", title, ignore.case = TRUE)
  if (grepl("barplot", title, ignore.case = TRUE)) title <- gsub("barplot", "Bar Chart", title, ignore.case = TRUE)
  if (grepl("residual", title, ignore.case = TRUE)) title <- gsub("residual", "Residual Plot", title, ignore.case = TRUE)
  if (grepl("coefficient", title, ignore.case = TRUE)) title <- gsub("coefficient", "Coefficient Plot", title, ignore.case = TRUE)
  if (grepl("diagnostic", title, ignore.case = TRUE)) title <- gsub("diagnostic", "Diagnostic Plot", title, ignore.case = TRUE)

  # Capitalize first letter
  title <- paste0(toupper(substr(title, 1, 1)), substr(title, 2, nchar(title)))
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


#' Save plots to the output directory with improved duplicate detection and plotly support.
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
      if(inherits(plot_obj, "plotly")) return("plotly_object")
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
        inherits(item, "plotly") ||
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

      tryCatch({
        if (inherits(plot_obj, "ggplot")) {
          # Handle ggplot objects
          actual_filename_png <- paste0(proposed_filename_base, ".png")

          # Simple uniqueness check by appending number if file exists
          counter <- 1
          while(file.exists(actual_filename_png)) {
            actual_filename_png <- paste0(proposed_filename_base, "_", counter, ".png")
            counter <- counter + 1
          }

          relative_filename_png <- basename(actual_filename_png)

          # Add the generated title to the plot object itself before saving
          # This ensures the title is embedded in the plot if it was inferred
          plot_to_save <- plot_obj + ggplot2::labs(title = display_title) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
          ggplot2::ggsave(filename = actual_filename_png, plot = plot_to_save, width = 10, height = 7, dpi = 300)

          message(paste("Saved ggplot '", original_name, "' as '", relative_filename_png, "' with title '", display_title, "'", sep=""))
          saved_plot_info_by_signature[[plot_signature]] <- list(
            final_filename = relative_filename_png,
            final_title = display_title
          )
          final_plot_files[[original_name]] <- relative_filename_png
          final_plot_titles[[original_name]] <- display_title

        } else if (inherits(plot_obj, "plotly")) {
          # Handle plotly objects - save as interactive HTML widget
          if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
            warning("htmlwidgets package required to save plotly plots. Attempting to install...")
            tryCatch({
              install.packages("htmlwidgets", dependencies = TRUE, repos = 'https://cran.rstudio.com/')
            }, error = function(e) {
              warning("Failed to install htmlwidgets package: ", e$message)
            })
          }

          if (requireNamespace("htmlwidgets", quietly = TRUE)) {
            actual_filename_html <- paste0(proposed_filename_base, ".html")

            # Simple uniqueness check by appending number if file exists
            counter <- 1
            while(file.exists(actual_filename_html)) {
              actual_filename_html <- paste0(proposed_filename_base, "_", counter, ".html")
              counter <- counter + 1
            }

            relative_filename_html <- basename(actual_filename_html)

            # Save plotly as self-contained HTML widget
            htmlwidgets::saveWidget(plot_obj, actual_filename_html, selfcontained = TRUE, title = display_title)

            message(paste("Saved interactive plotly plot '", original_name, "' as '", relative_filename_html, "' with title '", display_title, "'", sep=""))
            saved_plot_info_by_signature[[plot_signature]] <- list(
              final_filename = relative_filename_html,
              final_title = display_title
            )
            final_plot_files[[original_name]] <- relative_filename_html
            final_plot_titles[[original_name]] <- display_title
          } else {
            warning(paste("Could not save plotly plot '", original_name, "' - htmlwidgets package not available"))
            final_plot_files[[original_name]] <- NA
            final_plot_titles[[original_name]] <- paste("Error saving plotly plot:", original_name)
          }

        } else if (inherits(plot_obj, "trellis")) {
          # Handle trellis/lattice objects
          actual_filename_png <- paste0(proposed_filename_base, ".png")

          counter <- 1
          while(file.exists(actual_filename_png)) {
            actual_filename_png <- paste0(proposed_filename_base, "_", counter, ".png")
            counter <- counter + 1
          }

          relative_filename_png <- basename(actual_filename_png)

          png(actual_filename_png, width = 3000, height = 2100, res = 300)
          print(plot_obj) # Trellis plots usually have titles handled internally or via main=
          dev.off()

          message(paste("Saved trellis plot '", original_name, "' as '", relative_filename_png, "' with title '", display_title, "'", sep=""))
          saved_plot_info_by_signature[[plot_signature]] <- list(
            final_filename = relative_filename_png,
            final_title = display_title
          )
          final_plot_files[[original_name]] <- relative_filename_png
          final_plot_titles[[original_name]] <- display_title

        } else if (inherits(plot_obj, "recordedplot")) {
          # Handle base R recorded plots
          actual_filename_png <- paste0(proposed_filename_base, ".png")

          counter <- 1
          while(file.exists(actual_filename_png)) {
            actual_filename_png <- paste0(proposed_filename_base, "_", counter, ".png")
            counter <- counter + 1
          }

          relative_filename_png <- basename(actual_filename_png)

          png(actual_filename_png, width = 3000, height = 2100, res = 300)
          replayPlot(plot_obj)
          dev.off()

          message(paste("Saved recorded plot '", original_name, "' as '", relative_filename_png, "' with title '", display_title, "'", sep=""))
          saved_plot_info_by_signature[[plot_signature]] <- list(
            final_filename = relative_filename_png,
            final_title = display_title
          )
          final_plot_files[[original_name]] <- relative_filename_png
          final_plot_titles[[original_name]] <- display_title

        } else if (is.list(plot_obj) && "call" %in% names(plot_obj)) {
          # Handle other base R plots stored as lists with calls
          actual_filename_png <- paste0(proposed_filename_base, ".png")

          counter <- 1
          while(file.exists(actual_filename_png)) {
            actual_filename_png <- paste0(proposed_filename_base, "_", counter, ".png")
            counter <- counter + 1
          }

          relative_filename_png <- basename(actual_filename_png)

          png(actual_filename_png, width = 3000, height = 2100, res = 300)
          warning(paste("Plot '", original_name, "' is of an unhandled list-call type for direct saving. Attempting to print it. Ensure base plots are captured as 'recordedplot'."))
          print(plot_obj) # Generic print attempt
          dev.off()

          message(paste("Saved list-call plot '", original_name, "' as '", relative_filename_png, "' with title '", display_title, "'", sep=""))
          saved_plot_info_by_signature[[plot_signature]] <- list(
            final_filename = relative_filename_png,
            final_title = display_title
          )
          final_plot_files[[original_name]] <- relative_filename_png
          final_plot_titles[[original_name]] <- display_title

        } else {
          warning(paste("Plot '", original_name, "' is of an unrecognized type (", class(plot_obj)[1], ") and cannot be saved automatically."))
          final_plot_files[[original_name]] <- NA
          final_plot_titles[[original_name]] <- paste("Error saving plot:", original_name)
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
