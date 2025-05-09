#' Process LLM interpretation output for formal reports with improved HTML formatting
#'
#' @param interpretation Text from LLM
#' @return Cleaned and formatted interpretation
#' @keywords internal
process_interpretation <- function(interpretation) {
  if (is.null(interpretation) || interpretation == "") return("<p>No interpretation provided.</p>")

  # Remove conversation starters like "Okay, here's..."
  interpretation <- gsub("^Okay,\\s+here'?s\\s+a[n]?\\s+(thorough\\s+)?interpretation\\s+of\\s+the\\s+findings,?\\s*", "", interpretation, ignore.case = TRUE, perl = TRUE)
  interpretation <- gsub("^Here'?s\\s+a[n]?\\s+(thorough\\s+)?interpretation\\s+of\\s+the\\s+findings,?\\s*", "", interpretation, ignore.case = TRUE, perl = TRUE)
  interpretation <- gsub("^Sure,\\s+here'?s\\s+an\\s+interpretation:?\\s*", "", interpretation, ignore.case = TRUE, perl = TRUE)

  # Remove any statements about "based on the provided R output" or similar
  interpretation <- gsub("based on the (provided|given) (R output|analysis results|summary text),?\\s*", "", interpretation, ignore.case = TRUE, perl = TRUE)

  # Remove any leading "The interpretation is as follows:" type phrases
  interpretation <- gsub("^The interpretation (is as follows|can be structured as follows):\\s*", "", interpretation, ignore.case = TRUE, perl = TRUE)

  # Convert markdown headers to proper HTML format
  # These use ^ which is fine with perl=TRUE even without (?m) as it matches start of string or start of line after \n if string has \n.
  interpretation <- gsub("^##\\s+([^\n]+)", "<h2>\\1</h2>", interpretation, perl = TRUE)
  interpretation <- gsub("^#\\s+([^\n]+)", "<h1>\\1</h1>", interpretation, perl = TRUE)

  # Convert markdown bold text to HTML
  interpretation <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", interpretation, perl = TRUE)
  interpretation <- gsub("__([^_]+)__", "<strong>\\1</strong>", interpretation, perl = TRUE)

  # Convert markdown italic text to HTML
  interpretation <- gsub("\\*(.*?)\\*", "<em>\\1</em>", interpretation, perl = TRUE)
  interpretation <- gsub("_([^_]+)_", "<em>\\1</em>", interpretation, perl = TRUE)


  # Handle bullet points properly
  # Replace (?m)^ with (^|\n) to explicitly match start of string or after a newline.
  # Corrected non-breaking space to regular space in replacement.
  interpretation <- gsub("(^|\\n)\\s*[-*+]\\s+(.*)", "\\1  <li>\\2</li>", interpretation, perl = TRUE)

  # Handle numbered lists
  interpretation <- gsub("(^|\\n)\\s*\\d+\\.\\s+(.*)", "\\1  <li>\\2</li>", interpretation, perl = TRUE)

  # Wrap sequences of <li> items with <ul>. This is a common pattern.
  # This regex is a bit more complex to handle optional surrounding <br> tags or existing <ul> tags.
  interpretation <- gsub("(<br\\s*/?>\\s*)*(<ul>|<ol>)?(\\s*(<li>.*?</li>\\s*)+)(</ul>|</ol>)?(<br\\s*/?>\\s*)*", "\n<ul>\n\\3\n</ul>\n", interpretation, perl = TRUE)
  interpretation <- gsub("</ul>\n\n<ul>", "", interpretation, perl = TRUE) # Merge adjacent lists

  # Note: A more robust Markdown-to-HTML conversion would typically involve parsing the document
  # into a tree structure rather than relying solely on sequential gsub calls.
  # For complex or nested structures, a dedicated Markdown parsing library might be more suitable.

  # Ensure paragraphs are wrapped in <p> tags if they aren't already part of a list or header
  lines <- strsplit(interpretation, "\n")[[1]]
  processed_lines <- character()
  in_list_tag <- FALSE # Tracks if we are inside <ul> or <ol> tags (though current logic wraps all lists in <ul>)

  for (line_idx in seq_along(lines)) {
    line <- lines[[line_idx]]
    trimmed_line <- trimws(line)

    # Check if the line is already an HTML block element or list item
    if (grepl("^<(ul|ol|li|h[1-6]|table|thead|tbody|tr|th|td|p|div|blockquote|pre)", trimmed_line, ignore.case = TRUE)) {
      processed_lines <- c(processed_lines, line)
      if (grepl("^<(ul|ol)", trimmed_line, ignore.case = TRUE)) in_list_tag <- TRUE
      if (grepl("^</(ul|ol)", trimmed_line, ignore.case = TRUE)) in_list_tag <- FALSE
    } else if (trimmed_line == "") {
      # Keep single empty lines, which might contribute to paragraph spacing or be removed later
      if (length(processed_lines) == 0 || nzchar(trimws(processed_lines[length(processed_lines)]))) {
        processed_lines <- c(processed_lines, "")
      }
    } else {
      # Wrap non-empty, non-tagged lines in <p>
      # Avoid wrapping if it's likely content within a list item that wasn't captured by <li> tag yet
      # (This part of logic can be tricky with regex alone)
      if (in_list_tag && !grepl("^<li>", trimmed_line, ignore.case = TRUE) &&
          length(processed_lines) > 0 && # Ensure processed_lines is not empty
          grepl("</li>$", trimws(processed_lines[length(processed_lines)]))) {
        # If in a list and the previous line was a list item, append to it (simple heuristic)
        # This is a very basic attempt to handle multi-line list items without proper parsing.
        # For true multi-line list items, markdown usually requires indentation.
        # This might be too aggressive or not general enough.
        # processed_lines[length(processed_lines)] <- sub("</li>$", paste0("<br>", line, "</li>"), processed_lines[length(processed_lines)])
        # Safer: just wrap in <p> or let it be, depending on desired strictness
        processed_lines <- c(processed_lines, paste0("<p>", line, "</p>"))
      } else {
        processed_lines <- c(processed_lines, paste0("<p>", line, "</p>"))
      }
    }
  }
  interpretation <- paste(processed_lines, collapse = "\n")

  # Clean up: remove <p> tags around list/header blocks, and remove empty <p> tags
  interpretation <- gsub("<p>\\s*<(ul|ol|h[1-6]|table|pre|blockquote)", "<\\1", interpretation, ignore.case = TRUE, perl = TRUE)
  interpretation <- gsub("</(ul|ol|h[1-6]|table|pre|blockquote)>\\s*</p>", "</\\1>", interpretation, ignore.case = TRUE, perl = TRUE)
  interpretation <- gsub("<p>\\s*</p>", "", interpretation, ignore.case = TRUE, perl = TRUE) # Remove empty p tags
  interpretation <- gsub("(\n\\s*){2,}", "\n\n", interpretation, perl = TRUE) # Consolidate multiple newlines to double newlines (for paragraph breaks)
  interpretation <- gsub("^\n+", "", interpretation, perl=TRUE) # Remove leading newlines
  interpretation <- gsub("\n+$", "", interpretation, perl=TRUE) # Remove trailing newlines


  return(interpretation)
}

#' Create a single output directory for both reports and images
#'
#' @param base_dir Base directory for output files
#' @param prefix Prefix for folder names
#' @return List with directory path
#' @keywords internal
create_output_dir <- function(base_dir = getwd(), prefix = "auto_stat") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  }
  output_dir_name <- paste0(prefix, "_", timestamp)
  full_output_dir <- file.path(base_dir, output_dir_name)
  if (!dir.exists(full_output_dir)) {
    dir.create(full_output_dir, recursive = TRUE)
  }
  return(list(output_dir = full_output_dir, timestamp = timestamp))
}

#' Generate a simplified HTML report
#'
#' @param data Dataset analyzed
#' @param question Research question
#' @param analysis_plan Analysis plan text
#' @param executable_code R code used for analysis
#' @param results Analysis results including plots and model objects
#' @param processed_interpretation Processed interpretation HTML
#' @param output_dir Directory where report and images should be saved (absolute path)
#' @param filename Report filename (e.g., "analysis_report.html")
#' @return Path to the generated report
#' @keywords internal
generate_simplified_report <- function(data, question, analysis_plan, executable_code,
                                       results, processed_interpretation, output_dir,
                                       filename = "analysis_report.html") {
  output_file_path <- file.path(output_dir, filename)
  has_knitr <- requireNamespace("knitr", quietly = TRUE)
  has_broom <- requireNamespace("broom", quietly = TRUE)
  has_htmltools <- requireNamespace("htmltools", quietly = TRUE)

  current_html_escape <- if(has_htmltools) htmltools::htmlEscape else function(text) {
    text <- gsub("&", "&amp;", text); text <- gsub("<", "&lt;", text)
    text <- gsub(">", "&gt;", text); text <- gsub("\"", "&quot;", text)
    text <- gsub("'", "&#39;", text); return(text)
  }

  model_tables_html <- character(0)
  other_results_html <- character(0)

  if (length(results) > 0) {
    for (res_name_orig in names(results)) {
      # Skip plot file/title entries, image folder, and known internal fields
      if (res_name_orig %in% c("plot_files", "plot_titles", "image_folder",
                               "prompt", "response", "code", "extracted_code", "code_response",
                               "plan", "plan_and_code_response", "detailed_results_summary") ||
          grepl("_error$", res_name_orig) || grepl("_warning$", res_name_orig) || grepl("_status$", res_name_orig) ||
          grepl("_output$", res_name_orig)
      ) {
        next
      }

      res_item <- results[[res_name_orig]]
      display_res_name <- res_name_orig
      is_block_result <- grepl("^block_\\d+$", res_name_orig)
      chunk_number_match <- regmatches(res_name_orig, regexpr("^block_(\\d+)$", res_name_orig))
      chunk_number <- if(length(chunk_number_match) > 0 && length(chunk_number_match[[1]]) > 1) chunk_number_match[[1]][2] else ""


      # Handle lm objects
      if (inherits(res_item, "lm") && has_broom && has_knitr) {
        title_prefix <- if(is_block_result) paste0("Regression Model Summary (from Code Chunk ", chunk_number, ")") else paste0("Regression Model Summary: ", current_html_escape(display_res_name))
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          tidy_coeffs <- broom::tidy(res_item, conf.int = TRUE)
          model_tables_html <- c(model_tables_html, "<h4>Coefficients:</h4>", "<div class='table-responsive'>", knitr::kable(tidy_coeffs, format = "html", caption = paste("Coefficients for model:", current_html_escape(display_res_name)), table.attr = "class='table table-striped table-hover table-sm'"), "</div>")
          glance_summary <- broom::glance(res_item)
          model_tables_html <- c(model_tables_html, "<h4>Model Fit Statistics:</h4>", "<div class='table-responsive'>", knitr::kable(glance_summary, format = "html", caption = paste("Fit statistics for model:", current_html_escape(display_res_name)), table.attr = "class='table table-striped table-hover table-sm'"), "</div>")
          model_tables_html <- c(model_tables_html, "<hr>")
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating table for lm object '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle anova objects (data.frame)
      else if (inherits(res_item, "anova") && inherits(res_item, "data.frame") && has_knitr) {
        title_prefix <- if(is_block_result) paste0("ANOVA Table (from Code Chunk ", chunk_number, ")") else paste0("ANOVA Table: ", current_html_escape(display_res_name))
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          res_item_df <- as.data.frame(res_item)
          clean_colnames_anova <- make.names(colnames(res_item_df), unique=TRUE)
          colnames(res_item_df) <- clean_colnames_anova
          model_tables_html <- c(model_tables_html, "<div class='table-responsive'>", knitr::kable(res_item_df, format = "html", caption = paste("ANOVA table:", current_html_escape(display_res_name)), table.attr = "class='table table-striped table-hover table-sm'"), "</div>")
          model_tables_html <- c(model_tables_html, "<hr>")
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating table for anova object '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle summary.lm objects (fallback if broom not used/failed)
      else if (inherits(res_item, "summary.lm") && has_knitr) {
        title_prefix <- if(is_block_result) paste0("Regression Model Summary (Raw, from Code Chunk ", chunk_number, ")") else paste0("Regression Model Summary (Raw): ", current_html_escape(display_res_name))
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          coef_table <- as.data.frame(res_item$coefficients)
          model_tables_html <- c(model_tables_html, "<h4>Coefficients:</h4>", "<div class='table-responsive'>", knitr::kable(coef_table, format = "html", caption = paste("Coefficients for model:", current_html_escape(display_res_name)), table.attr = "class='table table-striped table-hover table-sm'"), "</div>")
          if(!is.null(res_item$r.squared)) {
            model_tables_html <- c(model_tables_html, paste0("<p><strong>R-squared:</strong> ", round(res_item$r.squared, 4),
                                                             ", <strong>Adjusted R-squared:</strong> ", round(res_item$adj.r.squared, 4),"</p>"))
          }
          if(!is.null(res_item$fstatistic)) {
            fstat_val <- res_item$fstatistic # Ensure it's correctly accessed
            # Check if fstat_val has the expected number of elements
            if(length(fstat_val) >= 3) {
              f_value <- fstat_val[1]
              df_num <- fstat_val[2]
              df_den <- fstat_val[3]
              p_value_f <- stats::pf(f_value, df_num, df_den, lower.tail = FALSE)
              model_tables_html <- c(model_tables_html, paste0("<p><strong>F-statistic:</strong> ", round(f_value, 2),
                                                               " on ", df_num, " and ", df_den, " DF, ",
                                                               "p-value: ", format.pval(p_value_f)), "</p>")
            } else {
              model_tables_html <- c(model_tables_html, "<p><em>F-statistic data is incomplete.</em></p>")
            }
          }
          model_tables_html <- c(model_tables_html, "<hr>")
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating raw table for summary.lm object '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle other non-plot, non-list, non-function results (only if not a block_X result, or if block_X is very simple)
      else if (!is_block_result && !is.null(res_item) && !inherits(res_item, c("ggplot", "trellis", "recordedplot", "list", "function"))) {
        tryCatch({
          title_for_other <- paste0("Other Result: ", current_html_escape(display_res_name), " (Class: ", class(res_item)[1], ")")
          other_results_html <- c(other_results_html, paste0("<h4>", title_for_other, "</h4>"))
          output_capture <- utils::capture.output(print(res_item))
          # Limit lines for display to avoid overly long pre blocks
          max_lines_other_results <- 50
          escaped_output <- current_html_escape(paste(utils::head(output_capture, max_lines_other_results), collapse="\n"))

          other_results_html <- c(other_results_html, "<pre><code>", escaped_output,
                                  if(length(output_capture) > max_lines_other_results) "\n... (output truncated)", "</code></pre><hr>")
        }, error = function(e){
          other_results_html <- c(other_results_html, paste0("<p><em>Error displaying 'Other Result' for '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
    }
  }
  if (!has_knitr) { # Broom message is included with knitr as they are often used together for tables
    model_tables_html <- c(model_tables_html, "<p><em>Note: For best model table formatting, please install the 'knitr' and 'broom' R packages.</em></p>")
  }

  # Temporarily set locale to "C" (standard English) for date formatting
  old_locale <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")

  # Format the date in English
  formatted_date <- format(Sys.Date(), "%B %d, %Y")

  # Restore the original locale
  Sys.setlocale("LC_TIME", old_locale)

  html_content <- c(
    "<!DOCTYPE html>", "<html lang='en'>", "<head>", "<meta charset='UTF-8'>", "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    "<title>Statistical Analysis Report</title>", "<link rel='stylesheet' href='https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css'>",
    "<style>",
    "body { font-family: 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; margin: 20px; line-height: 1.6; color: #333; background-color: #f9f9f9; }",
    ".container { max-width: 1140px; background-color: #fff; padding: 30px; border-radius: 8px; box-shadow: 0 0 15px rgba(0,0,0,0.1); }",
    "h1, h2, h3, h4 { color: #0056b3; margin-top: 1.8em; margin-bottom: 0.8em; }",
    "h1 { border-bottom: 3px solid #0056b3; padding-bottom: 0.4em; font-size: 2.5em; }",
    "h2 { border-bottom: 2px solid #dee2e6; padding-bottom: 0.3em; font-size: 2em; }",
    "h3 { font-size: 1.5em; color: #007bff; }", "h4 { font-size: 1.2em; color: #17a2b8; }",
    "pre { background-color: #e9ecef; padding: 15px; border: 1px solid #ced4da; border-radius: 4px; overflow-x: auto; font-size: 0.9em; white-space: pre-wrap; word-wrap: break-word; }",
    "code { font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, Courier, monospace; }",
    ".table-responsive { display: block; width: 100%; overflow-x: auto; -webkit-overflow-scrolling: touch; }", # Corrected line
    ".table { margin-bottom: 2rem; font-size: 0.9rem; }", ".table th, .table td { padding: .5rem .75rem; }", ".table th { background-color: #e9ecef; }",
    "img { max-width: 100%; height: auto; border-radius: 4px; margin: 20px auto; box-shadow: 0 0 10px rgba(0,0,0,0.1); display: block; }",
    ".footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #dee2e6; font-size: 0.85em; color: #6c757d; text-align: center; }",
    "hr { margin-top: 2rem; margin-bottom: 2rem; border-top: 1px solid #dee2e6; }",
    "blockquote { background: #f8f9fa; border-left: 5px solid #007bff; margin: 1.5em 0px; padding: 0.8em 1.2em; font-style: italic; }",
    "blockquote p { display: inline; }",
    "</style>", "</head>", "<body>", "<div class='container'>",
    paste0("<h1>Statistical Analysis Report</h1>"), paste0("<p class='text-muted'>Date: ", formatted_date, "</p>"),
    paste0("<h2>Executive Summary</h2>"), paste0("<p>This report addresses the following research question:</p>"),
    paste0("<blockquote class='blockquote'><p class='mb-0'>", current_html_escape(question), "</p></blockquote>"), # Escaped question
    paste0("<p><strong>Dataset Overview:</strong> A dataset with ", nrow(data), " observations and ", ncol(data), " variables was analyzed.</p>"), "<hr>",
    paste0("<h2>Dataset Sample</h2>"), "<div class='table-responsive'>",
    if(has_knitr) { knitr::kable(utils::head(data, 10), format="html", caption="First 10 rows of the dataset", table.attr = "class='table table-sm table-striped table-hover'")
    } else { paste0("<p><em>Dataset sample table requires 'knitr'. First 10 rows (raw):</em></p><pre><code>", current_html_escape(paste(utils::capture.output(print(utils::head(data,10))), collapse="\n")), "</code></pre>") },
    "</div><hr>"
  )

  if (!is.null(results$plot_files) && length(results$plot_files) > 0) {
    html_content <- c(html_content, "<h2>Visualizations</h2>")
    included_plot_filenames <- character(0)
    for (plot_orig_name in names(results$plot_files)) {
      plot_file_path <- results$plot_files[[plot_orig_name]]
      if (is.na(plot_file_path) || plot_file_path == "") next
      if (!(plot_file_path %in% included_plot_filenames)) {
        title <- results$plot_titles[[plot_orig_name]]
        if (is.null(title) || title == "") title <- tools::toTitleCase(gsub("_", " ", tools::file_path_sans_ext(plot_file_path)))
        html_content <- c(html_content, paste0("<h3>", current_html_escape(title), "</h3>"),
                          paste0("<img src=\"", current_html_escape(plot_file_path), "\" alt=\"", current_html_escape(title), "\" class='img-fluid'>"))
        included_plot_filenames <- c(included_plot_filenames, plot_file_path)
      }
    }
    html_content <- c(html_content, "<hr>")
  }

  if (length(model_tables_html) > 0) {
    html_content <- c(html_content, "<h2>Statistical Model Summaries</h2>", model_tables_html)
  }
  if (length(other_results_html) > 0) {
    html_content <- c(html_content, "<h2>Other Results</h2>", other_results_html)
  }

  html_content <- c(html_content, "<h2>Interpretation and Conclusions</h2>", processed_interpretation, "<hr>",
                    "<h2>Appendix</h2>", "<h3>Analysis Plan Overview</h3>", "<pre><code>", current_html_escape(analysis_plan), "</code></pre>",
                    "<h3>Analysis Code</h3>", "<pre><code>", current_html_escape(executable_code), "</code></pre>",
                    "<div class='footer'>Generated by auto_stat R package</div>", "</div>", "</body>", "</html>")

  tryCatch({
    if (!dir.exists(output_dir)) { dir.create(output_dir, recursive = TRUE, showWarnings = FALSE) }
    writeLines(html_content, output_file_path, useBytes = TRUE)
    message("Simplified HTML report generated at: ", output_file_path)
  }, error = function(e) {
    warning("Error writing simplified HTML report to '", output_file_path, "': ", current_html_escape(e$message))
  })

  return(output_file_path)
}

#' Generate a professional HTML report with integrated figures
#'
#' @description
#' Generates a professional HTML report containing the analysis results, visualizations, code,
#' and interpretation. This function is typically called by `auto_stat()` or `auto_stat_two_step()`
#' but can also be used directly to create custom reports.
#'
#' @param data Dataset analyzed
#' @param question Research question
#' @param analysis_plan Analysis plan text
#' @param executable_code R code used for analysis
#' @param results Analysis results including plots
#' @param interpretation Results interpretation
#' @param output_file Output file path (backward compatibility, primarily for setting output_dir and filename)
#' @param report_num Report number (backward compatibility, currently not used effectively)
#' @param output_dir Base directory where the timestamped output folder will be created.
#' @param report_filename Report filename (e.g., "analysis_report.html")
#'
#' @return A list containing:
#' \item{report_path}{The file path to the generated HTML report.}
#' \item{output_dir}{The directory where the report and associated files were saved.}
#' \item{timestamp}{The timestamp used for the output directory.}
#'
#' @examples
#' \dontrun{
#' # Generate a custom report from analysis results
#' report_info <- generate_enhanced_report(
#'   data = iris,
#'   question = "Can we distinguish iris species based on sepal and petal measurements?",
#'   analysis_plan = "1. Perform exploratory data analysis...",
#'   executable_code = "library(ggplot2)\nggplot(iris, aes(x = Sepal.Length, y = Sepal.Width,
#'   color = Species)) + geom_point()",
#'   results = my_analysis_results,
#'   interpretation = "The analysis reveals clear separation between species...",
#'   output_dir = "my_reports"
#' )
#'
#' # Open the generated report
#' browseURL(report_info$report_path)
#' }
#' @export
generate_enhanced_report <- function(data, question, analysis_plan, executable_code, results,
                                     interpretation, output_file = NULL, report_num = NULL,
                                     output_dir = "auto_stat_output",
                                     report_filename = "analysis_report.html") {
  # Determine the correct base output directory
  if (!is.null(output_file)) {
    path_from_output_file <- dirname(output_file)
    if (path_from_output_file != "." && path_from_output_file != output_dir && nzchar(path_from_output_file)) {
      # If output_file has an absolute path or different directory, use it
      # but only if it's not just "." (current directory)
      report_filename <- basename(output_file)

      # Check if we can extract an output directory from output_file path
      if (dirname(path_from_output_file) != "." && basename(path_from_output_file) != path_from_output_file) {
        # If dirname(path_from_output_file) is not ".", the path might already have a parent directory structure
        output_dir <- path_from_output_file
      }
    }
  }

  # Ensure the base output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create the timestamped subfolder within output_dir
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  report_dir_name <- paste0("auto_stat_report_", timestamp)
  actual_output_dir <- file.path(output_dir, report_dir_name)

  # Ensure the nested output directory exists
  if (!dir.exists(actual_output_dir)) {
    dir.create(actual_output_dir, recursive = TRUE)
  }

  processed_interpretation <- process_interpretation(interpretation)

  # Save plots to the nested output directory
  results_with_plots <- save_plots_to_output_dir(results, actual_output_dir, data)

  # Set the final report path in the nested directory
  report_output_path <- file.path(actual_output_dir, report_filename)

  cat("Generating HTML report in: ", actual_output_dir, "\n")

  # Generate the report
  generate_simplified_report(
    data = data,
    question = question,
    analysis_plan = analysis_plan,
    executable_code = executable_code,
    results = results_with_plots,
    processed_interpretation = processed_interpretation,
    output_dir = actual_output_dir,
    filename = report_filename
  )

  cat("Report generated at:", report_output_path, "\n")

  return(list(
    report_path = report_output_path,
    output_dir = actual_output_dir,
    timestamp = timestamp
  ))
}
