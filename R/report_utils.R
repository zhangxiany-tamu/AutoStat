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

#' Generate a simplified HTML report with improved titles and comprehensive model handling
#'
#' @param data Dataset analyzed
#' @param question Research question
#' @param analysis_plan Analysis plan text
#' @param executable_code R code used for analysis
#' @param results Analysis results including plots and model objects
#' @param processed_interpretation Processed interpretation HTML
#' @param output_dir Directory where report and images should be saved (absolute path)
#' @param filename Report filename (e.g., "analysis_report.html")
#' @param llm_connection Optional LLM connection for generating better titles
#' @return Path to the generated report
#' @keywords internal
generate_simplified_report <- function(data, question, analysis_plan, executable_code,
                                       results, processed_interpretation, output_dir,
                                       filename = "analysis_report.html", llm_connection = NULL) {
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
  diagnostics_html <- character(0)
  data_summary_html <- character(0)

  # Remove duplicates before processing
  results <- remove_duplicate_statistical_objects(results)

  # Enhanced create_meaningful_title function with better formula extraction and optional LLM assistance
  create_meaningful_title <- function(name, obj, data = NULL, default_prefix = "Analysis Result", llm_connection = NULL) {
    # Helper function to extract formula from model objects
    extract_clean_formula <- function(model_obj) {
      tryCatch({
        if (!is.null(model_obj$call$formula)) {
          # Get the actual formula, not the code
          formula_obj <- eval(model_obj$call$formula, envir = environment(model_obj$terms))
          if (inherits(formula_obj, "formula")) {
            formula_str <- deparse(formula_obj)
          } else {
            formula_str <- deparse(model_obj$call$formula)
          }

          # Clean up the formula string
          formula_str <- gsub("\\s+", " ", formula_str)
          formula_str <- trimws(formula_str)

          # Extract outcome variable (left side of ~)
          if (grepl("~", formula_str)) {
            outcome <- trimws(strsplit(formula_str, "~")[[1]][1])
            # Remove any function calls like log(), sqrt(), etc.
            outcome <- gsub("^[a-zA-Z_][a-zA-Z0-9_]*\\((.+)\\)$", "\\1", outcome)
            return(outcome)
          }
        }
        return(NULL)
      }, error = function(e) {
        return(NULL)
      })
    }

    # Try LLM assistance if connection is provided
    if (!is.null(llm_connection)) {
      tryCatch({
        # Create a brief description of the object for the LLM
        obj_description <- paste(
          "Object name:", name,
          "| Object class:", paste(class(obj), collapse = ", "),
          if (inherits(obj, c("lm", "glm"))) {
            formula_str <- if (!is.null(obj$call$formula)) deparse(obj$call$formula) else "unknown"
            paste("| Model formula:", formula_str)
          } else "",
          if (is.data.frame(obj)) paste("| Columns:", paste(colnames(obj)[1:min(5, ncol(obj))], collapse = ", ")) else "",
          if (is.numeric(obj) && !is.null(names(obj))) paste("| Named values:", paste(names(obj)[1:min(3, length(names(obj)))], collapse = ", ")) else ""
        )

        prompt <- paste0(
          "Generate a concise, descriptive title (maximum 6 words) for this R statistical object:\n",
          obj_description, "\n\n",
          "Rules:\n",
          "- Use title case\n",
          "- Be specific about the analysis type\n",
          "- Include the outcome variable if it's a model\n",
          "- Avoid generic terms like 'Analysis Result'\n",
          "- Examples: 'Temperature Regression Model', 'Species Classification Results', 'Correlation Matrix'\n\n",
          "Title:"
        )

        llm_title <- send_to_llm(llm_connection, prompt, llm_params = list(temperature = 0.1, max_tokens = 50))
        llm_title <- trimws(gsub("[\"']", "", llm_title))

        # Validate LLM response (basic checks)
        if (nchar(llm_title) > 0 && nchar(llm_title) <= 80 && !grepl("^(The |A |An )", llm_title)) {
          return(llm_title)
        }
      }, error = function(e) {
        # Fall through to manual logic if LLM fails
      })
    }

    # Enhanced manual title generation logic
    is_block_result <- grepl("^block_\\d+$", name)
    chunk_number_match <- regmatches(name, regexpr("\\d+", name))
    chunk_number <- if(length(chunk_number_match) > 0) chunk_number_match[1] else ""

    # Special handling for common analysis objects with better pattern matching
    if (grepl("vif|variance[._]inflation", name, ignore.case = TRUE) ||
        (is.numeric(obj) && length(obj) <= 10 &&
         any(grepl("temp|wind|pressure", names(obj), ignore.case = TRUE)))) {
      title <- "Multicollinearity Assessment (VIF)"

    } else if (grepl("correlation|cor[._]matrix", name, ignore.case = TRUE) ||
               (is.matrix(obj) && all(diag(obj) == 1, na.rm = TRUE) && all(obj >= -1 & obj <= 1, na.rm = TRUE))) {
      title <- "Correlation Matrix"

    } else if (grepl("summary|describe|descriptive", name, ignore.case = TRUE) && is.data.frame(obj)) {
      title <- "Descriptive Statistics"

    } else if ((grepl("tidy|coef", name, ignore.case = TRUE) && is.data.frame(obj) &&
                any(c("estimate", "term", "coefficient") %in% tolower(colnames(obj)))) ||
               (is.data.frame(obj) && "estimate" %in% tolower(colnames(obj)))) {
      title <- "Model Coefficients"

    } else if (inherits(obj, c("lm", "glm"))) {
      # Enhanced model title extraction
      outcome_var <- extract_clean_formula(obj)
      if (!is.null(outcome_var) && nchar(outcome_var) > 0) {
        model_type <- if (inherits(obj, "glm")) "GLM" else "Regression"
        title <- paste(tools::toTitleCase(outcome_var), model_type, "Model")
      } else {
        title <- if (inherits(obj, "glm")) "Generalized Linear Model" else "Linear Regression Model"
      }

    } else if (inherits(obj, "summary.lm")) {
      # Try to get the model from the summary if possible
      title <- "Regression Summary"

    } else if (inherits(obj, "htest")) {
      if (!is.null(obj$method)) {
        # Extract key parts of the test name
        test_name <- obj$method
        test_name <- gsub("'s|'s", "", test_name) # Remove possessives
        test_name <- gsub("\\s+test.*$", "", test_name, ignore.case = TRUE) # Remove " test" and everything after
        test_name <- gsub("^(.*?)\\s+(t-test|correlation|chi-squared).*$", "\\1 \\2", test_name, ignore.case = TRUE)
        title <- paste(tools::toTitleCase(test_name), "Test")
      } else {
        title <- "Statistical Test"
      }

    } else if (!is_block_result) {
      # Generic cleanup for named objects
      clean_name <- gsub("[._]", " ", name)
      clean_name <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_name) # Add space for camelCase
      clean_name <- tools::toTitleCase(clean_name)

      # Apply domain-specific patterns
      if (grepl("fitted|residual|diagnostic", clean_name, ignore.case = TRUE)) {
        title <- "Model Diagnostics"
      } else if (grepl("model", clean_name, ignore.case = TRUE)) {
        title <- paste("Model:", clean_name)
      } else {
        title <- clean_name
      }

    } else if (is_block_result && nzchar(chunk_number)) {
      # Enhanced block result naming
      if (inherits(obj, c("lm", "glm"))) {
        outcome_var <- extract_clean_formula(obj)
        if (!is.null(outcome_var) && nchar(outcome_var) > 0) {
          model_type <- if (inherits(obj, "glm")) "GLM" else "Regression"
          title <- paste(tools::toTitleCase(outcome_var), model_type, "Model")
        } else {
          title <- paste0("Regression Model (Step ", chunk_number, ")")
        }
      } else if (inherits(obj, "summary.lm")) {
        title <- paste0("Regression Summary (Step ", chunk_number, ")")
      } else if (inherits(obj, "htest")) {
        title <- paste0("Statistical Test (Step ", chunk_number, ")")
      } else {
        title <- paste0(default_prefix, " (Step ", chunk_number, ")")
      }
    } else {
      title <- default_prefix
    }

    # Final cleanup
    title <- gsub("\\s+", " ", title) # Remove multiple spaces
    title <- gsub("^Model: Model$", "Statistical Model", title, ignore.case = TRUE)
    title <- gsub("^([^:]+): \\1$", "\\1", title) # Remove "X: X" patterns
    title <- trimws(title)

    return(title)
  }

  # List of known model classes that broom supports well
  broom_supported_models <- c("lm", "glm", "gam", "nls", "survfit", "coxph",
                              "glmnet", "randomForest", "svm", "rpart", "kmeans",
                              "lme", "felm", "aov", "anova")

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

      # Track processed objects to avoid duplicates
      processed_objects <- character(0)

      # Skip if we've already processed this object (avoid duplicates)
      object_signature <- paste(class(res_item), collapse = "_")
      if (identical(res_item, get0("last_processed_item", ifnotfound = NULL))) {
        next
      }

      # Handle lm objects
      if (inherits(res_item, "lm") && has_broom && has_knitr) {
        # Skip if this is a summary of an lm object we already processed
        if (paste(res_name_orig, "lm") %in% processed_objects) next

        title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Linear Regression Model", llm_connection)
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          tidy_coeffs <- broom::tidy(res_item, conf.int = TRUE)
          model_tables_html <- c(model_tables_html,
                                 "<h4>Coefficient Estimates:</h4>",
                                 "<div class='table-responsive'>",
                                 knitr::kable(tidy_coeffs, format = "html",
                                              caption = "Regression coefficients with 95% confidence intervals",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          glance_summary <- broom::glance(res_item)
          model_tables_html <- c(model_tables_html,
                                 "<h4>Model Performance Statistics:</h4>",
                                 "<div class='table-responsive'>",
                                 knitr::kable(glance_summary, format = "html",
                                              caption = "Model fit statistics and performance metrics",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          model_tables_html <- c(model_tables_html, "<hr>")
          processed_objects <- c(processed_objects, paste(res_name_orig, "lm"))
          assign("last_processed_item", res_item, envir = environment())
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating table for linear model '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle glm objects
      else if (inherits(res_item, "glm") && has_broom && has_knitr) {
        title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Generalized Linear Model", llm_connection)
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          tidy_coeffs <- broom::tidy(res_item, conf.int = TRUE)
          model_tables_html <- c(model_tables_html,
                                 "<h4>Coefficient Estimates:</h4>",
                                 "<div class='table-responsive'>",
                                 knitr::kable(tidy_coeffs, format = "html",
                                              caption = "GLM coefficients with 95% confidence intervals",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          glance_summary <- broom::glance(res_item)
          model_tables_html <- c(model_tables_html,
                                 "<h4>Model Performance Statistics:</h4>",
                                 "<div class='table-responsive'>",
                                 knitr::kable(glance_summary, format = "html",
                                              caption = "GLM fit statistics and performance metrics",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          model_tables_html <- c(model_tables_html, "<hr>")
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating table for GLM '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle GAM objects (from mgcv package)
      else if (inherits(res_item, "gam") && has_broom && has_knitr) {
        title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Generalized Additive Model", llm_connection)
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          tidy_gam <- broom::tidy(res_item)
          model_tables_html <- c(model_tables_html,
                                 "<h4>Smooth Terms:</h4>",
                                 "<div class='table-responsive'>",
                                 knitr::kable(tidy_gam, format = "html",
                                              caption = "GAM smooth terms and significance tests",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          glance_gam <- broom::glance(res_item)
          model_tables_html <- c(model_tables_html,
                                 "<h4>Model Performance Statistics:</h4>",
                                 "<div class='table-responsive'>",
                                 knitr::kable(glance_gam, format = "html",
                                              caption = "GAM fit statistics and performance metrics",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          model_tables_html <- c(model_tables_html, "<hr>")
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating GAM table '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle anova objects (data.frame)
      else if (inherits(res_item, "anova") && inherits(res_item, "data.frame") && has_knitr) {
        title_prefix <- create_meaningful_title(display_res_name, res_item, data, "ANOVA Results", llm_connection)
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          res_item_df <- as.data.frame(res_item)
          clean_colnames_anova <- make.names(colnames(res_item_df), unique=TRUE)
          colnames(res_item_df) <- clean_colnames_anova
          model_tables_html <- c(model_tables_html,
                                 "<div class='table-responsive'>",
                                 knitr::kable(res_item_df, format = "html",
                                              caption = "Analysis of variance table comparing model fits",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          model_tables_html <- c(model_tables_html, "<hr>")
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating ANOVA table '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle htest objects (statistical tests)
      else if (inherits(res_item, "htest") && has_knitr) {
        title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Statistical Test", llm_connection)
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          model_tables_html <- c(model_tables_html, paste0("<p><strong>Test:</strong> ", res_item$method, "</p>"))

          if (!is.null(res_item$statistic)) {
            stat_name <- names(res_item$statistic)
            model_tables_html <- c(model_tables_html, paste0("<p><strong>", stat_name, ":</strong> ", round(res_item$statistic, 4), "</p>"))
          }

          if (!is.null(res_item$p.value)) {
            model_tables_html <- c(model_tables_html, paste0("<p><strong>p-value:</strong> ", format.pval(res_item$p.value, digits = 4, eps = 0.0001), "</p>"))
          }

          if (!is.null(res_item$conf.int)) {
            conf_level <- attr(res_item$conf.int, "conf.level") * 100
            model_tables_html <- c(model_tables_html, paste0("<p><strong>", conf_level, "% Confidence Interval:</strong> [",
                                                             round(res_item$conf.int[1], 4), ", ", round(res_item$conf.int[2], 4), "]</p>"))
          }

          if (!is.null(res_item$estimate)) {
            estimate_names <- names(res_item$estimate)
            estimate_values <- round(as.numeric(res_item$estimate), 4)
            if(length(estimate_names) == length(estimate_values) && length(estimate_values) > 0) {
              estimates_text <- paste(paste0(estimate_names, ": ", estimate_values), collapse=", ")
              model_tables_html <- c(model_tables_html, paste0("<p><strong>Estimates:</strong> ", estimates_text, "</p>"))
            }
          }

          model_tables_html <- c(model_tables_html, "<hr>")
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating test results for '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle summary.lm objects (fallback if broom not used/failed)
      else if (inherits(res_item, "summary.lm") && has_knitr) {
        # Skip if we already processed the corresponding lm object
        base_name <- gsub("_summary$", "", res_name_orig)
        if (paste(base_name, "lm") %in% processed_objects) next

        title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Linear Regression Summary", llm_connection)
        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))
          coef_table <- as.data.frame(res_item$coefficients)
          model_tables_html <- c(model_tables_html,
                                 "<h4>Coefficient Estimates:</h4>",
                                 "<div class='table-responsive'>",
                                 knitr::kable(coef_table, format = "html",
                                              caption = "Regression coefficients and significance tests",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>")
          if(!is.null(res_item$r.squared)) {
            model_tables_html <- c(model_tables_html, paste0("<p><strong>R-squared:</strong> ", round(res_item$r.squared, 4),
                                                             ", <strong>Adjusted R-squared:</strong> ", round(res_item$adj.r.squared, 4),"</p>"))
          }
          if(!is.null(res_item$fstatistic)) {
            fstat_val <- res_item$fstatistic
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
          processed_objects <- c(processed_objects, paste(res_name_orig, "summary.lm"))
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating regression summary '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle other model objects that broom supports - UPDATED SECTION
      else if (has_broom && has_knitr &&
               any(sapply(broom_supported_models, function(x) inherits(res_item, x))) &&
               !inherits(res_item, c("matrix", "data.frame", "numeric", "integer", "character", "logical"))) {

        # Determine model type for title
        model_type <- if (inherits(res_item, "nls")) "Nonlinear Least Squares Model"
        else if (inherits(res_item, "survfit")) "Survival Analysis"
        else if (inherits(res_item, "coxph")) "Cox Proportional Hazards Model"
        else if (inherits(res_item, "glmnet")) "Regularized Regression Model"
        else if (inherits(res_item, "randomForest")) "Random Forest Model"
        else if (inherits(res_item, "svm")) "Support Vector Machine"
        else if (inherits(res_item, "rpart")) "Decision Tree Model"
        else if (inherits(res_item, "kmeans")) "K-Means Clustering"
        else paste("Statistical Model (", class(res_item)[1], ")")

        title_prefix <- create_meaningful_title(display_res_name, res_item, data, model_type, llm_connection)

        tryCatch({
          model_tables_html <- c(model_tables_html, paste0("<h3>", title_prefix, "</h3>"))

          # Try to get tidy results
          tidy_results <- tryCatch(broom::tidy(res_item, conf.int = TRUE),
                                   error = function(e) tryCatch(broom::tidy(res_item), error = function(e2) NULL))

          if (!is.null(tidy_results)) {
            model_tables_html <- c(model_tables_html,
                                   "<h4>Model Results:</h4>",
                                   "<div class='table-responsive'>",
                                   knitr::kable(tidy_results, format = "html",
                                                caption = paste("Results from", model_type),
                                                table.attr = "class='table table-striped table-hover table-sm'"),
                                   "</div>")
          }

          # Try to get glance results
          glance_results <- tryCatch(broom::glance(res_item), error = function(e) NULL)
          if (!is.null(glance_results)) {
            model_tables_html <- c(model_tables_html,
                                   "<h4>Model Summary Statistics:</h4>",
                                   "<div class='table-responsive'>",
                                   knitr::kable(glance_results, format = "html",
                                                caption = paste("Summary statistics for", model_type),
                                                table.attr = "class='table table-striped table-hover table-sm'"),
                                   "</div>")
          }

          model_tables_html <- c(model_tables_html, "<hr>")
          processed_objects <- c(processed_objects, paste(res_name_orig, "broom_model"))
        }, error = function(e) {
          model_tables_html <- c(model_tables_html, paste0("<p><em>Error generating table for model '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle VIF values (numeric vectors with names) - NEW SECTION
      else if (is.numeric(res_item) && !is.null(names(res_item)) && length(res_item) <= 20 &&
               (grepl("vif", res_name_orig, ignore.case = TRUE) ||
                all(res_item >= 1, na.rm = TRUE))) {  # VIF values are always >= 1

        title_prefix <- create_meaningful_title(display_res_name, res_item, data, "VIF Values", llm_connection)

        tryCatch({
          # Convert to data frame for better display
          vif_df <- data.frame(
            Variable = names(res_item),
            VIF = round(res_item, 3),
            stringsAsFactors = FALSE
          )

          data_summary_html <- c(data_summary_html,
                                 paste0("<h3>", title_prefix, "</h3>"),
                                 "<div class='table-responsive'>",
                                 knitr::kable(vif_df, format = "html",
                                              caption = "Variance Inflation Factor values",
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>",
                                 "<p><em>VIF > 5 suggests multicollinearity, VIF > 10 indicates serious multicollinearity</em></p>",
                                 "<hr>")
        }, error = function(e) {
          data_summary_html <- c(data_summary_html,
                                 paste0("<p><em>Error generating VIF table '",
                                        current_html_escape(display_res_name), "': ",
                                        current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle generic data.frame objects (tables, summaries, etc.)
      else if (is.data.frame(res_item) && has_knitr && nrow(res_item) <= 50 && ncol(res_item) <= 20) {
        # Determine if this is a statistical summary or just data
        is_summary <- grepl("summary|stats|describe|tidy", res_name_orig, ignore.case = TRUE) ||
          any(c("mean", "sd", "median", "min", "max", "n", "estimate", "statistic", "p.value") %in% colnames(res_item))

        if (is_summary) {
          title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Statistical Summary", llm_connection)
          target_html <- data_summary_html
        } else {
          title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Data Table", llm_connection)
          target_html <- data_summary_html
        }
        tryCatch({
          new_content <- c(paste0("<h3>", title_prefix, "</h3>"),
                           "<div class='table-responsive'>",
                           knitr::kable(res_item, format = "html",
                                        caption = paste("Data table:", display_res_name),
                                        table.attr = "class='table table-striped table-hover table-sm'"),
                           "</div>", "<hr>")

          if (is_summary) {
            data_summary_html <- c(data_summary_html, new_content)
          } else {
            data_summary_html <- c(data_summary_html, new_content)
          }
        }, error = function(e) {
          error_msg <- paste0("<p><em>Error generating data table '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>")
          data_summary_html <- c(data_summary_html, error_msg)
        })
      }
      # Handle matrix objects
      else if (is.matrix(res_item) && has_knitr && nrow(res_item) <= 20 && ncol(res_item) <= 20) {
        # Determine if this is a correlation matrix or other statistical matrix
        is_correlation <- grepl("cor|correlation", res_name_orig, ignore.case = TRUE) ||
          (all(diag(res_item) == 1, na.rm = TRUE) && all(res_item >= -1 & res_item <= 1, na.rm = TRUE))

        if (is_correlation) {
          title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Correlation Matrix", llm_connection)
        } else {
          title_prefix <- create_meaningful_title(display_res_name, res_item, data, "Matrix Result", llm_connection)
        }

        tryCatch({
          df_matrix <- as.data.frame(res_item)
          data_summary_html <- c(data_summary_html,
                                 paste0("<h3>", title_prefix, "</h3>"),
                                 "<div class='table-responsive'>",
                                 knitr::kable(df_matrix, format = "html",
                                              caption = paste("Matrix:", display_res_name),
                                              table.attr = "class='table table-striped table-hover table-sm'"),
                                 "</div>", "<hr>")
        }, error = function(e) {
          data_summary_html <- c(data_summary_html, paste0("<p><em>Error generating matrix table '", current_html_escape(display_res_name), "': ", current_html_escape(e$message), "</em></p>"))
        })
      }
      # Handle other numeric vectors (not VIF) - UPDATED SECTION
      else if (is.numeric(res_item) && length(res_item) <= 20 &&
               !grepl("vif", res_name_orig, ignore.case = TRUE)) {
        # Skip these - they're likely intermediate calculations or single values
        # that don't need to be in the report
        next
      }
    }
  }

  if (!has_knitr) {
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
    ".table-responsive { display: block; width: 100%; overflow-x: auto; -webkit-overflow-scrolling: touch; }",
    ".table { margin-bottom: 2rem; font-size: 0.9rem; }", ".table th, .table td { padding: .5rem .75rem; }", ".table th { background-color: #e9ecef; }",
    "img { max-width: 100%; height: auto; border-radius: 4px; margin: 20px auto; box-shadow: 0 0 10px rgba(0,0,0,0.1); display: block; }",
    ".footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #dee2e6; font-size: 0.85em; color: #6c757d; text-align: center; }",
    "hr { margin-top: 2rem; margin-bottom: 2rem; border-top: 1px solid #dee2e6; }",
    "blockquote { background: #f8f9fa; border-left: 5px solid #007bff; margin: 1.5em 0px; padding: 0.8em 1.2em; font-style: italic; }",
    "blockquote p { display: inline; }",
    "</style>", "</head>", "<body>", "<div class='container'>",
    paste0("<h1>Statistical Analysis Report</h1>"), paste0("<p class='text-muted'>Date: ", formatted_date, "</p>"),
    paste0("<h2>Executive Summary</h2>"), paste0("<p>This report addresses the following research question:</p>"),
    paste0("<blockquote class='blockquote'><p class='mb-0'>", current_html_escape(question), "</p></blockquote>"),
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
        if (is.null(title) || title == "") {
          title <- tools::toTitleCase(gsub("_", " ", tools::file_path_sans_ext(plot_file_path)))
        }

        html_content <- c(html_content, paste0("<h3>", current_html_escape(title), "</h3>"))

        # Check if it's an interactive HTML file (plotly) or regular image
        if (tools::file_ext(plot_file_path) == "html") {
          # Embed plotly HTML as iframe for full interactivity
          html_content <- c(html_content,
                            "<div class='plotly-container' style='margin: 20px auto; border-radius: 8px; box-shadow: 0 0 15px rgba(0,0,0,0.1); overflow: hidden;'>",
                            paste0("<iframe src=\"", current_html_escape(plot_file_path),
                                   "\" width=\"100%\" height=\"600px\" frameborder=\"0\" style=\"border: none; display: block;\"></iframe>"),
                            "</div>")
        } else {
          # Regular static image (PNG, JPG, etc.)
          html_content <- c(html_content,
                            paste0("<img src=\"", current_html_escape(plot_file_path),
                                   "\" alt=\"", current_html_escape(title), "\" class='img-fluid'>"))
        }

        included_plot_filenames <- c(included_plot_filenames, plot_file_path)
      }
    }
    html_content <- c(html_content, "<hr>")
  }

  if (length(model_tables_html) > 0) {
    html_content <- c(html_content, "<h2>Statistical Model Summaries</h2>", model_tables_html)
  }

  if (length(diagnostics_html) > 0) {
    html_content <- c(html_content, "<h2>Model Diagnostics</h2>", diagnostics_html)
  }

  if (length(data_summary_html) > 0) {
    html_content <- c(html_content, "<h2>Data Summaries</h2>", data_summary_html)
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
#' and interpretation. This function is typically called by `auto_stat()`
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
#' @param llm_connection Optional LLM connection for generating better titles
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
                                     report_filename = "analysis_report.html",
                                     llm_connection = NULL) {
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

  # Generate the report with LLM connection
  generate_simplified_report(
    data = data,
    question = question,
    analysis_plan = analysis_plan,
    executable_code = executable_code,
    results = results_with_plots,
    processed_interpretation = processed_interpretation,
    output_dir = actual_output_dir,
    filename = report_filename,
    llm_connection = llm_connection
  )

  cat("Report generated at:", report_output_path, "\n")

  return(list(
    report_path = report_output_path,
    output_dir = actual_output_dir,
    timestamp = timestamp
  ))
}

#' Detect and remove duplicate statistical objects from results
#'
#' @description
#' Identifies duplicate statistical objects (like multiple copies of the same model)
#' and returns a cleaned list with only unique objects, prioritizing more informative names.
#'
#' @param results List of analysis results from execute_code
#' @return List with duplicates removed
#' @keywords internal
remove_duplicate_statistical_objects <- function(results) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    warning("Package 'digest' not available. Duplicate detection will be limited.")
    return(results)
  }

  # Track objects we've seen by their signature
  seen_signatures <- list()
  cleaned_results <- list()

  # Copy non-statistical objects first
  for (name in names(results)) {
    item <- results[[name]]

    # Keep non-statistical objects as-is
    if (name %in% c("plot_files", "plot_titles", "image_folder") ||
        grepl("_error$|_warning$|_status$|_output$", name) ||
        inherits(item, c("ggplot", "plotly", "recordedplot"))) {
      cleaned_results[[name]] <- item
      next
    }

    # Generate signature for statistical objects
    signature <- generate_object_signature_simple(item)

    if (signature %in% names(seen_signatures)) {
      # This is a duplicate - decide which one to keep
      existing_name <- seen_signatures[[signature]]

      # Prefer more descriptive names over "block_X" names
      if (grepl("^block_\\d+$", existing_name) && !grepl("^block_\\d+$", name)) {
        # Replace the existing one with this better-named one
        cleaned_results[[existing_name]] <- NULL
        cleaned_results[[name]] <- item
        seen_signatures[[signature]] <- name
      }
      # Otherwise, skip this duplicate (keep the first one)

    } else {
      # New unique object
      cleaned_results[[name]] <- item
      seen_signatures[[signature]] <- name
    }
  }

  return(cleaned_results)
}

#' Generate a simple signature for statistical objects to detect duplicates
#' @param object The statistical object
#' @return A character string signature
#' @keywords internal
generate_object_signature_simple <- function(object) {
  tryCatch({
    if (inherits(object, "lm")) {
      # Use coefficients and formula for signature
      return(digest::digest(list(object$coefficients, deparse(object$call$formula)), algo = "md5"))
    } else if (inherits(object, "summary.lm")) {
      # Use the coefficient table
      return(digest::digest(object$coefficients, algo = "md5"))
    } else if (inherits(object, "htest")) {
      # Use method, statistic, and p-value
      return(digest::digest(list(object$method, object$statistic, object$p.value), algo = "md5"))
    } else if (is.data.frame(object) && all(c("term", "estimate") %in% colnames(object))) {
      # This is likely a tidy() result - use estimates
      return(digest::digest(object[, c("term", "estimate")], algo = "md5"))
    } else {
      # Generic signature for other objects
      return(digest::digest(object, algo = "md5"))
    }
  }, error = function(e) {
    # Fallback if digest fails
    return(paste(class(object), collapse = "_"))
  })
}
