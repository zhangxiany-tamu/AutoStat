# AutoStat Shiny App - Server Logic

function(input, output, session) {

  # ===== REACTIVE VALUES =====
  rv <- reactiveValues(
    data = NULL,
    analysis_results = NULL,
    analysis_history = list()  # Store all past analyses
  )

  # ===== DATA HANDLING =====

  # Load data based on user selection
  get_data <- reactive({
    if (input$data_source == "sample") {
      sample_datasets[[input$sample_data]]
    } else {
      req(input$file_upload)
      tryCatch({
        read.csv(input$file_upload$datapath, stringsAsFactors = FALSE)
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        NULL
      })
    }
  })

  # Update reactive value when data changes
  observe({
    rv$data <- get_data()
  })

  # Check if data is uploaded
  output$dataUploaded <- reactive({
    !is.null(rv$data)
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)

  # Data preview
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(
      head(rv$data, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    )
  })

  # Data statistics
  output$n_rows <- renderText({
    req(rv$data)
    nrow(rv$data)
  })

  output$n_cols <- renderText({
    req(rv$data)
    ncol(rv$data)
  })

  output$n_numeric <- renderText({
    req(rv$data)
    sum(sapply(rv$data, is.numeric))
  })

  output$data_structure <- renderPrint({
    req(rv$data)
    str(rv$data)
  })

  # ===== CONFIGURATION =====

  # Use sample question button
  observeEvent(input$use_sample_question, {
    req(input$sample_data)
    updateTextAreaInput(session, "research_question",
                       value = sample_questions[[input$sample_data]])
  })

  # Dynamic model selection based on provider
  output$model_selection <- renderUI({
    req(input$llm_provider)

    # Get the models for the selected provider
    # Structure: c("model-id" = "Display Label", ...)
    models <- provider_models[[input$llm_provider]]

    selectInput(
      "llm_model",
      "Model:",
      choices = models,
      selected = names(models)[1]
    )
  })

  # Check if ready to analyze
  output$readyToAnalyze <- reactive({
    !is.null(rv$data) &&
      nchar(trimws(input$research_question)) > 0 &&
      nchar(trimws(input$api_key)) > 0
  })
  outputOptions(output, "readyToAnalyze", suspendWhenHidden = FALSE)

  # ===== ANALYSIS =====

  # Run analysis
  observeEvent(input$run_analysis, {
    req(rv$data, input$research_question, input$api_key, input$llm_provider, input$llm_model)

    # Switch to Results tab to show progress
    updateTabItems(session, "tabs", "results")

    # Ensure we're using the model ID, not the display label
    selected_model_id <- input$llm_model
    if (grepl("\\(.*\\)", selected_model_id) || !grepl("-|\\d", selected_model_id)) {
      models <- provider_models[[input$llm_provider]]
      matching_id <- names(models)[models == selected_model_id]
      if (length(matching_id) > 0) {
        selected_model_id <- matching_id[1]
      }
    }

    # Clean the API key
    clean_api_key <- trimws(input$api_key)

    # Log key info for debugging
    cat("\n=== API Key Debug Info ===\n")
    cat("Provider:", input$llm_provider, "\n")
    cat("Model:", selected_model_id, "\n")
    cat("Key length:", nchar(clean_api_key), "\n")
    cat("Key prefix:", substr(clean_api_key, 1, 7), "...\n")
    cat("Has whitespace:", nchar(input$api_key) != nchar(clean_api_key), "\n")
    cat("=========================\n\n")

    # Run analysis with built-in progress indicator
    withProgress(message = 'Running Statistical Analysis', value = 0, {
      tryCatch({
        incProgress(1/6, detail = "Initializing...")
        Sys.sleep(0.5)  # Brief pause to show initial progress

        incProgress(1/6, detail = "Generating analysis plan...")

        # Run auto_stat
        results <- auto_stat(
          data = rv$data,
          question = input$research_question,
          api_key = clean_api_key,
          llm_provider = input$llm_provider,
          llm_model = selected_model_id,
          llm_generation_params = list(
            temperature = input$temperature,
            maxOutputTokens = input$max_tokens
          ),
          include_full_data = input$include_full_data,
          output_dir = tempdir()
        )

        incProgress(4/6, detail = "Creating report...")

        rv$analysis_results <- results

        # Add to history with timestamp and dataset info
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        dataset_name <- if(input$data_source == "sample") input$sample_data else "Uploaded Data"

        rv$analysis_history <- c(
          list(list(
            timestamp = timestamp,
            dataset = dataset_name,
            question = input$research_question,
            results = results,
            id = length(rv$analysis_history) + 1
          )),
          rv$analysis_history  # Newest first
        )

        incProgress(1/6, detail = "Complete!")
        showNotification("Analysis completed successfully!", type = "message", duration = 5)

      }, error = function(e) {
        showNotification(paste("Analysis error:", e$message), type = "error", duration = 10)
      })
    })
  })


  # ===== RESULTS =====

  # Check if has results
  output$hasResults <- reactive({
    !is.null(rv$analysis_results)
  })
  outputOptions(output, "hasResults", suspendWhenHidden = FALSE)

  # Server-side rendering for results page
  output$results_content <- renderUI({
    # No results yet
    if (is.null(rv$analysis_results) && length(rv$analysis_history) == 0) {
      return(box(
        title = "No Results Yet",
        width = 12,

        div(
          class = "alert alert-info",
          icon("info-circle"),
          " No analysis has been run yet. Go to the Configuration tab to set up and start your analysis."
        )
      ))
    }

    # Results available
    box(
      title = "Analysis Report",
      width = 12,
      solidHeader = TRUE,

      div(
        class = "alert alert-success",
        icon("check-circle"),
        " Analysis completed successfully! View the report below or download it."
      ),

      # History selector if multiple analyses exist
      if (length(rv$analysis_history) > 1) {
        tagList(
          selectInput(
            "history_selector",
            "Select Analysis:",
            choices = setNames(
              seq_along(rv$analysis_history),
              sapply(rv$analysis_history, function(x) {
                paste0(x$timestamp, " - ", x$dataset, " (", substr(x$question, 1, 50), "...)")
              })
            ),
            selected = 1
          ),
          br()
        )
      },

      downloadButton("download_report", "Download HTML Report", class = "btn-primary"),

      br(), br(),

      uiOutput("report_frame")
    )
  })

  # Get selected report from history or current
  get_selected_report <- reactive({
    if (length(rv$analysis_history) > 1 && !is.null(input$history_selector)) {
      # Return selected report from history
      rv$analysis_history[[as.numeric(input$history_selector)]]$results
    } else if (length(rv$analysis_history) > 0) {
      # Return most recent from history
      rv$analysis_history[[1]]$results
    } else {
      # Fallback to current results
      rv$analysis_results
    }
  })

  # Embed the HTML report directly
  output$report_frame <- renderUI({
    selected_report <- get_selected_report()
    req(selected_report)
    req(selected_report$report_path)

    if (file.exists(selected_report$report_path)) {
      # Get the directory and filename
      report_dir <- dirname(selected_report$report_path)
      report_file <- basename(selected_report$report_path)

      # Create a UNIQUE resource path for each report directory
      # Use the directory name as part of the resource name to make it unique
      dir_hash <- digest::digest(report_dir, algo = "md5", serialize = FALSE)
      resource_name <- paste0("report_", dir_hash)

      # Add resource path so Shiny can serve the file
      # This will create a new path or update if it already exists
      addResourcePath(resource_name, report_dir)

      # Create the URL
      report_url <- paste0(resource_name, "/", report_file)

      tags$iframe(
        src = report_url,
        width = "100%",
        height = "1200px",
        frameborder = "0",
        style = "border: 1px solid #d2d2d7; border-radius: 8px;"
      )
    } else {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        " Report file not found. Please download the report using the button above."
      )
    }
  })

  # Download report (supports history selection)
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("autostat_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      selected_report <- get_selected_report()
      req(selected_report)
      file.copy(selected_report$report_path, file)
    }
  )
}
