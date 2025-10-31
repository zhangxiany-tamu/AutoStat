# AutoStat Shiny App - User Interface
# Professional, clean, and elegant design

dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "AutoStat",
    titleWidth = 250
  ),

  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Configuration", tabName = "config", icon = icon("sliders-h")),
      menuItem("Results", tabName = "results", icon = icon("file-alt")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),

  # Body
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    tabItems(

      # ===== DATA TAB =====
      tabItem(
        tabName = "data",

        fluidRow(
          box(
            title = "Upload or Select Dataset",
            width = 12,
            solidHeader = TRUE,

            fluidRow(
              column(
                width = 6,
                radioButtons(
                  "data_source",
                  "Choose Data Source:",
                  choices = c("Upload CSV File" = "upload",
                             "Use Sample Dataset" = "sample"),
                  selected = "sample"
                )
              )
            ),

            conditionalPanel(
              condition = "input.data_source == 'upload'",
              fileInput(
                "file_upload",
                "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values", ".csv"),
                placeholder = "No file selected"
              ),
              div(
                class = "help-text",
                "Upload a CSV file with column headers. Maximum file size: 50MB."
              )
            ),

            conditionalPanel(
              condition = "input.data_source == 'sample'",
              selectInput(
                "sample_data",
                "Select Sample Dataset:",
                choices = c("mtcars", "iris", "airquality", "ToothGrowth"),
                selected = "mtcars"
              ),
              div(
                class = "help-text",
                "Sample datasets are provided for demonstration purposes."
              )
            )
          )
        ),

        fluidRow(
          conditionalPanel(
            condition = "output.dataUploaded",

            box(
              title = "Dataset Preview",
              width = 12,
              solidHeader = TRUE,

              DTOutput("data_preview")
            ),

            box(
              title = "Dataset Summary",
              width = 12,
              solidHeader = TRUE,

              fluidRow(
                column(4,
                  div(class = "stat-card",
                    div(class = "stat-value", textOutput("n_rows")),
                    div(class = "stat-label", "Observations")
                  )
                ),
                column(4,
                  div(class = "stat-card",
                    div(class = "stat-value", textOutput("n_cols")),
                    div(class = "stat-label", "Variables")
                  )
                ),
                column(4,
                  div(class = "stat-card",
                    div(class = "stat-value", textOutput("n_numeric")),
                    div(class = "stat-label", "Numeric Variables")
                  )
                )
              ),

              br(),
              verbatimTextOutput("data_structure")
            )
          )
        )
      ),

      # ===== CONFIGURATION TAB =====
      tabItem(
        tabName = "config",

        fluidRow(
          box(
            title = "Research Question",
            width = 12,
            solidHeader = TRUE,

            conditionalPanel(
              condition = "input.data_source == 'sample' && output.dataUploaded",
              actionButton(
                "use_sample_question",
                "Use Sample Question",
                class = "btn-default",
                icon = icon("lightbulb")
              ),
              br(), br()
            ),

            textAreaInput(
              "research_question",
              NULL,
              value = "",
              rows = 4,
              placeholder = "Enter your research question here. Be specific about what you want to analyze."
            ),

            div(
              class = "help-text",
              "Example: What factors are most strongly associated with fuel efficiency?"
            )
          )
        ),

        fluidRow(
          box(
            title = "LLM Configuration",
            width = 6,
            solidHeader = TRUE,

            selectInput(
              "llm_provider",
              "LLM Provider:",
              choices = c("Anthropic (Claude)" = "anthropic",
                         "OpenAI (GPT)" = "openai",
                         "Google (Gemini)" = "gemini"),
              selected = "anthropic"
            ),

            uiOutput("model_selection"),

            passwordInput(
              "api_key",
              "API Key:",
              placeholder = "Enter your API key"
            ),

            div(
              class = "help-text",
              HTML("Get API keys from:
                   <a href='https://console.anthropic.com/' target='_blank'>Anthropic</a> |
                   <a href='https://platform.openai.com/' target='_blank'>OpenAI</a> |
                   <a href='https://ai.google.dev/' target='_blank'>Google</a>")
            )
          ),

          box(
            title = "Advanced Parameters",
            width = 6,
            solidHeader = TRUE,

            sliderInput(
              "temperature",
              "Temperature (Creativity):",
              min = 0,
              max = 1,
              value = 0.3,
              step = 0.1
            ),

            sliderInput(
              "max_tokens",
              "Max Tokens:",
              min = 1000,
              max = 16000,
              value = 8000,
              step = 1000
            ),

            checkboxInput(
              "include_full_data",
              "Include full dataset in prompts (for small datasets)",
              value = TRUE
            ),

            div(
              class = "help-text",
              "Lower temperature = more focused responses. Higher max tokens = more detailed analyses."
            )
          )
        ),

        fluidRow(
          box(
            title = "Run Analysis",
            width = 12,
            solidHeader = TRUE,

            conditionalPanel(
              condition = "!output.dataUploaded",
              div(
                class = "alert alert-info",
                icon("info-circle"),
                " Please upload or select a dataset in the Data tab to continue."
              )
            ),

            conditionalPanel(
              condition = "output.dataUploaded && input.research_question == ''",
              div(
                class = "alert alert-warning",
                icon("exclamation-triangle"),
                " Please enter a research question above."
              )
            ),

            conditionalPanel(
              condition = "output.dataUploaded && input.research_question != '' && input.api_key == ''",
              div(
                class = "alert alert-warning",
                icon("exclamation-triangle"),
                " Please enter your API key above."
              )
            ),

            conditionalPanel(
              condition = "output.readyToAnalyze",
              div(
                class = "alert alert-success",
                icon("check-circle"),
                " Ready to analyze! Click the button below to start."
              ),

              br(),

              actionButton(
                "run_analysis",
                "Run Statistical Analysis",
                class = "btn-primary btn-lg",
                icon = icon("play"),
                width = "100%"
              )
            )
          )
        )
      ),

      # ===== RESULTS TAB =====
      tabItem(
        tabName = "results",

        # Always show - server-side rendering handles what to display
        fluidRow(
          uiOutput("results_content")
        )
      ),

      # ===== HELP TAB =====
      tabItem(
        tabName = "help",

        fluidRow(
          box(
            title = "About AutoStat",
            width = 12,
            solidHeader = TRUE,

            p(
              "AutoStat is an R package that uses Large Language Models (LLMs) to automate ",
              "statistical analysis workflows. It generates analysis plans, executable R code, ",
              "and professional reports based on your data and research questions."
            ),

            h4("Key Features"),
            tags$ul(
              tags$li("Multi-Provider Support: Compatible with Anthropic (Claude), OpenAI (GPT), and Google (Gemini)"),
              tags$li("Automated Analysis: Generates R code for statistical analysis from natural language questions"),
              tags$li("Two-Step Approach: Develops an analysis plan, then produces executable code"),
              tags$li("Professional Reports: Creates HTML reports with visualizations and interpretations")
            )
          )
        ),

        fluidRow(
          box(
            title = "Quick Start Guide",
            width = 12,
            solidHeader = TRUE,

            h4("Step 1: Upload Data"),
            p("Go to the Data tab and either upload your CSV file or select a sample dataset."),

            h4("Step 2: Configure Analysis"),
            p("In the Configuration tab:"),
            tags$ul(
              tags$li("Enter your research question"),
              tags$li("Select your preferred LLM provider and model"),
              tags$li("Provide your API key"),
              tags$li("Optionally adjust advanced parameters")
            ),

            h4("Step 3: Run Analysis"),
            p("Navigate to the Analysis tab and click 'Run Statistical Analysis' when ready."),

            h4("Step 4: View Results"),
            p("Once complete, go to the Results tab to view visualizations, statistical outputs, and interpretations."),

            h4("Step 5: Figures and Downloads"),
            p("All generated figures are automatically:"),
            tags$ul(
              tags$li("Displayed in the embedded HTML report on the Results page"),
              tags$li("Included when you download the HTML report"),
              tags$li("Saved in high quality (both static PNG and interactive HTML formats)")
            ),
            p("To save figures: Download the HTML report, which contains all figures embedded within it.")
          )
        ),

        fluidRow(
          box(
            title = "Supported Models (2025)",
            width = 12,
            solidHeader = TRUE,

            h4("Anthropic Claude"),
            tags$ul(
              tags$li(strong("claude-sonnet-4-5-20250929"), " - Best for complex analysis and coding (Recommended)"),
              tags$li(strong("claude-opus-4-1-20250805"), " - Most capable for complex reasoning"),
              tags$li(strong("claude-haiku-4-5-20251001"), " - Fast and cost-effective")
            ),

            h4("OpenAI GPT"),
            tags$ul(
              tags$li(strong("gpt-5"), " - Latest flagship model with advanced reasoning (Recommended)"),
              tags$li(strong("gpt-5-mini"), " - Balanced performance and cost"),
              tags$li(strong("gpt-5-nano"), " - Fast and economical"),
              tags$li(strong("gpt-4o"), " - Previous generation, still capable")
            ),

            h4("Google Gemini"),
            tags$ul(
              tags$li(strong("gemini-2.5-flash"), " - Fast with excellent performance (Recommended)"),
              tags$li(strong("gemini-2.5-pro"), " - Most advanced with 1M token context"),
              tags$li(strong("gemini-2.5-flash-lite"), " - Most cost-efficient option")
            )
          )
        ),

        fluidRow(
          box(
            title = "Figure Display and Download",
            width = 12,
            solidHeader = TRUE,

            h4("How Figures Work"),
            p("AutoStat generates professional visualizations for your analysis:"),

            tags$ul(
              tags$li(strong("Automatic Generation:"), " The AI determines what plots are most appropriate for your research question"),
              tags$li(strong("Multiple Formats:"), " Figures are saved as both static images (PNG) and interactive plots (HTML/Plotly) when possible"),
              tags$li(strong("Embedded Display:"), " All figures appear directly in the HTML report on the Results page"),
              tags$li(strong("High Quality:"), " Figures are publication-ready quality")
            ),

            h4("Saving Figures"),
            p("To save all figures along with your analysis:"),
            tags$ol(
              tags$li("Click the 'Download HTML Report' button on the Results page"),
              tags$li("Open the downloaded HTML file in any web browser"),
              tags$li("All figures are embedded and can be viewed offline"),
              tags$li("Right-click any figure to save it individually, or print the entire report to PDF")
            )
          )
        ),

        fluidRow(
          box(
            title = "Resources",
            width = 12,
            solidHeader = TRUE,

            p(
              HTML("<strong>GitHub Repository:</strong>
                   <a href='https://github.com/zhangxiany-tamu/AutoStat' target='_blank'>
                   github.com/zhangxiany-tamu/AutoStat</a>")
            ),

            p(
              HTML("<strong>Get API Keys:</strong><br>
                   Anthropic: <a href='https://console.anthropic.com/' target='_blank'>console.anthropic.com</a><br>
                   OpenAI: <a href='https://platform.openai.com/' target='_blank'>platform.openai.com</a><br>
                   Google: <a href='https://ai.google.dev/' target='_blank'>ai.google.dev</a>")
            )
          )
        )
      )
    )
  )
)
