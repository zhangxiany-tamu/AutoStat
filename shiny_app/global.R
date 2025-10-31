# Global configuration for AutoStat Shiny App
# This file loads libraries and defines shared resources

# Load required packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(htmltools)

# Install and load AutoStat if available
if (!require("AutoStat", quietly = TRUE)) {
  if (file.exists("../R")) {
    # Development mode - source files directly
    source("../R/api_utils.R")
    source("../R/data_utils.R")
    source("../R/main_functions.R")
    source("../R/execute.R")
    source("../R/plot_utils.R")
    source("../R/report_utils.R")
    source("../R/retry_utils.R")
  } else {
    stop("AutoStat package not found. Please install it first.")
  }
}

# Sample datasets for demonstration
sample_datasets <- list(
  "mtcars" = mtcars,
  "iris" = iris,
  "airquality" = airquality,
  "ToothGrowth" = ToothGrowth
)

# Sample research questions
sample_questions <- list(
  "mtcars" = "What factors are most strongly associated with fuel efficiency (mpg)?",
  "iris" = "Can we distinguish iris species based on petal and sepal measurements?",
  "airquality" = "How do temperature and wind speed affect ozone levels?",
  "ToothGrowth" = "How does vitamin C dose and delivery method affect tooth growth?"
)

# LLM provider configurations
provider_models <- list(
  "anthropic" = c(
    "claude-sonnet-4-5-20250929" = "Claude Sonnet 4.5 (Recommended)",
    "claude-haiku-4-5-20251001" = "Claude Haiku 4.5"
  ),
  "openai" = c(
    "gpt-5" = "GPT-5 (Recommended)",
    "gpt-5-mini" = "GPT-5 Mini",
    "gpt-5-nano" = "GPT-5 Nano",
    "gpt-4o" = "GPT-4o",
    "gpt-4o-mini" = "GPT-4o Mini",
    "gpt-4-turbo" = "GPT-4 Turbo",
    "gpt-4" = "GPT-4"
  ),
  "gemini" = c(
    "gemini-2.5-flash" = "Gemini 2.5 Flash (Recommended)",
    "gemini-2.5-pro" = "Gemini 2.5 Pro",
    "gemini-2.5-flash-lite" = "Gemini 2.5 Flash Lite"
  )
)

# Maximum file upload size (50MB)
options(shiny.maxRequestSize = 50 * 1024^2)
