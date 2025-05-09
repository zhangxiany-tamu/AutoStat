# AutoStat

![autoStatR](https://img.shields.io/badge/R-AutoStat-blue)
![License](https://img.shields.io/badge/license-MIT-green)

**AutoStat** is an R package that harnesses the power of Large Language Models (LLMs) to automate statistical analysis workflows. It connects to various LLM providers (Claude, GPT, Gemini) to generate analysis plans, executable R code, and comprehensive reports based on your data and research questions.

## Features

- 🔄 **Multi-Provider Support**: Works with Anthropic (Claude), OpenAI (GPT), and Google (Gemini) models
- 📊 **Automated Analysis**: Generate complete R code for statistical analysis from natural language questions
- 📝 **Two-Step Approach**: First develops a comprehensive analysis plan, then produces executable code
- 📈 **Smart Visualizations**: Creates meaningful plots tailored to your research questions
- 📑 **Professional Reports**: Generates HTML reports with interpretations, visualizations, and code
- 🛠️ **Error Handling**: Robust error detection and resolution when executing generated code

## Installation

```r
# Install from GitHub
devtools::install_github("zhangxiany-tamu/autoStatR")
```

## Quick Start

```r
library(autoStatR)

# Basic usage
results <- auto_stat(
  data = mtcars,
  question = "What factors are most strongly associated with fuel efficiency (mpg)?",
  api_key = "your_api_key",                  # Your LLM provider API key
  llm_provider = "anthropic",                # Choose: "anthropic", "openai", or "gemini"
  llm_model = "claude-3-opus-20240229"       # Specify model version
)

# Open the generated report
browseURL(results$report_path)
```

## Example

```r
# Analyze the iris dataset using Gemini
results <- auto_stat(
  data = iris,
  question = "How well can we distinguish iris species based on sepal and petal measurements?",
  api_key = "your_gemini_api_key",
  llm_provider = "gemini",
  llm_model = "gemini-2.0-flash",
  output_dir = "iris_analysis"
)
```

## Supported LLM Providers

Provider | Example Model | Setup
---------|---------------|------
Anthropic | claude-3-opus-20240229 | Requires [Anthropic API key](https://console.anthropic.com/)
OpenAI | gpt-4 | Requires [OpenAI API key](https://platform.openai.com/)
Google | gemini-2.0-flash | Requires [Google AI Studio API key](https://ai.google.dev/)

## License

MIT License
