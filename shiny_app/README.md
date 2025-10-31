# AutoStat Shiny App

A professional web application for automated statistical analysis using Large Language Models.

## Features

- **Multi-Provider LLM Support**: Works with Anthropic Claude, OpenAI GPT, and Google Gemini
- **Automated Analysis**: Generate statistical analysis plans and R code from natural language questions
- **Interactive Visualizations**: View and explore results with interactive plots
- **Professional Reports**: Download comprehensive HTML reports
- **Clean, Modern UI**: Professional dashboard design with intuitive navigation

## Local Development

1. Install required R packages:
```r
install.packages(c("shiny", "shinydashboard", "DT", "plotly", "htmltools"))
```

2. Install AutoStat package:
```r
devtools::install_github("zhangxiany-tamu/AutoStat")
```

3. Run the app:
```r
shiny::runApp("shiny_app")
```

## Deployment

This app is designed to be deployed on [shinyapps.io](https://www.shinyapps.io/).

### Deploy to shinyapps.io:

```r
library(rsconnect)

# Set up your account (first time only)
rsconnect::setAccountInfo(name='YOUR_ACCOUNT',
                         token='YOUR_TOKEN',
                         secret='YOUR_SECRET')

# Deploy the app
rsconnect::deployApp('shiny_app', appName = 'autostat')
```

## Usage

1. **Upload Data**: Choose a sample dataset or upload your own CSV file
2. **Configure**: Enter your research question and API credentials
3. **Analyze**: Run the automated statistical analysis
4. **Results**: View visualizations, statistical outputs, and interpretations
5. **Download**: Get a complete HTML report

## API Keys

Get your API keys from:
- **Anthropic**: https://console.anthropic.com/
- **OpenAI**: https://platform.openai.com/
- **Google**: https://ai.google.dev/

## License

MIT License
