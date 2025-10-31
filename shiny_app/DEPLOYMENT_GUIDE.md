# AutoStat Shiny App - Deployment Guide

## Local Testing

### Prerequisites
Ensure you have R and the following packages installed:

```r
install.packages(c("shiny", "shinydashboard", "DT", "plotly", "htmltools"))
```

### Run Locally

From the AutoStat directory:

```r
shiny::runApp("shiny_app")
```

Or from within the shiny_app directory:

```r
shiny::runApp()
```

The app should open in your default web browser at `http://127.0.0.1:XXXX`

## Deployment to shinyapps.io

### Step 1: Set Up Account

1. Go to https://www.shinyapps.io/ and create an account
2. Navigate to your dashboard: https://www.shinyapps.io/admin/#/dashboard
3. Go to Account > Tokens
4. Click "Show" to reveal your token and secret

### Step 2: Configure rsconnect

In R, install and configure rsconnect:

```r
install.packages("rsconnect")

rsconnect::setAccountInfo(
  name = "YOUR_ACCOUNT_NAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
```

### Step 3: Deploy the App

From the AutoStat directory:

```r
# Deploy with a specific app name
rsconnect::deployApp(
  appDir = "shiny_app",
  appName = "autostat",
  appTitle = "AutoStat - Automated Statistical Analysis"
)
```

The deployment process will:
- Upload all necessary files
- Install required R packages
- Make your app available at: `https://YOUR_ACCOUNT.shinyapps.io/autostat`

### Step 4: Configure App Settings (Optional)

On shinyapps.io dashboard:

1. **Instance Size**: Start with the free tier (1 GB RAM)
2. **Instance Idle Timeout**: 15 minutes (free tier)
3. **Max Worker Processes**: 1 (free tier)
4. **Max Connections**: Adjust based on expected usage

### Troubleshooting

**Package Installation Errors:**
- Ensure all dependencies are listed in the app
- Check shinyapps.io logs for specific errors

**App Timeout:**
- LLM API calls can take time
- Consider upgrading to a paid tier for longer timeouts
- Or use async processing for long-running tasks

**Memory Issues:**
- Large datasets may require more memory
- Consider data size limits
- Upgrade instance if needed

## Updating the Deployed App

To update your deployed app:

```r
rsconnect::deployApp(
  appDir = "shiny_app",
  appName = "autostat",
  forceUpdate = TRUE
)
```

## App Structure

```
shiny_app/
├── ui.R                 # User interface definition
├── server.R             # Server logic and reactivity
├── global.R             # Global configuration and shared resources
├── DESCRIPTION          # App metadata for deployment
├── .rscignore           # Files to exclude from deployment
├── README.md            # Documentation
└── www/
    └── custom.css       # Custom styling
```

## Security Notes

- API keys are entered by users and never stored
- Use HTTPS (automatic on shinyapps.io)
- Consider adding usage limits if deployed publicly
- Monitor API costs on your LLM provider dashboards

## Performance Tips

1. **Caching**: Consider caching common analyses
2. **Data Size**: Limit uploaded file sizes (currently 50MB)
3. **Timeout Handling**: Implement proper error handling for slow API calls
4. **Progress Indicators**: Keep users informed during long operations

## Support

For issues with:
- **Shiny App**: Check the app logs on shinyapps.io
- **AutoStat Package**: https://github.com/zhangxiany-tamu/AutoStat
- **shinyapps.io**: https://docs.posit.co/shinyapps.io/

## License

MIT License
