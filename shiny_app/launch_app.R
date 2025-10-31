#!/usr/bin/env Rscript

# AutoStat Shiny App Launcher
# Double-click this file or run: Rscript launch_app.R

cat("
╔═══════════════════════════════════════════╗
║     AutoStat - Automated Statistical     ║
║            Analysis Web App               ║
╚═══════════════════════════════════════════╝

Starting app...
")

# Change to the shiny_app directory
setwd(dirname(sys.frame(1)$ofile))

# Launch the app
shiny::runApp(launch.browser = TRUE)
