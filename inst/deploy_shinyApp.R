
# Copy secret from shinyapps.io here


# deloy app to shinyapps.io
rsconnect::deployApp(
  appDir = "inst/shiny-apps/curvana-explorer",
  appName = "curvana"
)
