#' Launch the Curvana Shiny app
#'
#' Starts the interactive Curvana Shiny application bundled with the package.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp()}.
#'
#' @return Invisibly returns the value from \code{shiny::runApp()}.
#' @examples 
#' run_curvana_app()
#' @export
run_curvana_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required. Please install it with install.packages('shiny').")
  }
  if (!requireNamespace("bs4Dash", quietly = TRUE)) {
    stop("Package 'bs4Dash' is required. Please install it with install.packages('bs4Dash').")
  }

  app_dir <- system.file("shiny-apps", "curvana-explorer", package = "curvana")
  if (identical(app_dir, "") || !dir.exists(app_dir)) {
    stop("Shiny app directory not found in the installed package.")
  }

  shiny::runApp(appDir = app_dir, ...)
}
