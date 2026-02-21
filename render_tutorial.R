# Script to render the curvana tutorial
library(rmarkdown)

# Set pandoc path to RStudio installation
pandoc_dir <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)

cat("Setting pandoc directory to:", pandoc_dir, "\n")
cat("Checking pandoc availability...\n")
cat("Pandoc available:", pandoc_available(), "\n")

if (pandoc_available()) {
  cat("Pandoc version:", as.character(pandoc_version()), "\n")
  cat("Rendering tutorial.Rmd...\n")
  
  # Render the tutorial
  render(
    input = "vignettes/tutorial.Rmd",
    output_format = html_document(
      toc = TRUE,
      toc_float = TRUE,
      theme = "readable",
      number_sections = FALSE,
      highlight = "tango"
    ),
    output_file = "tutorial.html",
    output_dir = "."
  )
  
  cat("Tutorial rendered successfully!\n")
  cat("Output file: tutorial.html\n")
} else {
  cat("ERROR: Pandoc not available. Please install pandoc.\n")
}