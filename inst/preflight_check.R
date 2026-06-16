if (!requireNamespace("devtools", quietly = TRUE)) {
	stop("Package 'devtools' is required. Install it with install.packages('devtools').")
}

if (!file.exists("DESCRIPTION")) {
	if (file.exists(file.path("..", "DESCRIPTION"))) {
		setwd("..")
	} else {
		stop("Cannot find package root (DESCRIPTION). Run this script from the package root or the dev/ folder.")
	}
}

message("[1/4] Installing dependencies...")
devtools::install_deps(dependencies = TRUE)

message("[2/3] Generating documentation...")
devtools::document()

message("[3/4] Building package manual (PDF)...")
if (nzchar(Sys.which("pdflatex"))) {
	manual_path <- devtools::build_manual(path = "doc")
	message("Manual written to: ", manual_path)
} else {
	warning("Skipping manual build: 'pdflatex' not found. Install TinyTeX or a TeX distribution.")
}

message("[4/4] Running checks...")
devtools::check()
# or 
devtools::check(vignettes = FALSE)  
 
# remove the current installation of curvana if you want to test the installation process
remove.packages('curvana')

