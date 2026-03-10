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

message("[1/3] Installing dependencies...")
devtools::install_deps(dependencies = TRUE)

message("[2/3] Generating documentation...")
devtools::document()

message("[3/3] Running checks...")
devtools::check()
# or 
devtools::check(vignettes = FALSE)  

# remove the current installation of curvana if you want to test the installation process
remove.packages('curvana')
