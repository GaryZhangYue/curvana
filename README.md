# Overview

**curvana** is an R package that provides a comprehensive framework for analyzing
atomic force microscopy (AFM) force–distance curves. It is built around an S4
object class called **`fdObj`**, which standardizes data import, storage,
extraction, and analysis.

The `fdObj` structure organizes all raw and processed data into designated slots,
ensuring data integrity, simplifying analysis workflows, and facilitating
reproducible data sharing.

The package supports both script-based workflows in R and an interactive Shiny
application for users who prefer a graphical interface.

---

## Installation

You can install **curvana** directly from GitHub using the **devtools** package:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install curvana from GitHub
devtools::install_github("GaryZhangYue/curvana", dependencies = TRUE)

# Load the package
library(curvana)
```

---

## Core Workflow

**curvana** accepts as input a directory of raw *deflection–displacement* curves.
All curves are automatically registered in the internal **metadata** slot, while
the corresponding raw data are stored in the **rawcurves** slot.

Once imported, users can apply curvana’s analysis functions to:

- **Transform** raw deflection–displacement curves into calibrated **force–distance** curves  
- **Quantify** key mechanical properties such as:
  - Adhesive force  
  - Adhesive and repulsive energies  
  - Rupture and repulsive distances  

All computed results are automatically saved back into the `fdObj` metadata,
maintaining a complete, self-contained record of the analysis.

---

## Main Utilities

The package provides utilities for the full AFM force-curve workflow:

- **Data import and object construction**: `createFdObjFromFolder()` creates an
  `fdObj` from a folder of AFM text files.
- **Curve transformation**: `transform_curves()` converts raw deflection curves
  into calibrated force-distance curves and stores the processed outputs in the
  `fdObj` object.
- **Analytical metrics**: `analyze_curves_all_analytical_metrics()` calculates
  adhesive force, adhesive and repulsive energies, rupture distance, repulsive
  distance, and related metrics.
- **Curve-level visualization**: `plot_deflection_curves()`, `plot_fd_curves()`,
  and `plot_a_curve_metrics()` support quality control, single-curve inspection,
  and annotated metric visualization.
- **Summary visualization**: `plot_metric_violin()`, `plot_pca_biplot()`,
  `plot_complex_heatmap()`, and `plot_raw_deflection_heatmap()` help compare
  groups and explore multivariate patterns across samples.

---

## Visualization and Reporting

**curvana** includes flexible plotting utilities for generating publication-ready figures:

- **Single-curve plots** for detailed inspection of transformed curves and annotated metrics  
- **Batch-curve plots** for visualizing raw or processed curves across many samples  
- **PCA biplots** for visualizing main trend of variation across groups and the feature driving it
- **Violin plots** for summarizing distributions of curve-derived properties and performing statistical tests
- **Heatmaps** for exploring raw deflection patterns and multivariate feature trends  

These tools make it easy to explore data quality, compare experimental conditions,
and visualize quantitative trends across samples.

---

## Shiny App

**curvana** includes an interactive Shiny application for code-free exploration
and analysis of AFM force-distance datasets.

Main app capabilities include:

- importing AFM datasets and metadata
- transforming curves and calculating analytical metrics
- inspecting annotated single-curve plots
- generating violin plots, PCA biplots, and heatmaps
- downloading processed results for downstream analysis

You can launch the app from R with:

```r
library(curvana)
run_curvana_app()
```

The packaged app entry points are:

- `R/shiny_app.R`
- `inst/shiny-apps/curvana-explorer/app.R`

---

## Tutorials

The repository includes the tutorials and rendered walkthroughs below:

- `vignettes/vignette_minimal_workflow.html`
- `vignettes/vignette_full_workflow.html`
- `vignettes/tutorial.html`

`vignette_minimal_workflow.html` presents the shortest practical path through
the package, focusing on the core steps needed to import data, transform curves,
calculate analytical metrics, and generate the main result plots.

`vignette_full_workflow.html` provides a more complete walkthrough of the
package, covering the same core pipeline in greater depth along with more
extensive visualization, comparison, and exploratory analysis examples.


