# Overview

**Curvana** is an R-based, open-source software for end-to-end analysis of AFM force-distance data. Curvana is available both as an R package and as an **online (and local) Shiny application**. Curvana provides a comprehensive suite of functions, supporting the full AFM force-distance analysis workflow, including curve transformation, core analytical metric calculation, publication-ready figure generation, and statistical analysis. 

It is built around an S4 object class called **`fdObj`**, which standardizes data import, storage,
extraction, and analysis. The `fdObj` structure organizes all raw and processed data into designated slots, ensuring data integrity, simplifying analysis workflows, and facilitating
reproducible data sharing.

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

## Supported File Formats

**curvana** supports importing AFM force-distance curves from three file formats:

### Bruker NanoScope/Veeco ASCII

Column-separated format where each displacement-deflection pair represents one curve segment.

- **Approach segment**: Uses displacement column `Calc_Ramp_Ex_nm` and deflection column `Defl_V_Ex` (reversed)
- **Retract segment**: Uses displacement column `Calc_Ramp_Rt_nm` (reversed) and deflection column `Defl_V_Rt`
- Files may contain approach only, retract only, or both segments
- Both `Defl_V_Ex` and `Calc_Ramp_Rt_nm` are automatically reversed so that both curves begin with the contact region
- Any rows starting with `#` or `"` are omitted

### JPK ASCII Format

JPK text export files contain approach (extend) and/or retract segments marked by segment headers.

- The function automatically detects segments and extracts sensitivity and spring constant values separately for each
- Files may contain approach only, retract only, or both segments
- Users are encouraged to import deflection data in Voltage units
- If force data is provided (indicated by 'N' unit), it will be converted back to Voltage using the associated sensitivity and spring constant values
- By default, the displacement and deflection columns of the approach segment are reversed so that both curves begin with the contact region

### Generic Column-Separated Format

Generic format for column-separated AFM data files where piezo displacement and cantilever deflection voltage are stored in separate columns.

- Files can contain approach segment only, retract segment only, or both segments
- Users must specify the column names for displacement and deflection of each segment
- Any rows starting with `#` or `"` are omitted

---

## Core Workflow

**curvana** accepts as input a directory of raw *deflection–displacement* curves, in the format listed above. All curves are automatically registered in the internal **metadata** slot, while
the corresponding raw data are stored in the **rawcurves** slot.

Once imported, users can apply curvana’s analysis functions to:

- **Transform** raw deflection–displacement curves into calibrated **force–distance** curves  
- **Quantify** key mechanical properties such as:
  - Adhesive force  
  - Adhesive and repulsive energies  
  - Rupture/adhesive and repulsive distances  

All computed results are automatically saved back into the `fdObj` metadata, maintaining a complete, self-contained record of the analysis.

The package also provides essential downstream analysis tools, including:

- **PCA biplots** for visualizing main trends of variation across experimental groups and identifying the features that drive them
- **Heatmaps** for exploring multivariate feature patterns and sample clustering
- **Violin plots** with integrated statistical testing for comparing distributions of curve-derived properties across conditions

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

`vignette_minimal_workflow.html` presents the shortest practical path through the package, focusing on the core steps needed to import data, transform curves, calculate analytical metrics, and generate the main result plots.

`vignette_full_workflow.html` provides a more complete walkthrough of the package, covering the same core pipeline in greater depth along with a single-curve example showing the analysis step-by-step.


