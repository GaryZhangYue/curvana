# Overview

**curvana** is an R package that provides a comprehensive framework for analyzing
atomic force microscopy (AFM) force–distance curves. It is built around an S4
object class called **`fdObj`**, which standardizes data import, storage,
extraction, and analysis.

The `fdObj` structure organizes all raw and processed data into designated slots,
ensuring data integrity, simplifying analysis workflows, and facilitating
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

## Visualization and Reporting

**curvana** includes flexible plotting utilities for generating publication-ready figures:

- **Scatter plots** of individual or batch-wise raw and processed curves  
- **Violin plots** summarizing distributions of curve-derived properties  

These tools make it easy to explore data quality, compare experimental conditions,
and visualize quantitative trends across samples.

---

## Integration with shinyCurvana

**curvana** seamlessly integrates with **shinyCurvana**, an R Shiny application that
provides a **code-free, user-friendly interface** for performing the same analyses
interactively.

Together, they enable both scripting-based and graphical workflows for AFM
force–distance data processing and interpretation.

