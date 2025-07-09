#' Extract subset of fdObj by sample or metadata column
#'
#' @param fdObj An object of class \code{fdObj}
#' @param by_sample A character vector of sample names to keep
#' @param by_col A named list like list(colname = value) to filter metadata (e.g., list(type = "control"))
#'
#' @return A subsetted \code{fdObj} object
#' @export
extract <- function(fdObj, by_sample = NULL, by_col = NULL) {
  if (!inherits(fdObj, "fdObj")) {
    stop("fdObj must be of class 'fdObj'")
  }

  if (is.null(by_sample) && is.null(by_col)) {
    stop("You must provide either by_sample or by_col.")
  }
  if (!is.null(by_sample) && !is.null(by_col)) {
    stop("Please provide only one of by_sample or by_col.")
  }

  # Determine which samples to keep
  if (!is.null(by_sample)) {
    samples_to_keep <- intersect(by_sample, rownames(fdObj@metadata))
  } else {
    if (!is.list(by_col) || length(by_col) != 1) {
      stop("by_col must be a named list of one element, like list(condition = 'treated')")
    }
    colname <- names(by_col)[1]
    value <- by_col[[1]]
    if (!colname %in% colnames(fdObj@metadata)) {
      stop(paste("Column", colname, "not found in metadata"))
    }
    samples_to_keep <- rownames(fdObj@metadata[fdObj@metadata[[colname]] %in% value, , drop = FALSE])
  }

  if (length(samples_to_keep) == 0) {
    stop("No matching samples found.")
  }

  # Subset
  metadata_subset <- fdObj@metadata[samples_to_keep, , drop = FALSE]
  raw_subset <- fdObj@rawCurves[samples_to_keep]
  approach_subset <- fdObj@approachCurves[samples_to_keep]
  retract_subset <- fdObj@retractCurves[samples_to_keep]
  senscal_approach_subset <- fdObj@senscal_segment$approach[samples_to_keep]
  senscal_retract_subset <- fdObj@senscal_segment$retract[samples_to_keep]
  baseline_approach_subset <- fdObj@baseline_segment$approach[samples_to_keep]
  baseline_retract_subset  <- fdObj@baseline_segment$retract[samples_to_keep]

  # Combine senscal_segment and baseline_segment
  senscal_combined <- list(
    approach = senscal_approach_subset,
    retract  = senscal_retract_subset
  )
  baseline_combined <- list(
    approach = baseline_approach_subset,
    retract  = baseline_retract_subset
  )
  # Return new object
  new("fdObj",
      rawCurves = raw_subset,
      approachCurves = approach_subset,
      retractCurves = retract_subset,
      metadata = metadata_subset,
      senscal_segment = senscal_combined,
      baseline_segment = baseline_combined
      )
}

#' Combine two fdObj objects
#'
#' @param x A `fdObj` object
#' @param y A `fdObj` object. This object cannot have any overlapping sample names with the first fdObj.
#'
#' @return A new combined `fdObj` object. metadata matrix will be concatenated using bind_rows (dplyr). All slots will be concatenated.
#' @export

combineFdObj <- function(x, y) {
  stopifnot(inherits(x, "fdObj"), inherits(y, "fdObj"))

  # Check for overlapping sample names
  name_overlap <- intersect(names(x@rawCurves), names(y@rawCurves))
  if (length(name_overlap) > 0) {
    stop("Overlapping sample names in rawCurves: ", paste(name_overlap, collapse = ", "))
  }

  # Combine curve slots
  raw_combined      <- c(x@rawCurves,      y@rawCurves)
  approach_combined <- c(x@approachCurves, y@approachCurves)
  retract_combined  <- c(x@retractCurves,  y@retractCurves)

  # Combine senscal_segment and baseline_segment
  senscal_combined <- list(
    approach = c(x@senscal_segment$approach, y@senscal_segment$approach),
    retract  = c(x@senscal_segment$retract,  y@senscal_segment$retract)
  )
  baseline_combined <- list(
    approach = c(x@baseline_segment$approach, y@baseline_segment$approach),
    retract  = c(x@baseline_segment$retract,  y@baseline_segment$retract)
  )

  # Combine metadata (preserve rownames, handle missing columns)
  x_meta <- tibble::rownames_to_column(x@metadata, var = ".sample")
  y_meta <- tibble::rownames_to_column(y@metadata, var = ".sample")
  metadata_combined <- dplyr::bind_rows(x_meta, y_meta)
  metadata_combined <- tibble::column_to_rownames(metadata_combined, var = ".sample")

  # Construct and return new fdObj
  new("fdObj",
      rawCurves = raw_combined,
      approachCurves = approach_combined,
      retractCurves = retract_combined,
      metadata = metadata_combined,
      senscal_segment = senscal_combined,
      baseline_segment = baseline_combined)
}
