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

  # Subset rawCurves
  raw_subset <- fdObj@rawCurves[samples_to_keep]

  # Subset approachCurves only if not empty
  if (length(fdObj@approachCurves) > 0) {
    approach_subset <- fdObj@approachCurves[samples_to_keep]
  } else {
    approach_subset <- list()
  }

  # Subset retractCurves only if not empty
  if (length(fdObj@retractCurves) > 0) {
    retract_subset <- fdObj@retractCurves[samples_to_keep]
  } else {
    retract_subset <- list()
  }

  # Subset metadata
  metadata_subset <- fdObj@metadata[samples_to_keep, , drop = FALSE]

  # Return new object
  new("fdObj",
      rawCurves = raw_subset,
      approachCurves = approach_subset,
      retractCurves = retract_subset,
      metadata = metadata_subset)
}
