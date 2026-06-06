#' An S4 class to hold AFM force-distance raw and processed curves with metadata
#'
#' @description
#' `fdObj` is the main container class used throughout the package to keep a set
#' of AFM curves and their derived results together in one validated object.
#' Rather than passing raw curves, transformed curves, calibration segments, and
#' metadata as separate objects, `fdObj` stores them in aligned slots so that
#' every sample keeps the same identity across import, transformation,
#' visualization, and analysis steps.
#'
#' In a typical workflow, an `fdObj` is first created from a folder of raw curve
#' files. At that stage, the object mainly contains imported raw curves and a
#' metadata table. Downstream functions then populate additional slots with
#' transformed approach and retract curves, curve regions used for sensitivity
#' and baseline estimation, and analytical results written back into the
#' metadata. This makes `fdObj` the central analysis object for the package and
#' allows most functions to take an `fdObj`, update its contents, and return the
#' modified object.
#'
#' The class also enforces consistency between its parts. Curve lists must stay
#' named consistently, transformed curves must have the expected structure, and
#' metadata rows must match the registered curve names. This validity checking
#' helps prevent analyses from being run on mismatched or incomplete datasets.
#'
#' @slot rawCurves A named list of data.frames containing raw AFM data. Each
#'   data.frame must include at least one complete distance-deflection pair:
#'   either approach columns \code{Calc_Ramp_Ex_nm} and \code{Defl_V_Ex},
#'   or retract columns \code{Calc_Ramp_Rt_nm} and \code{Defl_V_Rt}; both
#'   pairs are also allowed. Additional columns are allowed.
#' @slot approachCurves A named list of 2-column data.frames (distance vs force from approach segment)
#' @slot retractCurves A named list of 2-column data.frames (distance vs force from retract segment)
#' @slot metadata A data.frame with row names matching names(rawCurves). Stores metadata and calculated features.
#' @slot senscal_segment A list with two slots: approach and retract, each is a named list of data.frames, storing the part of the raw curve used for sensitivity calculation.
#' @slot baseline_segment A list with two slots: approach and retract, each is a named list of data.frames, storing the part of the raw curve used for baseline calculation
#' @importFrom methods new setClass setMethod show
setClass(
  "fdObj",
  slots = list(
    rawCurves = "list",
    approachCurves = "list",
    retractCurves = "list",
    metadata = "data.frame",
    senscal_segment = "list",
    baseline_segment = "list"
  ),
  validity = function(object) {
    check_named_list_of_dfs <- function(x, ref_names, coln = NULL) {
      if (!is.list(x) || any(!sapply(x, is.data.frame))) return(FALSE)
      if (!identical(names(x), ref_names)) return(FALSE)
      if (!is.null(coln) && any(sapply(x, ncol) != coln)) return(FALSE)
      TRUE
    }

    # --- rawCurves ---
    if (!check_named_list_of_dfs(object@rawCurves, names(object@rawCurves))) {
      return("rawCurves must be a named list of data.frames")
    }

    approach_cols <- c("Calc_Ramp_Ex_nm", "Defl_V_Ex")
    retract_cols <- c("Calc_Ramp_Rt_nm", "Defl_V_Rt")
    
    invalid_curves <- vapply(
      object@rawCurves,
      function(df) {
        has_approach <- all(approach_cols %in% colnames(df))
        has_retract <- all(retract_cols %in% colnames(df))
        !has_approach && !has_retract
      },
      logical(1)
    )
    if (any(invalid_curves)) {
      bad_names <- names(object@rawCurves)[invalid_curves]
      return(sprintf(
        "Each raw curve must include either approach columns (%s) or retract columns (%s), or both. Invalid: %s",
        paste(approach_cols, collapse = ", "),
        paste(retract_cols, collapse = ", "),
        paste(bad_names, collapse = ", ")
      ))
    }

    # --- approachCurves ---
    if (!check_named_list_of_dfs(object@approachCurves, names(object@rawCurves), 2)) {
      return("approachCurves must be a named list of 2-column data.frames with same names as rawCurves")
    }

    # --- retractCurves ---
    if (!check_named_list_of_dfs(object@retractCurves, names(object@rawCurves), 2)) {
      return("retractCurves must be a named list of 2-column data.frames with same names as rawCurves")
    }

    # --- metadata ---
    if (is.null(rownames(object@metadata)) ||
        !setequal(rownames(object@metadata), names(object@rawCurves))) {
      return("Row names of metadata must match names of rawCurves")
    }

    # --- senscal_segment and baseline_segment ---
    check_seg_structure <- function(seg) {
      is.list(seg) &&
        all(c("approach", "retract") %in% names(seg)) &&
        check_named_list_of_dfs(seg$approach, names(object@rawCurves)) &&
        check_named_list_of_dfs(seg$retract, names(object@rawCurves))
    }

    if (!check_seg_structure(object@senscal_segment)) {
      return("senscal_segment must be a list with 'approach' and 'retract', each a named list of data.frames")
    }

    if (!check_seg_structure(object@baseline_segment)) {
      return("baseline_segment must be a list with 'approach' and 'retract', each a named list of data.frames")
    }

    TRUE
  }
)

#' Display a Summary of an fdObj Object
#'
#' @description
#' The `show()` method provides a concise overview of an \code{fdObj} object.
#' It is automatically called when an fdObj is printed in the console (e.g., typing its name).
#' The summary includes the number of registered samples, imported raw curves,
#' successfully transformed approach/retract curves, and metadata column names.
#'
#' @param object An \code{fdObj} object.
#'
#' @details
#' This method is defined for the S4 class \code{fdObj} and is executed automatically
#' when the object is shown in the console. It provides a quick overview of data
#' completeness and analysis progress without modifying the object.
#'
#' @return
#' Prints a formatted summary of the object in the console.
#'
#' @examples
#' \dontrun{
#' folder <- system.file("extdata", package = "curvana")
#' test <- createFdObjFromFolder(folder)
#' test  # calling 'show()' implicitly
#' }
#'
#' @export
setMethod("show", "fdObj", function(object) {
  cat("An object of class \"fdObj\"\n")
  cat("----------------------------------------\n")

  # Metadata summary
  n_samples <- nrow(object@metadata)
  cat("Samples registered: ", n_samples, "\n")

  # Raw and transformed curves
  n_raw <- length(object@rawCurves)
  n_approach <- sum(sapply(object@approachCurves, function(x) {
    !is.null(x) && nrow(x) > 0
  }))

  n_retract <- sum(sapply(object@retractCurves, function(x) {
    !is.null(x) && nrow(x) > 0
  }))
  cat("Raw curves imported: ", n_raw, "\n")
  cat("Transformed approach curves: ", n_approach, "\n")
  cat("Transformed retract curves:  ", n_retract, "\n")

  # Metadata columns
  cat("\nMetadata columns:\n")
  cat(paste0("  - ", colnames(object@metadata), collapse = "\n"), "\n")
})

