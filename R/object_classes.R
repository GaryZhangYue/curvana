#' An S4 class to hold AFM force-distance raw and processed curves with metadata
#'
#' @slot rawCurves A named list of data.frames, each with 4 columns (raw AFM data)
#' @slot approachCurves A named list of 2-column data.frames (distance vs force from approach segment)
#' @slot retractCurves A named list of 2-column data.frames (distance vs force from retract segment)
#' @slot metadata A data.frame with row names matching names(rawCurves). Stores metadata and calculated features.
setClass(
  "fdObj",
  slots = list(
    rawCurves = "list",
    approachCurves = "list",
    retractCurves = "list",
    metadata = "data.frame"
  ),
  validity = function(object) {
    # --- rawCurves ---
    if (!is.list(object@rawCurves) || any(!sapply(object@rawCurves, is.data.frame))) {
      return("rawCurves must be a list of data.frames")
    }
    if (any(sapply(object@rawCurves, ncol) != 4)) {
      return("Each data.frame in rawCurves must have exactly 4 columns")
    }
    if (is.null(names(object@rawCurves)) || any(names(object@rawCurves) == "")) {
      return("rawCurves must be a *named* list")
    }

    # --- approachCurves ---
    if (!is.list(object@approachCurves) || any(!sapply(object@approachCurves, is.data.frame))) {
      return("approachCurves must be a list of data.frames")
    }
    if (!identical(names(object@rawCurves), names(object@approachCurves))) {
      return("approachCurves must have the same names as rawCurves")
    }
    if (any(sapply(object@approachCurves, ncol) != 2)) {
      return("Each data.frame in approachCurves must have exactly 2 columns")
    }

    # --- retractCurves ---
    if (!is.list(object@retractCurves) || any(!sapply(object@retractCurves, is.data.frame))) {
      return("retractCurves must be a list of data.frames")
    }
    if (!identical(names(object@rawCurves), names(object@retractCurves))) {
      return("retractCurves must have the same names as rawCurves")
    }
    if (any(sapply(object@retractCurves, ncol) != 2)) {
      return("Each data.frame in retractCurves must have exactly 2 columns")
    }

    # --- metadata ---
    if (is.null(rownames(object@metadata))) {
      return("metadata must have row names")
    }
    if (!setequal(rownames(object@metadata), names(object@rawCurves))) {
      return("Row names of metadata must match the names of rawCurves")
    }

    TRUE
  }
)
