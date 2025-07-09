#' An S4 class to hold AFM force-distance raw and processed curves with metadata
#'
#' @slot rawCurves A named list of data.frames, each with 4 columns (raw AFM data)
#' @slot approachCurves A named list of 2-column data.frames (distance vs force from approach segment)
#' @slot retractCurves A named list of 2-column data.frames (distance vs force from retract segment)
#' @slot metadata A data.frame with row names matching names(rawCurves). Stores metadata and calculated features.
#' @slot senscal_segment A list with two slots: approach and retract, each is a named list of data.frames, storing the part of the raw curve used for sensitivity calculation.
#' @slot baseline_segment A list with two slots: approach and retract, each is a named list of data.frames, storing the part of the raw curve used for baseline calculation
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
    if (!check_named_list_of_dfs(object@rawCurves, names(object@rawCurves), 4)) {
      return("rawCurves must be a named list of 4-column data.frames")
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
