#' Calculate Sensitivity from a Segment of AFM Data
#'
#' This function identifies a linear segment from AFM deflection vs. piezo extension data and calculates
#' the sensitivity (slope) using linear regression. It iteratively adds chunks of data to build a segment
#' with high linearity (\eqn{R^2 > 0.999}) and optionally removes outliers to maintain this criterion.
#'
#' @param end Integer. The maximum index to consider in the `x` and `y` vectors (i.e., up to which point to search).
#' @param intv Integer. The chunk size to use when iteratively selecting data segments.
#' @param x Numeric vector. The piezo extension values (e.g., distance in nm).
#' @param y Numeric vector. The deflection values (e.g., voltage).
#'
#' @return A list of three elements:
#' \describe{
#'   \item{sens_x}{A numeric vector of x values used for sensitivity fitting.}
#'   \item{sens_y}{A numeric vector of y values used for sensitivity fitting.}
#'   \item{sensitivity}{The calculated sensitivity (slope) as a numeric value, or \code{NULL} if not computable.}
#' }
#'
#' @examples
#' x <- seq(0, 100, by = 1)
#' y <- 0.02 * x + rnorm(length(x), sd = 0.01)
#' calc_sensitivity(end = 80, intv = 10, x = x, y = y)
#'
#' @export
calc_sensitivity <- function(end, intv, x, y) {
  sens_x <- numeric()
  sens_y <- numeric()

  i <- 1
  while ((i + intv - 1) <= end) {
    idx <- i:(i + intv - 1)
    x_seg <- x[idx]
    y_seg <- y[idx]

    combined_x <- c(sens_x, x_seg)
    combined_y <- c(sens_y, y_seg)

    r <- suppressWarnings(cor(combined_x, combined_y, use = "complete.obs"))

    if (!is.na(r) && r^2 > 0.999) {
      sens_x <- combined_x
      sens_y <- combined_y
    } else {
      for (j in intv:1) {
        temp_x <- x_seg
        temp_y <- y_seg

        temp_x <- temp_x[-j]
        temp_y <- temp_y[-j]

        r_try <- suppressWarnings(cor(c(sens_x, temp_x), c(sens_y, temp_y), use = "complete.obs"))

        if (!is.na(r_try) && r_try^2 > 0.999) {
          sens_x <- c(sens_x, temp_x)
          sens_y <- c(sens_y, temp_y)
          break
        }
      }
    }

    i <- i + intv
  }

  if (length(sens_x) == 0 || length(sens_y) == 0) {
    return(list(NULL, NULL, NULL))
  } else {
    fit <- lm(sens_y ~ sens_x)
    sensitivity <- -coef(fit)[2]
    return(list(sens_x, sens_y, sensitivity))
  }
}
