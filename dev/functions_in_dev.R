# integration of area by interpolating the x intersept
calc_signed_areas <- function(x, y) {
  stopifnot(length(x) == length(y))

  area_above <- 0
  area_below <- 0

  for (i in seq_len(length(x) - 1)) {
    x1 <- x[i];   y1 <- y[i]
    x2 <- x[i+1]; y2 <- y[i+1]

    if (y1 >= 0 && y2 >= 0) {
      # Entirely above x-axis
      area_above <- area_above + 0.5 * (x2 - x1) * (y1 + y2)
    } else if (y1 <= 0 && y2 <= 0) {
      # Entirely below x-axis
      area_below <- area_below + 0.5 * (x2 - x1) * abs(y1 + y2)
    } else {
      # Sign change → interpolate x-intercept
      x0 <- x1 + (0 - y1) * (x2 - x1) / (y2 - y1)  # linear interpolation

      if (y1 > 0) {
        # Triangle above x-axis
        area_above <- area_above + 0.5 * (x0 - x1) * (y1 + 0)
        # Triangle below x-axis
        area_below <- area_below + 0.5 * (x2 - x0) * abs(0 + y2)
      } else {
        # Triangle below x-axis
        area_below <- area_below + 0.5 * (x0 - x1) * abs(y1 + 0)
        # Triangle above x-axis
        area_above <- area_above + 0.5 * (x2 - x0) * (0 + y2)
      }
    }
  }

  list(
    area_above_x_axis = area_above,
    area_below_x_axis = area_below
  )
}
