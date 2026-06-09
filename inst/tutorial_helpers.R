# Tutorial helper functions

view_df <- function(df, caption) {
  if (requireNamespace("DT", quietly = TRUE)) {
    return(DT::datatable(
      df,
      caption = caption,
      extensions = "Buttons",
      options = list(
        dom = "Blfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
      )
    ))
  }

  if (!missing(caption) && nzchar(caption)) {
    message(caption)
  }

  utils::head(df)
}
