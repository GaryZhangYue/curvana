# Tutorial helper functions
library(DT)

view_df <- function(df,caption){
  datatable(df,
            caption = caption,
            extensions = 'Buttons',
            options = list(dom = 'Blfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           lengthMenu = list(c(10,25,50,-1),
                                             c(10,25,50,"All"))))
}
