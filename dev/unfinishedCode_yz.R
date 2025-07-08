library('fdafmR')
library(dplyr)
library(ggplot2)
# test: creating fdOBJ
folder = system.file("extdata", package = "fdafmR")
test = createFdObjFromFolder(folder = folder)
test = createFdObjFromFolder(folder = folder,threads = 2)

# test: the extract function
test@metadata$date = sub(pattern = '_.*','',test@metadata$filename)
test@metadata$bacteria = sub(".*f?_(.*)_loc.*", "\\1", test@metadata$filename)

test.paeru = extract(test,by_col = list(bacteria = "paeru"))
test.paeru2 = extract(test,by_sample = rownames(test@metadata[test@metadata$bacteria == 'paeru',]))
test.pmon = extract(test,by_col = list(bacteria = 'pmon2'))

# test: combine
test.combine.1 = combineFdObj(test.paeru,test.pmon)
test.combine.1@metadata
test.combine.2 = combineFdObj(test.paeru,test.paeru2) # should return error

# test: deflection_plot
deflection_plot(test,split_curves_by = 'bacteria')
deflection_plot(test,split_curves_by = 'bacteria',group_curves_by = 'date',
                alpha = 0.5,point_size = 0.5,
                color_map = list(`180424` = 'darkred',
                                 `180425` = 'blue'))
deflection_plot(test,split_curves_by = 'date',group_curves_by = 'bacteria',
                alpha = 0.5,point_size = 0.5,
                color_map = list(`paeru` = 'darkred',
                                 `pmon2` = 'blue',
                                 sc = 'orange',
                                 mica = 'lightblue'))
