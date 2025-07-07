library('fdafmR')
library(dplyr)

# test of creating fdOBJ
folder = system.file("extdata", package = "fdafmR")
test = createFdObjFromFolder(folder = folder)
test = createFdObjFromFolder(folder = folder,threads = 2)

# test of the extract function
test@metadata$date = sub(pattern = '_.*','',test@metadata$filename)
test@metadata$bacteria = sub(".*fv_(.*)_loc.*", "\\1", test@metadata$filename)

test.paeru = extract(test,by_col = list(bacteria = "paeru"))
test.paeru2 = extract(test,by_sample = rownames(test@metadata[test@metadata$bacteria == 'paeru',]))
test.pmon = extract(test,by_col = list(bacteria = 'pmon2'))

# test of combine
test.combine.1 = combineFdObj(test.paeru,test.pmon)
test.combine.1@metadata
test.combine.2 = combineFdObj(test.paeru,test.paeru2) # should return error
