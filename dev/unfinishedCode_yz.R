library('fdafmR')
library(dplyr)

# test of creating fdOBJ
test = createFdObjFromFolder(folder = '/Users/zhangy68/Desktop/test_data/sample')

x = read.table('/Users/zhangy68/Desktop/test_data/sample/180424_fv_paeru_loc1-9-0.spm.txt', header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# test of the extract function
test@metadata$date = sub(pattern = '_.*','',test@metadata$filename)
test@metadata$bacteria = sub(".*fv_(.*)_loc.*", "\\1", test@metadata$filename)

test.paeru = extract(test,by_col = list(bacteria = "paeru"))

test.paeru2 = extract(test,by_sample = rownames(test@metadata[test@metadata$bacteria == 'paeru',]))

