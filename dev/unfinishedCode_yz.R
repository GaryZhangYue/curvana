library('fdafmR')

library(dplyr)
test = createFdObjFromFolder(folder = '/Users/zhangy68/Desktop/test_data/sample')

x = read.table('/Users/zhangy68/Desktop/test_data/sample/180424_fv_paeru_loc1-9-0.spm.txt', header = TRUE, sep = "\t", stringsAsFactors = FALSE)
