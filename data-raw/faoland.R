## Generate the FAO_land_history data set.

library('readr')
library('devtools')
FAO_land_history <- read_csv('data-raw/FAO_Land.csv')

use_data(FAO_land_history, compress='xz', overwrite=TRUE)
