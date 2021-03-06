## Generate the FAO_land_history data set.

library('readr')
library('dplyr')
library('devtools')
FAO_land_history <- read_csv('data-raw/FAO_Land.csv')
CCI_land_history <- read_csv('data-raw/FAO_CCI_LandCover.csv')

CCI_land_history %>%
  dplyr:::rename(GCAM_commodity = Land_Type) %>%
  bind_rows(FAO_land_history) ->
  Land_history

use_data(Land_history, compress='xz', overwrite=TRUE)
