#' Historical land use data
#'
#' This dataset contains harvested area for crops by GCAM commodity from 1961-2020 and
#' land area for land cover types from 1970-2010 by GCAM region
#'
#' @format A data frame with 24313 rows and 4 variables:
#' \describe{
#' \item{region}{GCAM region name.}
#' \item{GCAM_commodity}{GCAM commodity name.}
#' \item{year}{Year of the observation.}
#' \item{area}{Land area for that commodity for the region and year.}
#' }
#' @source Cropland area is from FAO, aggregated to GCAM commodity and region. Non-crop area is from SAGE, processed by Moirai and then aggregated to GCAM type and region.
"Land_history"
