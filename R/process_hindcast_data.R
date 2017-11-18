# process_hindcast_data.R

#' get_hindcast_AgProdChange
#'
#' @details Read in FAO production & ha by country and crop,
#'          Aggregate to GCAM regions and commodities,
#'          Compute & return AgProdChange in historical period
#' @return AgProdChange in historical period
#' @import readr
#' @importFrom tidyr gather
#' @importFrom dplyr mutate select left_join filter
#' @author KVC October 2017
#' @export
get_hindcast_AgProdChange <- function(){
  # Silence package checks
  prod <- ha <- year <- value <- yield.x <- yield.y <- iso <- GCAM_region_ID <- region <-
    GCAM_commodity <- FAO_country <- item <- na.omit <- prev_year <- yield <-
    AgProdChange <- NULL

  # Read in mappings
  agluCtry <- suppressMessages(read_csv("./inst/extdata/mappings/AGLU_ctry.csv", skip = 3))
  iso_GCAM_regID <- suppressMessages(read_csv("./inst/extdata/mappings/iso_GCAM_regID.csv", skip = 3))
  GCAM_region_names <- suppressMessages(read_csv("./inst/extdata/mappings/GCAM_region_names.csv", skip = 3))
  FAO_ag_items_PRODSTAT <- suppressMessages(read_csv("./inst/extdata/mappings/FAO_ag_items_PRODSTAT.csv", skip = 3))

  # Read production & harvested area (we'll calculate yield from this so we can aggregate)
  faoHA <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_ha.csv",
                                     col_types = cols(.default = "c")))
  faoProd <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_prod.csv"))

  # Tidy data
  faoHA %>%
    gather(year, ha, -FAO_country, -item) %>%
    mutate(year = as.integer(year), ha = as.integer(ha)) ->
    faoHA

  faoProd %>%
    gather(year, prod, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
    faoProd

  # Join data and compute average yield
  faoHA %>%
    filter(year %in% YEARS) %>%
    left_join(faoProd, by=c("FAO_country", "item", "year")) %>%
    left_join(select(agluCtry, FAO_country, iso), by="FAO_country") %>%
    left_join(select(iso_GCAM_regID, iso, GCAM_region_ID), by="iso") %>%
    left_join(GCAM_region_names, by="GCAM_region_ID") %>%
    left_join(select(FAO_ag_items_PRODSTAT, item, GCAM_commodity), by="item") %>%
    na.omit() %>%
    group_by(region, GCAM_commodity, year) %>%
    summarize(ha = sum(ha), prod = sum(prod)) %>%
    mutate(yield = prod / ha) %>%
    select(region, GCAM_commodity, year, yield) ->
    faoYield

  # Print yields
  write_csv(faoYield, "./outputs/hindcast_yield.csv")

  # Compute AgProdChange
  faoYield %>%
    mutate(prev_year = year - 1) %>%
    filter(prev_year %in% YEARS) %>%
    left_join(faoYield, by=c("region", "GCAM_commodity", "prev_year" = "year")) %>%
    mutate(AgProdChange = (yield.x / yield.y) - 1) %>%
    select(region, GCAM_commodity, year, AgProdChange) ->
    faoAgProdChange

  return(faoAgProdChange)
}

