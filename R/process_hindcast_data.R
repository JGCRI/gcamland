# process_hindcast_data.R

#' get_hindcast_AgProdChange
#'
#' @details Read in FAO production & ha by country and crop,
#'          Aggregate to GCAM regions and commodities,
#'          Compute & return AgProdChange in historical period
#' @return AgProdChange in historical period
#' @importFrom readr read_csv write_csv
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
  faoHA <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_ha.csv"))
  faoProd <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_prod.csv"))

  # Tidy data
  faoHA %>%
    gather(year, ha, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
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

#' get_hindcast_prices
#'
#' @details Read in FAO prices by country and crop,
#'          Aggregate to GCAM regions and commodities,
#' @return prices in historical period
#' @import dplyr
#' @importFrom readr read_csv
#' @importFrom tidyr gather
#' @author KVC October 2017
#' @export
get_hindcast_prices <- function(){
  # Silence package checks
  prod <- price <- year <- value <- price_fy <- uniqueJoinField <- na.omit <-
    FAO_country <- GCAM_commodity <- item <- deflator <- NULL

  # Read in mappings
  agluCtry <- suppressMessages(read_csv("./inst/extdata/mappings/AGLU_ctry.csv", skip = 3))
  iso_GCAM_regID <- suppressMessages(read_csv("./inst/extdata/mappings/iso_GCAM_regID.csv", skip = 3))
  GCAM_region_names <- suppressMessages(read_csv("./inst/extdata/mappings/GCAM_region_names.csv", skip = 3))
  FAO_ag_items_PRODSTAT <- suppressMessages(read_csv("./inst/extdata/mappings/FAO_ag_items_PRODSTAT.csv", skip = 3))

  # Read prices & production (we'll weight prices by production when we can aggregate)
  faoPrices <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_prices.csv"))
  faoProd <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_prod.csv"))

  # Read in GDP deflator
  gdpDeflator <- suppressMessages(read_csv("./inst/extdata/hindcast-data/gdp_deflator.csv", skip = 1))

  # Tidy data
  faoProd %>%
    gather(year, prod, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
    faoProd

  faoPrices %>%
    gather(year, price, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
    faoPrices

  # Join data and compute average price
  faoPrices %>%
    left_join(faoProd, by=c("FAO_country", "item", "year")) %>%
    left_join(select(FAO_ag_items_PRODSTAT, item, GCAM_commodity), by="item") %>%
    # Convert prices to 1975$
    left_join(select(gdpDeflator, year, deflator), by="year") %>%
    mutate(price = price / deflator) %>%
    replace_na(list(price = 0, prod = 0)) %>%
    mutate(value = price * prod) %>%
    group_by(year, GCAM_commodity) %>%
    summarize(value = sum(value), prod = sum(prod)) %>%
    mutate(price = value / prod / 1000.0) %>%
    ungroup() ->
    faoPrices

  # Add prices for years prior to FAO data start
  faoPrices %>%
    filter(year == min(faoPrices$year)) %>%
    select(-year, -value, -prod) %>%
    rename(price_fy = price) ->
    faoPricesFirstYear

  faoPrices %>%
    select(GCAM_commodity) %>%
    distinct() %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) %>%
    left_join(faoPrices, by=c("GCAM_commodity", "year")) %>%
    left_join(faoPricesFirstYear, by=c("GCAM_commodity")) %>%
    mutate(price = if_else(is.na(price), price_fy, price)) %>%
    select(GCAM_commodity, year, price) %>%
    rename(sector = GCAM_commodity) ->
    faoPrices

  return(faoPrices)
}

