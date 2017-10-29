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
  fao_ha <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_ha.csv"))
  fao_prod <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_prod.csv"))

  # Tidy data
  fao_ha %>%
    gather(year, ha, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
    fao_ha

  fao_prod %>%
    gather(year, prod, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
    fao_prod

  # Join data and compute average yield
  fao_ha %>%
    left_join(fao_prod, by=c("FAO_country", "item", "year")) %>%
    left_join(select(agluCtry, FAO_country, iso), by="FAO_country") %>%
    left_join(select(iso_GCAM_regID, iso, GCAM_region_ID), by="iso") %>%
    left_join(GCAM_region_names, by="GCAM_region_ID") %>%
    left_join(select(FAO_ag_items_PRODSTAT, item, GCAM_commodity), by="item") %>%
    na.omit() %>%
    group_by(region, GCAM_commodity, year) %>%
    summarize(ha = sum(ha), prod = sum(prod)) %>%
    mutate(yield = prod / ha) %>%
    filter(year %in% YEARS) %>%
    select(region, GCAM_commodity, year, yield) ->
    fao_Yield

  # Print yields
  write_csv(fao_Yield, "./outputs/hindcast_yield.csv")

  # Compute AgProdChange
  fao_Yield %>%
    mutate(prev_year = year - 1) %>%
    filter(prev_year %in% YEARS) %>%
    left_join(fao_Yield, by=c("region", "GCAM_commodity", "prev_year" = "year")) %>%
    mutate(AgProdChange = (yield.x / yield.y) - 1) %>%
    select(region, GCAM_commodity, year, AgProdChange) ->
    fao_AgProdChange

  return(fao_AgProdChange)
}

#' get_hindcast_prices
#'
#' @details Read in FAO prices by country and crop,
#'          Aggregate to GCAM regions and commodities,
#' @return prices in historical period
#' @author KVC October 2017
#' @export
get_hindcast_prices <- function(){
  # Silence package checks
  prod <- price <- year <- value <- price_fy <- UNIQUE_JOIN_FIELD <- na.omit <-
    FAO_country <- GCAM_commodity <- item <- NULL

  # Read in mappings
  agluCtry <- suppressMessages(read_csv("./inst/extdata/mappings/AGLU_ctry.csv", skip = 3))
  iso_GCAM_regID <- suppressMessages(read_csv("./inst/extdata/mappings/iso_GCAM_regID.csv", skip = 3))
  GCAM_region_names <- suppressMessages(read_csv("./inst/extdata/mappings/GCAM_region_names.csv", skip = 3))
  FAO_ag_items_PRODSTAT <- suppressMessages(read_csv("./inst/extdata/mappings/FAO_ag_items_PRODSTAT.csv", skip = 3))

  # Read prices & production (we'll weight prices by production when we can aggregate)
  fao_prices <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_prices.csv"))
  fao_prod <- suppressMessages(read_csv("./inst/extdata/hindcast-data/fao_prod.csv"))

  # Tidy data
  fao_prod %>%
    gather(year, prod, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
    fao_prod

  fao_prices %>%
    gather(year, price, -FAO_country, -item) %>%
    mutate(year = as.integer(year)) ->
    fao_prices

  # Join data and compute average price
  fao_prices %>%
    left_join(fao_prod, by=c("FAO_country", "item", "year")) %>%
    left_join(select(FAO_ag_items_PRODSTAT, item, GCAM_commodity), by="item") %>%
    mutate(value = price * prod) %>%
    na.omit() %>%
    group_by(year, GCAM_commodity) %>%
    summarize(value = sum(value), prod = sum(prod)) %>%
    mutate(price = value / prod / 1000.0) %>%
    ungroup() ->
    fao_prices

  # Add prices for years prior to FAO data start
  fao_prices %>%
    filter(year == min(fao_prices$year)) %>%
    select(-year, -value, -prod) %>%
    rename(price_fy = price) ->
    fao_prices_fy

  fao_prices %>%
    select(GCAM_commodity) %>%
    distinct() %>%
    mutate(UNIQUE_JOIN_FIELD = 1) %>%
    full_join(mutate(tibble(year = YEARS), UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
    select(-UNIQUE_JOIN_FIELD) %>%
    left_join(fao_prices, by=c("GCAM_commodity", "year")) %>%
    left_join(fao_prices_fy, by=c("GCAM_commodity")) %>%
    mutate(price = if_else(is.na(price), price_fy, price)) %>%
    select(GCAM_commodity, year, price) %>%
    rename(sector = GCAM_commodity) ->
    fao_prices

  return(fao_prices)
}

