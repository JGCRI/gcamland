#' get_prices
#'
#' @details Read in prices for all periods and return them
#' @param aScenType Type of scenario to run, either "Reference" or "Hindcast".
#' @return Tibble containing prices by commodity and year
#' @importFrom readr read_csv
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @author KVC October 2017
#' @export
get_prices <- function(aScenType) {
  # Silence package checks
  region <- sector <- year <- price <- scenario <- Units <- subregion <- NULL

  # Get prices
  if(grepl("Hindcast", aScenType)) {
    prices <- get_hindcast_prices()
    if(aScenType == "Hindcast5yr") {
      # Need 5 year average price instead of single year
      # Ideally this would use the timestep information
      prices %>%
        mutate(year1 = year,
               year = round(year / 5) * 5) %>%
        group_by(sector, year) %>%
        summarize(price = mean(price)) %>%
        ungroup() ->
        prices
    }
  } else {
    file <- paste("./scenario-data/AgPrices_", aScenType, ".csv", sep="")
    prices <- suppressMessages(read_csv(system.file("extdata", file, package = "gcamland"), skip = 1))

    # Tidy data
    if(aScenType == "PCHES") {
      # PCHES scenarios have subregional price information
      prices %>%
        select(-scenario, -Units) %>%
        gather(year, price, -region, -sector, -subregion) %>%
        mutate(year = as.integer(year)) ->
        prices
    } else {
      prices %>%
        select(-scenario, -Units) %>%
        gather(year, price, -region, -sector) %>%
        mutate(year = as.integer(year)) ->
        prices
    }

  }

  # Filter for only years included in model simulation (or those before start year)
  prices %>%
    filter(year <= max(YEARS[[aScenType]])) ->
    prices

  return(prices)
}

#' get_hindcast_prices
#'
#' @details Read in FAO prices by GCAM regions and commodity. Prices were processed by Ryna.
#' @return prices in historical period
#' @import dplyr
#' @importFrom readr read_csv
#' @author KVC October 2018
#' @export
get_hindcast_prices <- function(){
  # Silence package checks
  price <- year <- sector <- GCAM_region_name <- GCAM_commod <- pp_2005usd_tonne <- uniqueJoinField <-  NULL

  # Read prices (these are already aggregated to gcam commodity and region)
  faoPrices <- suppressMessages(read.csv(system.file("extdata", "./hindcast-data/prod_price_rgn_unified.csv", package = "gcamland")))

  # Convert to 1975$/kg, filter for the right region, rename columns
  faoPrices %>%
    distinct %>%
    #dplyr::filter(GCAM_region_name == DEFAULT.REGION) %>%
    rename(region = GCAM_region_name) %>%
    # mutate(price = price / 3.05 / 1000) %>% # 3.05 converts from 2005$ to 1975$; 1000 converts from tonnes to kg
    # ^ already done
    select(region, year, sector, price) ->
    faoPrices

  # Prices for PalmFruit are missing prior to 1991, copy 1990 prices backward
  faoPrices %>%
    filter(sector == "PalmFruit",
           year == 1991) %>%
    select(-year) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) %>%
    filter(year < 1991) %>%
    bind_rows(faoPrices) ->
    faoPrices

  # Forest, Fodder, biomass & Pasture prices are missing from FAO price data set
  # Read in from a separate file
  extraPrices <- suppressMessages(read.csv(system.file("extdata", "./hindcast-data/price_extra.csv", package = "gcamland"), skip=4))

  # for nonUS regions, set the extra prices to a value of 1 (any nonzero constant)
  faoPrices %>%
    select(region) %>%
    distinct %>%
    filter(region != 'USA') %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(extraPrices%>%
                select(sector, year) %>%
                distinct %>%
                mutate(uniqueJoinField = 1,
                       price = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) %>%
    bind_rows(extraPrices) ->
    extraPrices

  # bind to faoPrices
  faoPrices %>%
    bind_rows(extraPrices) ->
    faoPrices


  return(faoPrices)
}

#' Price tables for each scenario type.
#'
#' Currently supported types are "Reference" and "Hindcast".  This structure is
#' a list of tibbles with all of the prices for the model, for each scenario.
#' @include constants.R
#' @author Kate Calvin, Robert Link
PRICES <- sapply(SCEN.TYPES, get_prices, simplify=FALSE, USE.NAMES=TRUE)
