# process_hindcast_data.R

#' get_historic_yields
#'
#' @details Read in FAO production & ha by country and crop,
#'          Aggregate to GCAM regions and commodities,
#' @return YieldRatio in historical period
#' @import readr
#' @importFrom tidyr gather
#' @importFrom dplyr mutate
#' @importFrom stats aggregate
#' @author KVC June 2019
#' @export
get_historic_yields <- function(){
  # Silence package checks
  region <- yield <- na.omit <- FAO_country <- item <- iso <- GCAM_region_ID <- GCAM_commodity <- year <- ha <- NULL

  # Read in mappings
  agluCtry <- suppressMessages(read_csv(system.file("extdata", "./mappings/AGLU_ctry.csv", package = "gcamland"), skip = 3))
  iso_GCAM_regID <- suppressMessages(read_csv(system.file("extdata", "./mappings/iso_GCAM_regID.csv", package = "gcamland"), skip = 3))
  GCAM_region_names <- suppressMessages(read_csv(system.file("extdata", "./mappings/GCAM_region_names.csv", package = "gcamland"), skip = 3))
  FAO_ag_items_PRODSTAT <- suppressMessages(read_csv(system.file("extdata", "./mappings/FAO_ag_items_PRODSTAT.csv", package = "gcamland"), skip = 3))

  # Read production & harvested area (we'll calculate yield from this so we can aggregate)
  faoHA <- suppressMessages(read_csv(system.file("extdata", "./hindcast-data/fao_ha.csv", package = "gcamland"),
                                     col_types = cols(.default = "c")))
  faoProd <- suppressMessages(read_csv(system.file("extdata", "./hindcast-data/fao_prod.csv", package = "gcamland")))

  # Tidy data
  faoHA %>%
    gather(year, ha, -FAO_country, -item) %>%
    replace_na(list(ha = 0)) ->
    faoHA
  faoHA$year <- as.integer(faoHA$year)
  faoHA$ha <- as.integer(faoHA$ha)

  faoProd %>%
    gather(year, prod, -FAO_country, -item) %>%
    replace_na(list(prod = 0))->
    faoProd
  faoProd$year <- as.integer(faoProd$year)
  faoProd$prod <- as.integer(faoProd$prod)

  # Join data and compute average yield
  faoYield <- merge(faoHA, faoProd, by=c("FAO_country", "item", "year"), all.x = TRUE)
  faoYield <- merge(faoYield, agluCtry[c("FAO_country", "iso")], by=c("FAO_country"), all.x = TRUE)
  faoYield <- merge(faoYield, iso_GCAM_regID[c("iso", "GCAM_region_ID")], by=c("iso"), all.x = TRUE)
  faoYield <- merge(faoYield, GCAM_region_names, by=c("GCAM_region_ID"), all.x = TRUE)
  faoYield <- merge(faoYield, FAO_ag_items_PRODSTAT[c("item", "GCAM_commodity")], by=c("item"), all.x = TRUE)
  faoYield <- faoYield[c("region", "GCAM_commodity", "year", "ha", "prod")]
  faoYield <- na.omit(faoYield)
  faoYield <- aggregate(.~region + GCAM_commodity + year, faoYield, FUN="sum")
  faoYield$yield <- faoYield$prod / faoYield$ha
  faoYield <- faoYield[c("region", "GCAM_commodity", "year", "yield")]
  faoYield <- na.omit(faoYield)

  return(faoYield)
}

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
  year <- prev_year <- GCAM_commodity <- yield.x <- yield.y <- region <- AgProdChange <- NULL

  # Get yields and filter for hindcast years
  YIELDS.HIST %>%
    filter(year %in% YEARS$Hindcast) ->
    faoYield

  # Compute AgProdChange
  faoYield %>%
    mutate(prev_year = year - 1) %>%
    filter(prev_year %in% YEARS$Hindcast) %>%
    left_join(faoYield, by=c("region", "GCAM_commodity", "prev_year" = "year")) %>%
    mutate(AgProdChange = (yield.x / yield.y) - 1) %>%
    select(region, GCAM_commodity, year, AgProdChange) ->
    faoAgProdChange

  return(faoAgProdChange)
}

#' get_historic_yield_ratios
#'
#' @details Read in FAO production & ha by country and crop,
#'          Aggregate to GCAM regions and commodities,
#'          Calculate ratio of yield to first GCAM historical year
#' @return YieldRatio in historical period
#' @import readr
#' @importFrom tidyr gather
#' @importFrom dplyr mutate select left_join filter
#' @author KVC June 2019
#' @export
get_historic_yield_ratios <- function(){
  # Silence package checks
  year <- region <- yield <- base_yield <- NULL

  # Get yields and filter for years up to and including the final historical year
  YIELDS.HIST %>%
    filter(year <= min(YEARS$Hindcast),
           region == DEFAULT.REGION) ->
    faoYield

  # Calculate ratio to final historical year
  faoYield %>%
    filter(year == min(YEARS$Hindcast)) %>%
    select(-year) %>%
    rename(base_yield = yield) ->
    byYield

  faoYield %>%
    left_join(byYield, by=c("region", "GCAM_commodity")) %>%
    mutate(yieldRatio = yield / base_yield) %>%
    select(-yield, -base_yield) ->
    histYieldRatio

  return(histYieldRatio)
}

#' Historic FAO Yields
#'
#' Yields at geopolitical region level for all FAO years
#' @author Kate Calvin
YIELDS.HIST <- get_historic_yields()

#' Ratio of Yield to First Historical Year
#'
#' Yields at geopolitical region level for all FAO years
#' @include constants.R
#' @author Kate Calvin
YIELD.RATIOS <- get_historic_yield_ratios()
