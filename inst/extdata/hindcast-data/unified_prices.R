library(dplyr)
library(tidyr)
library(ggplot2)

# bring together many disparate price data sets into a single set that has 1970ish-2015 for
# all GCAM crop commodities and regions.


# 1.  from 1991-2012, it uses gcam data system values for all regions and commodities.
#     The way I updated the code to have it output prices for every year instead of just
#     the average 2008-2012 probably didn't have it fill out some weird Cotton data quite correctly ->
#     When you plot the final FiberCrop prices, the 1991-2012 years don't really fit the trend.
# 1a. Specifically, the US fibercrop behavior was definitely wrong - in joining the production data
#     to the price data around Line 179 of zchunk_LB1321.region_ag_prices.R, there end up being only
#     NA data despite there being production and price data for both cotton lint and cottonseed (not
#     seed cotton). This was because the join was done on `item.code` information.
#     So I dropped that column so that we could actually get data. This does not seem to impact other
#     commodities/regions significantly when I compare price time series with my changed gcamdata
#     output vs an old version that kept `item.code`. Basically, some but not all gross looking
#     behavior went away and it doesn't seem like new gross looking behavior was introduced.
# 1b. Canadian FiberCrop - post 1990, FAO has no data for any kind of fibercrop in Canada
#     (https://www.fao.org/faostat/en/#data/PP) and this is consistent with what comes out of
#     the gcam data system and with Ryna's data (no values post 1990, though we wouldn't use
#     them anyway). Pre 1990, FAO does have fibre crop nes data in Canada (https://www.fao.org/faostat/en/#data/PA)
#     which is presumably where Ryna's pre 1990 values are sourced from. This results in a massive
#     discontinuity int he price time series. We MAY need/want to drop any pre-1990 values and
#     just backfill the 1991 value. **CHECK WITH XIN**
# 2.  For years after 2012, all regions, all commodities, we use Ryna's values where we have
#     them and just extend 2012 values through 2015 when we don't.
# 3.  For years before 1991, for nonUSSR, non China regions, we use Ryna's data where we have it.
# 4.  For years before 1991, frmr USSR regions, we use FAO discontinued series producer prices,
#     with the units adjusted to 1975USD/kg via current year exchange rates (https://www.fao.org/faostat/en/#data/PE)
#     and then US GDP deflation (gcam data system fao gdp deflators assumption file)
#     (which Xin recommended). The time series look very reasonable in the frmr USSR regions
#     doing that.
# 5.  For years before 1991, China, I did the same calculations I did for the USSR with the
#     discontinued China data. This results in some very weird price time series for some crops
#     but that's a direct reflection of the exchange rate.
#     **CHECK WITH XIN**
# 6.  We ended up with Brazil prices from 1981, so I just extended the 1981 price back to 1975
# 7.  For any other commodities and regions with missing years in 1975-2015, we just extended the
#     closest available price through those missing years.


source('R/constants.R')

# -------------------------------------
# Basic FAO production data for weighting prices going from FAO items to GCAM commodities
faoPRODSTAT <- suppressMessages(read.csv(system.file("extdata", "./mappings/FAO_ag_items_PRODSTAT.csv", package = "gcamland"), skip=3))
fao_prod <- suppressMessages(read.csv(system.file("extdata", "./hindcast-data/fao_prod.csv", package = "gcamland"))) %>%
  gather(year, value, -FAO_country, -item, -nit) %>%
  mutate(year = as.integer(substr(year, 2, 5)),
         prod_tonnes = value,
         prod_kg = value*1000) %>%
  select(-nit, -value) %>%
  left_join(faoPRODSTAT %>% select(item, GCAM_commodity), by = 'item')%>%
  rename(Country = FAO_country, Item = item)

# Region names to make things nice
gcamRegionNames <- suppressMessages(read.csv(system.file("extdata", "./mappings/GCAM_region_names.csv", package = "gcamland"), skip=3))

# USA GDP eflator constants to convert prices sourced from the old, discontinued FAO producer price data (LCU)
# to 1975USD
aglu.DEFLATOR_BASE_YEAR     <- 2005 # year used as the basis for computing regional price deflators
suppressMessages(read.csv(system.file("extdata", "./hindcast-data/FAO_GDP_Deflators.csv",
                                      package = "gcamland"),
                          skip=6)) %>%
  filter(Area == 'United States of America')%>%
  select(Area, Year, Value) %>%
  group_by(Area) %>%
  mutate(currentUSD_per_baseyearUSD = (Value / Value[Year == aglu.DEFLATOR_BASE_YEAR])) %>%
  ungroup() ->
  fao_gdp_deflators

# -------------------------------------
# Read prices we will use for:
# 1. pre-1990, non USSR regions
# 2. post 2012, all regions
# (these are already aggregated to gcam commodity and region).
# These prices are in 2005 USD/tonne so they have to be converted to 1975 USD/kg.
faoPrices1 <- suppressMessages(read.csv(system.file("extdata", "./hindcast-data/prod_price_rgn_Ryna_pre1990_nonUSSR.csv", package = "gcamland"), skip=3))

# Convert to 1975$/kg, filter for the right region, rename columns
faoPrices1 %>%
  # filter(GCAM_region_name == DEFAULT.REGION) %>%
  rename(sector = GCAM_commod,
         price = pp_2005usd_tonne) %>%
  mutate(price = price / 3.05 / 1000) %>% # 3.05 converts from 2005$ to 1975$; 1000 converts from tonnes to kg
  select(GCAM_region_name, year, sector, price) ->
  faoPrices1

# -------------------------------------
# PRE 1991 USSR regions.

# Exchange Rates from https://www.fao.org/faostat/en/#data/PE
USSRexchange <- suppressMessages(read.csv(
  system.file("extdata", "./hindcast-data/FAOSTAT_data_1-4-2022_USSRcurrency_toUSD_exchangeRate_by_year.csv",
              package = "gcamland")))

frmrUSSR <- c('Central Asia', 'Europe_Eastern', 'Europe_Non_EU', 'Russia')

# Read prices we will use for pre-1990, USSR regions
# these are not aggregated to gcam commodity and region, so have to do that.
# also the prices are local currency unit per tonne.
# https://www.fao.org/faostat/en/#data/PA
faoPrices2 <- suppressMessages(read.csv(system.file("extdata", "./hindcast-data/prod_price_rgn_FAOSTAT_data_1-4-2022_pre1990_USSR.csv", package = "gcamland")))

# convert LCU/tonne to USD/tonne in each year via the FAO exchange rate.
# Then convert USD/tonne by year to 1975 USD/kg.
# Then convert FAO commodity prices to GCAM commodity prices via
# weighted average using production as weights.
faoPrices2 %>%
  select(-Domain.Code, -Country.Code..FAO., -Element.Code, -Item.Code,
         -Year.Code, -Flag, -Flag.Description) %>%
  left_join(USSRexchange %>% select(Country = Area, exchangeRateUnit=Item, Year, exchangeRate=Value),
            by=c('Country', 'Year')    ) %>%
  mutate(pp_currentUSD_tonne = Value/exchangeRate) %>%
  select(-Element, -Unit, -Value, -exchangeRateUnit, -exchangeRate) %>%
  # convert yearly USD per tonne to 1975USD per kg for every year:
  left_join(fao_gdp_deflators %>% select(Year, currentUSD_per_baseyearUSD),
            by = 'Year')  %>%
  # 3.05 converts from 2005$ to 1975$; 1000 converts from tonnes to kg
  mutate(pp1975usd_per_kg = (pp_currentUSD_tonne/currentUSD_per_baseyearUSD)/3.05 / 1000) %>%
  select(Country, Item, year = Year, price = pp1975usd_per_kg) %>%
  left_join(fao_prod, by = c("Country" , "Item", "year") ) %>%
  mutate(revenue = price*prod_kg) %>%
  group_by(Country, GCAM_commodity, year) %>%
  summarise(revenue = sum(revenue),
            production = sum(prod_kg)) %>%
  ungroup() %>%
  mutate(price = revenue / production)  %>%
  na.omit %>%
  select(-revenue, -production) %>%
  mutate(Country = as.character(Country),
         sector = as.character(GCAM_commodity)) %>%
  select(-GCAM_commodity) ->
  faoPrices2 # 1975USD/kg for the USSR, 1970-1990

# -------------------------------------
# read prices we will use 1991-2012, all regions
# (these are  aggregated to gcam commodity and region)
# in 1975USD/kg
faoPrices3 <- suppressMessages(read.csv(system.file("extdata", "./hindcast-data/L1321.prP_R_C_Y_75USDkg.csv", package = "gcamland"), skip=1)) %>%
  left_join(gcamRegionNames, by = 'GCAM_region_ID') %>%
  select(-GCAM_region_ID) %>%
  rename(price = value,
         GCAM_region_name = region) %>%
  mutate(sector =if_else(as.character(GCAM_commodity) == 'RootTuber',
                         'Root_Tuber', as.character(GCAM_commodity))) %>%
  select(-GCAM_commodity)

# -------------------------------------
# Read in prices for China that we will use pre 1991

# Exchange Rates from https://www.fao.org/faostat/en/#data/PE
BCexchange <- suppressMessages(read.csv(
  system.file("extdata", "./hindcast-data/FAOSTAT_data_1-4-2022_Brazil_China_currency_toUSD_exchangeRate_by_year.csv",
              package = "gcamland"),  stringsAsFactors = FALSE)) %>%
  mutate(Area = if_else(Area == 'China, mainland', 'China', Area))


# Read prices we will use
# these are not aggregated to gcam commodity and region, so have to do that.
# also the prices are local currency unit per tonne.
# https://www.fao.org/faostat/en/#data/PA
faoPrices4 <- suppressMessages(read.csv(
  system.file("extdata", "./hindcast-data/prod_price_rgn_FAOSTAT_data_1-4-2022_pre1990_Brazil_China.csv",
              package = "gcamland"), stringsAsFactors = FALSE))

# convert LCU/tonne to USD/tonne in each year via the FAO exchange rate.
# Then convert USD/tonne by year to 1975 USD/kg.
# Then convert FAO commodity prices to GCAM commodity prices via
# weighted average using production as weights.
faoPrices4 %>%
  filter(Country == 'China') %>%
  select(-Domain.Code, -Country.Code..FAO., -Element.Code, -Item.Code,
         -Year.Code, -Flag, -Flag.Description) %>%
  left_join(BCexchange %>% select(Country = Area, exchangeRateUnit=Item, Year, exchangeRate=Value),
            by=c('Country', 'Year')    ) %>%
  mutate(pp_currentUSD_tonne = Value/exchangeRate) %>%
  select(-Element, -Unit, -Value, -exchangeRateUnit, -exchangeRate) %>%
  # convert yearly USD per tonne to 1975USD per kg for every year:
  left_join(fao_gdp_deflators %>% select(Year, currentUSD_per_baseyearUSD),
            by = 'Year')  %>%
  # 3.05 converts from 2005$ to 1975$; 1000 converts from tonnes to kg
  mutate(pp1975usd_per_kg = (pp_currentUSD_tonne/currentUSD_per_baseyearUSD)/3.05 / 1000) %>%
  select(Country, Item, year = Year, price = pp1975usd_per_kg) %>%
  left_join(fao_prod, by = c("Country" , "Item", "year") ) %>%
  mutate(revenue = price*prod_kg) %>%
  group_by(Country, GCAM_commodity, year) %>%
  summarise(revenue = sum(revenue),
            production = sum(prod_kg)) %>%
  ungroup() %>%
  mutate(price = revenue / production)  %>%
  na.omit %>%
  select(-revenue, -production) %>%
  mutate(GCAM_region_name = as.character(Country),
         sector = as.character(GCAM_commodity)) %>%
  select(-GCAM_commodity, -Country) ->
  faoPrices4 # 1975USD/kg for China

# -------------------------------------
# Bring these prices together into one file.
prices_post2012 <- faoPrices1 %>% filter(year > 2012)
prices_1991_2012 <- faoPrices3
prices_pre1991_nonUSSR <- faoPrices1 %>%
  filter(!(GCAM_region_name %in% frmrUSSR) ) %>%
  filter(year < 1991)
prices_pre1991_frmrUSSR <- faoPrices1 %>%
  filter(GCAM_region_name %in% frmrUSSR)%>%
  select(-price, -year) %>%
  distinct() %>%
  left_join(faoPrices2 %>% select(-Country), by = c("sector")) %>%
  na.omit
prices_pre1991_China <- faoPrices4 %>% filter(year < 1991)

bind_rows(prices_post2012,
          prices_1991_2012,
          prices_pre1991_nonUSSR,
          prices_pre1991_frmrUSSR,
          prices_pre1991_China) %>%
  select(GCAM_region_name,  sector,    price, year) %>%
  arrange(GCAM_region_name, sector, year) %>%
  filter(sector != 'FodderHerb')->
  faoPrices

# rm(faoPrices1,
#    faoPrices2,
#    faoPrices3, faoPrices4,
#    prices_post2012,
#    prices_1991_2012,
#    prices_pre1991_nonUSSR,
#    prices_pre1991_frmrUSSR,
#    prices_pre1991_China)

# -------------------------------------

# Deal with missing years for non-palmfruit
subset_miss_years <- function(faoPricesdf){
  faoPricesdf %>%
    filter(sector != "PalmFruit") %>%
    group_by(GCAM_region_name, sector) %>%
    mutate(nyears = n(),
           minyear = min(year),
           maxyear = max(year)) %>%
    ungroup %>%
    filter(maxyear < max(YEARS$Hindcast) | minyear > min(YEARS$Hindcast),
           sector %in% c(CROP_GROUP1,
                         CROP_GROUP2,
                         CROP_GROUP3,
                         CROP_GROUP4,
                         CROP_GROUP5)) %>%
    arrange(GCAM_region_name, sector, year) ->
    missingYears
  return(missingYears)
}

missingYears <- subset_miss_years(faoPrices)

# do the easy options - things that are maybe missing 2012-2015,
# just extend to 2015
missingYears %>%
  filter(maxyear >= 2012,
         maxyear < 2015) %>%
  filter(year == maxyear) %>%
  select(GCAM_region_name, sector,price, nyears, minyear, maxyear) %>%
  distinct %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) %>%
  filter(year > maxyear) %>%
  select(-nyears, -minyear, -maxyear) %>%
  bind_rows(faoPrices) %>%
  distinct ->
  faoPrices

missingYears <- subset_miss_years(faoPrices)

# fix any interior missing years (eg data for 1991-2015 but not 1997)
missingYears %>%
  mutate(Length_should_be = maxyear - minyear+1) %>%
  filter(nyears != Length_should_be) ->
  interior_missing
#TODO assert that interior_Missing has 0 rows - it does right now.

# All that's left is missing years on the front end.
# For now, just do whatever the earliest price is repeated back.
missingYears %>%
  filter(year == minyear) %>%
  select(GCAM_region_name, sector,price, nyears, minyear, maxyear) %>%
  distinct %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) %>%
  filter(year < minyear) %>%
  select(-nyears, -minyear, -maxyear) %>%
  bind_rows(faoPrices) %>%
  distinct ->
  faoPrices

# TODO assert that there's now 0 missing years:
missingYears2 <- subset_miss_years(faoPrices)

# -------------------------------------
# Deal with palmfruit
faoPrices %>%
  filter(sector == 'PalmFruit') %>%
  group_by(GCAM_region_name, sector) %>%
  mutate(nyears = n(),
         minyear = min(year),
         maxyear = max(year)) %>%
  ungroup ->
  palmfruit

palmfruit %>%
  filter(nyears >= length(YEARS$Hindcast))  %>%
  select(-nyears, -maxyear, -minyear) ->
  goodpalm

# places where we drag 1991 back and 2012 forward:
palmfruit %>%
  filter(nyears == 22,
         minyear == 1991,
         maxyear == 2012) ->
  simplepalm

simplepalm %>%
  filter(year == 1991) %>%
  select(-year) %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) %>%
  filter(year < 1991) %>%
  select(-nyears, -minyear, -maxyear) ->
  simplepalm_pre1991

simplepalm %>%
  filter(year == 2012) %>%
  select(-year) %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) %>%
  filter(year > 2012) %>%
  select(-nyears, -minyear, -maxyear) %>%
  bind_rows(simplepalm_pre1991,
            simplepalm %>% select(-nyears, -minyear, -maxyear) ,
            goodpalm) ->
  goodpalm

rm(simplepalm, simplepalm_pre1991)


# leaves only Brazil, USA
# TODO add some assertions/tests
palmfruit %>%
  filter(!(GCAM_region_name %in% unique(goodpalm$GCAM_region_name))) ->
  remaining_palm

# Brazil has 1981-2012 data. Fill 2012 out to 2015
remaining_palm %>%
  filter(GCAM_region_name == 'Brazil',
         nyears < length(YEARS$Hindcast))%>%
  filter(year == 2012) %>%
  select(-year) %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) %>%
  filter(year > 2012) %>%
  select(-nyears, -minyear, -maxyear) %>%
  bind_rows(remaining_palm %>%
              filter(GCAM_region_name == 'Brazil',
                     nyears < length(YEARS$Hindcast)) %>%
              select(-nyears, -minyear, -maxyear) ,
            goodpalm)->
  goodpalm

remaining_palm %>%
  filter(GCAM_region_name == 'Brazil',
         nyears < length(YEARS$Hindcast))%>%
  filter(year == minyear) %>%
  select(-year) %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) %>%
  filter(year < minyear) %>%
  select(-nyears, -minyear, -maxyear) %>%
  bind_rows(remaining_palm %>%
              filter(GCAM_region_name == 'Brazil',
                     nyears < length(YEARS$Hindcast)) %>%
              select(-nyears, -minyear, -maxyear) ,
            goodpalm)->
  goodpalm

# USA has 1991-2015 data. Fill 1991 back
remaining_palm %>%
  filter(GCAM_region_name == 'USA',
         nyears < length(YEARS$Hindcast))%>%
  filter(year == 1991) %>%
  select(-year) %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(year = YEARS$Hindcast), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) %>%
  filter(year < 1991) %>%
  select(-nyears, -minyear, -maxyear) %>%
  bind_rows(remaining_palm %>%
              filter(GCAM_region_name == 'USA',
                     nyears < length(YEARS$Hindcast))%>%
              select(-nyears, -minyear, -maxyear) ,
            goodpalm)->
  goodpalm

rm(remaining_palm, palmfruit)

faoPrices %>%
  filter(sector!="PalmFruit") %>%
  bind_rows(goodpalm)->
  faoPrices

rm(goodpalm)

# -------------------------------------
# test plotting
faoPrices %>%
  filter(sector %in% c(CROP_GROUP1,
                       CROP_GROUP2,
                       CROP_GROUP3,
                       CROP_GROUP4,
                       CROP_GROUP5),
         year %in% YEARS$Hindcast) ->
  faoPrices

# p<- ggplot(faoPrices %>% filter(sector == 'Corn')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('Corn')
# ggsave('inst/extdata/hindcast-data/price_time_series/Corn.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'FiberCrop')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('FiberCrop')
# ggsave('inst/extdata/hindcast-data/price_time_series/FiberCrop.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'MiscCrop')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('MiscCrop')
# ggsave('inst/extdata/hindcast-data/price_time_series/MiscCrop.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'OtherGrain')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('OtherGrain')
# ggsave('inst/extdata/hindcast-data/price_time_series/OtherGrain.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'OilCrop')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('OilCrop')
# ggsave('inst/extdata/hindcast-data/price_time_series/OilCrop.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'Rice')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('Rice')
# ggsave('inst/extdata/hindcast-data/price_time_series/Rice.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'Root_Tuber')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('Root_Tuber')
# ggsave('inst/extdata/hindcast-data/price_time_series/RootTuber.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'SugarCrop')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('SugarCrop')
# ggsave('inst/extdata/hindcast-data/price_time_series/SugarCrop.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'Wheat')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('Wheat')
# ggsave('inst/extdata/hindcast-data/price_time_series/Wheat.jpg', p,
#        width = 12, height = 6, units = 'in')
#
# p<- ggplot(faoPrices %>% filter(sector == 'PalmFruit')) +
#   geom_point(aes(x=year, y=price), size = 0.2) +
#   facet_wrap(~GCAM_region_name, nrow = 4) +
#   ggtitle('PalmFruit')
# ggsave('inst/extdata/hindcast-data/price_time_series/PalmFruit.jpg', p,
#        width = 12, height = 6, units = 'in')

# -------------------------------------
# save final unified prices
write.csv(faoPrices, 'inst/extdata/hindcast-data/prod_price_rgn_unified.csv', row.names = FALSE)


# old <- read.csv('inst/extdata/hindcast-data/prod_price_rgn_unified_old.csv', stringsAsFactors = F) %>%
#   rename(old=price)
#
# faoPrices %>%
#   left_join(old, by = c('GCAM_region_name', 'sector', 'year')) %>%
#   mutate(difference=price-old) ->
#   compare
#
# compare %>%
#   group_by(GCAM_region_name, sector) %>%
#   summarize(maxdiff = max(abs(difference)),
#             mindiff=min(abs(difference))) %>%
#   ungroup ->
#   x1
#
# x1%>%
#   gather(metric,  value, -sector, -GCAM_region_name) -> x
#
# ggplot(x %>% filter(sector!='FiberCrop')) +
#   geom_point(aes(x=sector, y=value, color=metric)) +
#   facet_wrap(~GCAM_region_name, nrow=4)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#
# ggplot(x %>% filter(sector=='FiberCrop')) +
#   geom_point(aes(x=sector, y=value, color=metric)) +
#   facet_wrap(~GCAM_region_name, nrow=4)+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
