context("Bayesian")

test_that("get_historical_land_data returns filtered FAO data", {
    ## no filtering
    expect_equivalent(get_historical_land_data() %>% select(-variable, -obsvar),
                      FAO_land_history %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))
    ## region filtering
    expect_equivalent(get_historical_land_data("USA") %>% select(-variable, -obsvar),
                      dplyr::filter(FAO_land_history, region=="USA") %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))
    ## year filtering
    expect_equivalent(get_historical_land_data(years=c(1972, 1984)) %>% select(-variable, -obsvar),
                      dplyr::filter(FAO_land_history, year==1972 | year==1984) %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))
    ## commodity filtering
    expect_equivalent(get_historical_land_data(commodities="Corn") %>% select(-variable, -obsvar),
                      dplyr::filter(FAO_land_history, GCAM_commodity == "Corn") %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))

    ## complex filter
    all_output_commodities <-
        c("UrbanLand", "Tundra", "RockIceDesert", "UnmanagedPasture",
          "Pasture", "Grassland", "Shrubland", "OtherArableLand", "Wheat",
          "SugarCrop", "Root_Tuber", "Rice", "PalmFruit", "OtherGrain",
          "OilCrop", "MiscCrop", "FodderHerb", "FodderGrass", "FiberCrop",
          "Corn", "UnmanagedForest", "Forest", "willow", "biomass")
    expect_equivalent(get_historical_land_data("Australia_NZ", 1972:1984,
                                               all_output_commodities) %>% select(-variable, -obsvar),
                      readr::read_csv('complex_filter_ref.csv') %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))
})


test_that("Functions returned by get_lpdf are valid", {
    x <- c(1,1, 0.5, 0.5)
    sig <- c(1, 0.5, 1, 0.5)

    f1 <- get_lpdf(Inf)
    expect_equal(f1(x,sig),
                 dnorm(x, sd=sig, log=TRUE))

    f2 <- get_lpdf(1)
    expect_equal(f2(x,sig),
                 dcauchy(x, scale=sig, log=TRUE))

    ## Verify that it works with fractional df
    f3 <- get_lpdf(2.5)
    expect_equal(f3(x,sig),
                 dt(x/sig, df=2.5, log=TRUE) - log(sig))

    expect_error({f4 <- get_lpdf(0)})
    expect_error({f5 <- get_lpdf(-1)})
    expect_error({f6 <- get_lpdf(c(1,2,3,4))})
})
