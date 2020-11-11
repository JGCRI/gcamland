context("Bayesian")

testscenarios <- readRDS('data/bayes-scenario-info.rds')

test_that("get_historical_land_data returns filtered FAO data", {
    ## no filtering
    expect_equivalent(get_historical_land_data() %>% select(region, year, land.type, obs),
                      Land_history %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))
    ## region filtering
    expect_equivalent(get_historical_land_data("USA") %>% select(region, year, land.type, obs),
                      dplyr::filter(Land_history, region=="USA") %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))
    ## year filtering
    expect_equivalent(get_historical_land_data(years=c(1972, 1984)) %>% select(region, year, land.type, obs),
                      dplyr::filter(Land_history, year==1972 | year==1984) %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))
    ## commodity filtering
    expect_equivalent(get_historical_land_data(commodities="Corn") %>% select(region, year, land.type, obs),
                      dplyr::filter(Land_history, GCAM_commodity == "Corn") %>%
                        dplyr::rename(land.type=GCAM_commodity, obs=area))

    # ## complex filter
    all_output_commodities <-
        c("UrbanLand", "Tundra", "RockIceDesert", "UnmanagedPasture",
          "Pasture", "Grassland", "Shrubland", "OtherArableLand", "Wheat",
          "SugarCrop", "Root_Tuber", "Rice", "PalmFruit", "OtherGrain",
          "OilCrop", "MiscCrop", "FodderHerb", "FodderGrass", "FiberCrop",
          "Corn", "UnmanagedForest", "Forest", "willow", "biomass")
    get_historical_land_data("Australia_NZ", 1972:1984,
                             all_output_commodities) %>% dplyr::select(-variable, -obsvar, -trend, -detrended) ->
      modelData
    modelData <- modelData[with(modelData, order(region, land.type, year)), ]

    readr::read_csv('data/complex_filter_ref.csv', col_types='ccdd') %>%
      dplyr::rename(land.type=GCAM_commodity, obs=area) ->
      compareData
    compareData <- compareData[with(compareData, order(region, land.type, year)), ]

    expect_equal(modelData$obs, compareData$obs, tolerance = 0.01)
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


test_that("The table produced by grand_table_bayes is grand.", {
    gt <- grand_table_bayes(testscenarios)
    expect_true(inherits(gt, 'data.frame'))
    expect_equal(nrow(gt), 100)
    expect_equal(ncol(gt), 13)
    expect_setequal(names(gt),
                    c("xi", "lp_", "expectation.type",
                      "share.old1", "share.old2", "share.old3", "share.old4", "share.old5",
                      "linear.years1", "linear.years2", "linear.years3", "linear.years4", "linear.years5",
                      "logit.agforest", "logit.afnonpast",
                      "logit.crop", "region"))
    expect_setequal(unique(gt$expectation.type), c("Perfect", "Adaptive", "HybridPerfectAdaptive", "Linear", "HybridLinearAdaptive"))
    expect_equal(unique(gt$region), "USA")
})

test_that("EV function produces correct answer.", {
    ev <- EV(testscenarios)
    expect_true(inherits(ev, 'data.frame'))
    expect_equal(nrow(ev), 5)
    expect_equal(ncol(ev), 11)
    expect_equal(ev$logit.agforest, rep(1.55, 5), tolerance=1e-4)
    expect_equal(ev$logit.afnonpast, rep(1.05, 5), tolerance=1e-4)
    expect_equal(ev$logit.crop, rep(1.05, 5), tolerance=1e-4)
    expect_equal(ev$xi, rep(1, 5), tolerance=1e-4)
})


test_that("waic function produces correct answer.", {
    w <- waic(testscenarios)
    expect_true(inherits(w, 'data.frame'))
    expect_equal(nrow(w), 5)
    expect_equal(w$waic[1], 4378.428, tolerance=1e-3)
    expect_equal(w$se[1], 164.7537, tolerance=1e-3)
    expect_equal(w$dwaic, c(0.0, 38.49833, 100.09073, 412.17123, 434.01699), tolerance=1e-4)
    expect_equal(w$se.dwaic, c(0.0, 8.230109, 24.071678, 34.265683, 49.118918), tolerance=1e-4)
    expect_equal(w$awgt, c(1.000000e+00, 4.367108e-09, 1.843206e-22, 3.148869e-90, 5.680770e-95))
})
