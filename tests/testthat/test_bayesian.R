context("Bayesian")

testscenarios <- readRDS('data/scenario-info.rds')

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


test_that("The table produced by grand_table is grand.", {
    gt <- grand_table(testscenarios)
    expect_true(inherits(gt, 'data.frame'))
    expect_equal(nrow(gt), 10000)
    expect_equal(ncol(gt), 9)
    expect_setequal(names(gt),
                    c("xi", "lp_", "expectation.type", "share.old",
                      "linear.years", "logit.agforest", "logit.afnonpast",
                      "logit.crop", "region"))
    expect_setequal(unique(gt$expectation.type), c("Perfect", "Lagged"))
    expect_equal(unique(gt$region), "USA")
    expect_true(all(is.na(gt$share.old)))
    expect_true(all(is.na(gt$linear.years)))
})


test_that("MAP function produces correct answer.", {
    map <- MAP(testscenarios)
    expect_true(inherits(map, 'data.frame'))
    expect_equal(nrow(map), 2)
    expect_equal(ncol(map), 8)
    expect_equal(map$logit.agforest, rep(1.21875, 2), tolerance=1e-4)
    expect_equal(map$logit.afnonpast, rep(3.65625, 2), tolerance=1e-4)
    expect_equal(map$logit.crop, rep(3.84375, 2), tolerance=1e-4)
    expect_equal(map$xi, rep(0.4, 2))
    expect_equal(map$dev_, rep(146.3854, 2))
    expect_true(all(is.na(map$share.old)))
    expect_true(all(is.na(map$linear.years)))
})



test_that("EV function produces correct answer.", {
    ev <- EV(testscenarios)
    expect_true(inherits(ev, 'data.frame'))
    expect_equal(nrow(ev), 2)
    expect_equal(ncol(ev), 7)
    expect_equal(ev$logit.agforest, rep(1.961597, 2), tolerance=1e-4)
    expect_equal(ev$logit.afnonpast, rep(3.172973, 2), tolerance=1e-4)
    expect_equal(ev$logit.crop, rep(3.530459, 2), tolerance=1e-4)
    expect_equal(ev$xi, rep(0.439868, 2))
    expect_true(all(is.na(ev$share.old)))
    expect_true(all(is.na(ev$linear.years)))
})


test_that("waic function produces correct answer.", {
    w <- waic(testscenarios)
    expect_true(inherits(w, 'data.frame'))
    expect_equal(nrow(w), 2)
    expect_equal(w$waic[1], 152.7175, tolerance=1e-3)
    expect_equal(w$se[1], 8.080363, tolerance=1e-3) # rethinking::waic reported
                                        # se.waic ~ 8.1 for this model.
    ## These next two tests need work.  Since we are using two copies of the
    ## same model, dwaic and se.dwaic are both zero.  Still, we can at least
    ## verify that the calculations run.
    expect_equal(w$dwaic, c(0.0, 0.0))
    expect_equal(w$se.dwaic, c(0.0, 0.0))
    expect_equal(w$awgt, c(0.5, 0.5))
})

test_that("HPDI function produces correct answer.", {
    ilst <- HPDI(testscenarios)

    ## List should be two copies of the same matrix
    expect_true(inherits(ilst, 'list'))
    expect_equal(length(ilst), 2)
    expect_equivalent(ilst[[1]], ilst[[2]])
    expect_true(inherits(ilst[[1]], 'matrix'))

    hpdi_ref <- structure(c(0.33984375, 1.74609375, 1.3828125, NA, NA, 0.2, 4.95703125,
                            4.76953125, 5.68359375, NA, NA, 0.8), .Dim = c(6L, 2L))
    expect_equivalent(ilst[[1]], hpdi_ref)
})
