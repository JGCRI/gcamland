context("Bayesian")

test_that("get_historical_data returns filtered FAO data", {
    ## no filtering
    expect_equivalent(get_historical_data(), FAO_land_history)
    ## region filtering
    expect_equivalent(get_historical_data("USA"),
                 dplyr::filter(FAO_land_history, region=="USA"))
    ## year filtering
    expect_equivalent(get_historical_data(years=c(1972, 1984)),
                 dplyr::filter(FAO_land_history, year==1972 | year==1984))
    ## commodity filtering
    expect_equivalent(get_historical_data(commodities="Corn"),
                 dplyr::filter(FAO_land_history, GCAM_commodity == "Corn"))

    ## complex filter
    all_output_commodities <-
        c("UrbanLand", "Tundra", "RockIceDesert", "UnmanagedPasture",
          "Pasture", "Grassland", "Shrubland", "OtherArableLand", "Wheat",
          "SugarCrop", "Root_Tuber", "Rice", "PalmFruit", "OtherGrain",
          "OilCrop", "MiscCrop", "FodderHerb", "FodderGrass", "FiberCrop",
          "Corn", "UnmanagedForest", "Forest", "willow", "biomass")
    expect_equivalent(get_historical_data("Australia_NZ", 1972:1984,
                                          all_output_commodities),
                      readr::read_csv('complex_filter_ref.csv'))
})

