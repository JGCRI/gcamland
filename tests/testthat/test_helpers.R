context("helper functions")

test_that("invalid input detected in get_timestep", {
    expect_error(get_timestep(0, "Reference"), "Invalid period")
    expect_error(get_timestep(1, "Reference"), "Invalid period")
    expect_error(get_timestep(999, "Reference"), "Invalid period")
})

test_that("scenario names are generated correctly", {
    expect_equal(getScenName("test", "Perfect", 5, -3, -3, -3),
                 "test_Perfect_AgroForest-3_AgroForestNonPasture-3_Cropland-3")
    expect_equal(getScenName("test", "Linear", 5, -3, -3, -3),
                 "test_Linear5_AgroForest-3_AgroForestNonPasture-3_Cropland-3")
    expect_equal(getScenName("test", "Adaptive", 0.5, -3, -3, -3),
                 "test_Adaptive0.5_AgroForest-3_AgroForestNonPasture-3_Cropland-3")
})

test_that("Scenario info objects can be converted to lists and restored.", {
    si1 <- SCENARIO.INFO
    ll <- as.list(si1)
    si2 <- as.ScenarioInfo(ll)
    expect_equivalent(si1, si2)
})

