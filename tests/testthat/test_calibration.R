# Test land cover in the calibration period matches read in values

context("calibration")

test_that("land cover matches calibration data", {
  # Finally, test (NB rounding numeric columns to a sensible number of
  # digits; otherwise spurious mismatches occur)
  # Also first converts integer columns to numeric (otherwise test will
  # fail when comparing <int> and <dbl> columns)
  DIGITS <- 1
  round_df <- function(x, digits = DIGITS) {
    integer_columns <- sapply(x, class) == "integer"
    x[integer_columns] <- lapply(x[integer_columns], as.numeric)

    numeric_columns <- sapply(x, class) == "numeric"
    x[numeric_columns] <- round(x[numeric_columns], digits)
    x
  }

  # For now, only test normal GCAM config
  if( SCENARIO.INFO$mScenario == "Reference" & SCENARIO.INFO$mExpectationType == "Perfect") {
    # Get comparison data
    compareData <- read_csv("./comparison-data/LandAllocation_Reference.csv", skip = 1)
    compareData %>%
      rename(name = `land-allocation`) %>%
      gather(year, land.allocation, -scenario, -region, -name, -Units) %>%
      mutate(year = as.integer(year)) %>%
      filter(region == REGION, year %in% YEARS) %>%
      filter(year <= YEARS[FINAL_CALIBRATION_PERIOD]) %>%
      select(name, year, land.allocation) ->
      compareData

    # Look for output data in outputs under top level
    # (as this code will be run in tests/testthat)
    outputData <- read_csv("../../outputs/land/landAllocation_Reference_Perfect.csv")
    outputData %>%
      # Filter for years we have comparison data
      filter(year %in% unique(compareData$year)) %>%
      select(name, year, land.allocation) ->
      outputData

    expect_identical(dim(outputData), dim(compareData),
                     info = paste("Dimensions are not the same for base year land allocation"))

    expect_equivalent(round_df(outputData), round_df(compareData),
                      info = paste("base year land allocation doesn't match"))
  }

})
