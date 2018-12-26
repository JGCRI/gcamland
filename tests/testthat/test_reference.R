# Test land cover in the reference scenario matches expected reference results

context("reference")

basepath <- file.path(tempdir(), "outputs")

test_that("land cover matches reference values", {
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

  if (SCENARIO.INFO$mScenarioName == "Reference_Perfect") {

    # Get comparison data
    compareData <- read_csv("./comparison-data/LandAllocation_Reference_Perfect.csv", skip = 1, col_types='cdic')
    compareData %>%
      filter(region == SCENARIO.INFO$mRegion,
             year %in% YEARS[[SCENARIO.INFO$mScenarioType]]) ->
      compareData

    # Look for output data in outputs under top level
    # (as this code will be run in tests/testthat)
    file <- file.path(basepath, paste0("output_", SCENARIO.INFO$mScenarioName, ".rds"))
    readRDS(file) %>%
      select(-yield, -expectedYield, -expectedPrice) %>%
      mutate(region = SCENARIO.INFO$mRegion) ->
      outputData

    compareData %>%
      select(-land.allocation) %>%
      left_join(outputData, by=c("region", "year", "name")) %>%
      replace_na(list(land.allocation = 0)) %>%
      select(-scenario) ->
      outputData

    expect_identical(dim(outputData), dim(compareData),
                     info = paste("Dimensions are not the same for reference land allocation"))

    expect_equivalent(round_df(outputData), round_df(compareData),
                      info = paste("reference land allocation doesn't match"))

  }

})
