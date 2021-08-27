context("Model results with read in share weights")

basepath <- file.path(tempdir(), "outputs")
test.info <- update_scen_info(aName = "ReadInShWt", aCalibrateShareWt = FALSE)
test.info$mOutputDir <- basepath

test_that("Model runs successfully with read in share weights.", {
    ## Run the model to generate outputs.  This needs to be the first test in
    ## this context, as all the rest will depend on these results.
    expect_message(run_model(test.info, aVerbose=TRUE))
})


test_that("shares for all nodes add to 1 with read in share weights", {
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

  # Get outputs
  path <- basepath
  file <- file.path(path, "landShares.csv")
  outputData <- read_csv(file, col_types = "ccdi")

  # Aggregate to nodes
  outputData %>%
    group_by(parent, year) %>%
    summarize(share = sum(share)) ->
    outputData

  # Create data frame for comparison, where land cover equals base year level in all years
  outputData %>%
    mutate(share = 1) ->
    compareData

  expect_equivalent(round_df(outputData), round_df(compareData),
                    info = paste("shares don't all add to 1 with read in share weights"))

})

test_that("land cover with read in share weights matches reference values", {
  # Finally, test (NB rounding numeric columns to a sensible number of
  # digits; otherwise spurious mismatches occur)
  # Also first converts integer columns to numeric (otherwise test will
  # fail when comparing <int> and <dbl> columns)
  DIGITS <- 0
  round_df <- function(x, digits = DIGITS) {
    integer_columns <- sapply(x, class) == "integer"
    x[integer_columns] <- lapply(x[integer_columns], as.numeric)

    numeric_columns <- sapply(x, class) == "numeric"
    x[numeric_columns] <- round(x[numeric_columns], digits)
    x
  }

  # Get comparison data
  compareData <- read.csv("./comparison-data/LandAllocation_Reference_Perfect.csv", skip = 1, stringsAsFactors = FALSE)
  expect_true(is.character(compareData$region))
  expect_true(is.character(compareData$name))
  expect_true(is.integer(compareData$year))
  expect_true(is.numeric(compareData$land.allocation))
  compareData %>%
    filter(region == SCENARIO.INFO$mRegion,
           year >= 2010) -> # We are using 2010 share weights, so 1990 & 2005 won't match
    compareData

  # Look for output data in outputs under top level
  # (as this code will be run in tests/testthat)
  file <- file.path(basepath, paste0("output_", test.info$mScenarioName, ".rds"))
  readRDS(file) %>%
    select(-yield, -expectedYield, -expectedPrice, -cost, -expectedProfit, -shareWeight, -scenario, -harvested.land) %>%
    mutate(region = SCENARIO.INFO$mRegion) %>%
    filter(year >= 2010) -> # We are using 2010 share weights, so 1990 & 2005 won't match
    outputData

  expect_identical(dim(outputData), dim(compareData),
                   info = paste("Dimensions are not the same with read in shareweights"))

  expect_equal(outputData$land.allocation, compareData$land.allocation, tolerance = 0.01,
                    info = paste("read in share weight land allocation doesn't match reference"))

})
