#### Test model outputs.  Any test that requires actually running the model
#### should go in this file, so that we only have to run it once.

context("Model results")

basepath <- file.path(tempdir(), "outputs")
test.info <- SCENARIO.INFO
test.info$mOutputDir <- basepath

test_that("Model runs successfully.", {
    ## Run the model to generate outputs.  This needs to be the first test in
    ## this context, as all the rest will depend on these results.
    expect_message(run_model(test.info))
})


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

  # Get comparison data
  compareData <- read_csv("./comparison-data/HistLandAllocation.csv")
  compareData %>%
    filter(region == REGION, year <= YEARS[FINAL_CALIBRATION_PERIOD]) ->
    compareData

  # Look for output data in outputs under top level
  # (as this code will be run in tests/testthat)
  path <- file.path(basepath,"land")
  file <- file.path(path, paste0("landAllocation_", test.info$mScenarioName,
                                 ".csv"))
  expect_true(file.exists(file))
  read_csv(file) %>%
    mutate(region = REGION) ->
    outputData

  compareData %>%
    select(-land.allocation) %>%
    left_join(outputData, by=c("region", "year", "name")) %>%
    replace_na(list(land.allocation = 0)) %>%
    select(-scenario) ->
    outputData

  expect_identical(dim(outputData), dim(compareData),
                   info = paste("Dimensions are not the same for base year land allocation"))

  expect_equivalent(round_df(outputData), round_df(compareData),
                    info = paste("base year land allocation doesn't match"))

})

test_that("shares for all nodes add to 1", {
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
                    info = paste("shares don't all add to 1"))

})

test_that("perfect expectations about yield are calculated correctly", {
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

  # With perfect expectations, yield should match expected yield exactly
  if (SCENARIO.INFO$mExpectationType == "Perfect") {

    # Get actual data
    path <- basepath
    file <- file.path(path, "yield.csv")
    actualData <- read_csv(file)

    # Get expected data
    path <- file.path(basepath, "expectedYield")
    file <- file.path(path, paste0("expectedYield_", SCENARIO.INFO$mScenarioName, ".csv"))
    read_csv(file) %>%
      rename(yield = expectedYield) ->
      expectedData

    expect_identical(dim(actualData), dim(expectedData),
                     info = paste("Dimensions are not the same for expected yield with perfect expectations"))

    expect_equivalent(round_df(actualData), round_df(expectedData),
                      info = paste("Expected yield doesn't match actual yield"))

  }

})

test_that("land area doesn't change over time", {
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

  # Get output data
  # Look for output data in outputs under top level
  # (as this code will be run in tests/testthat)
  path <- file.path(basepath, "land")
  file <- file.path(path, paste0("landAllocation_", SCENARIO.INFO$mScenarioName, ".csv"))
  outputData <- read_csv(file)

  # Aggregate to regions
  outputData %>%
    separate(name, into=c("type", "AEZ"), sep="AEZ") %>%
    group_by(AEZ, year) %>%
    summarize(land.allocation = sum(land.allocation)) ->
    outputData

  # Create data frame for comparison, where land cover equals base year level in all years
  outputData %>%
    # Filter for first year
    filter(year == min(YEARS)) %>%
    select(-year) %>%
    # Copy to all years
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) %>%
    # Select appropriate columns
    select(AEZ, year, land.allocation) ->
    compareData

  expect_equivalent(round_df(outputData), round_df(compareData),
                    info = paste("total land area changes over time"))

})
