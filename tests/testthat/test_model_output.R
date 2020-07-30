#### Test model outputs.  Any test that requires actually running the model
#### should go in this file, so that we only have to run it once.

context("Model results")

basepath <- file.path(tempdir(), "outputs")
test.info <- SCENARIO.INFO
test.info$mOutputDir <- basepath
scentype <- test.info$mScenarioType

test_that("Model runs successfully.", {
    ## Run the model to generate outputs.  This needs to be the first test in
    ## this context, as all the rest will depend on these results.
    expect_message(run_model(test.info, aVerbose=TRUE))
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
  compareData <- read_csv("./comparison-data/HistLandAllocation.csv", col_types='ccid')
  finalcalper <- TIME.PARAMS[[scentype]]$FINAL_CALIBRATION_PERIOD
  compareData %>%
    filter(region == test.info$mRegion,
           year <= YEARS[[scentype]][finalcalper]) ->
    compareData

  # Look for output data in outputs under top level
  # (as this code will be run in tests/testthat)
  path <- basepath
  file <- file.path(path, paste0("output_", test.info$mScenarioName, ".rds"))
  expect_true(file.exists(file))
  readRDS(file) %>%
    select(-yield, -expectedPrice, -expectedYield, -cost, -expectedProfit, -shareWeight, -harvested.land) %>%
    mutate(region = test.info$mRegion) ->
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
      select(-yield, -expectedYield, -expectedPrice, -cost, -expectedProfit, -shareWeight, -harvested.land) %>%
      mutate(region = SCENARIO.INFO$mRegion) ->
      outputData

    compareData %>%
      select(-land.allocation) %>%
      left_join(outputData, by=c("region", "year", "name")) %>%
      replace_na(list(land.allocation = 0)) %>%
      select(-scenario) ->
      outputData

    compareData <- compareData[c("name", "year", "region", "land.allocation")]

    expect_identical(dim(outputData), dim(compareData),
                     info = paste("Dimensions are not the same for reference land allocation"))

    expect_equivalent(round_df(outputData), round_df(compareData),
                      info = paste("reference land allocation doesn't match"))

  }

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
  if (test.info$mExpectationType == "Perfect") {

    # Get actual data
    path <- basepath
    file <- file.path(path, paste0("output_", test.info$mScenarioName, ".rds"))
    readRDS(file) %>%
      select(-land.allocation, -expectedYield, -expectedPrice) ->
      actualData

    # Get expected data
    file <- file.path(path, paste0("output_", test.info$mScenarioName, ".rds"))
    readRDS(file) %>%
      select(-land.allocation, -yield, -expectedPrice) %>%
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
  path <- basepath
  file <- file.path(path, paste0("output_", test.info$mScenarioName, ".rds"))
  outputData <- readRDS(file)

  # Aggregate to regions
  outputData %>%
    group_by(year) %>%
    summarize(land.allocation = sum(land.allocation)) ->
    outputData

  # Create data frame for comparison, where land cover equals base year level in all years
  outputData %>%
    # Filter for first year
    filter(year == min(YEARS[[scentype]])) %>%
    select(-year) %>%
    # Copy to all years
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS[[scentype]]), uniqueJoinField = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) %>%
    # Select appropriate columns
    select(year, land.allocation) ->
    compareData

  expect_equivalent(round_df(outputData), round_df(compareData),
                    info = paste("total land area changes over time"))

})

test_that("scenario land data can be retrieved", {
    ldl <- get_scenario_land_data(test.info)
    expect_true(is.list(ldl))
    for(ld in ldl) {
        expect_equal(ncol(ld), 6)
        expect_setequal(names(ld),
                        c('land.type', 'year', 'model', 'variable', 'region',
                          'scenario'))
    }
})

test_that("scenario land data is correct", {
  ldl <- get_scenario_land_data(test.info)

  # Filter model data for 2010 only
  ldl$Reference_Perfect %>%
    filter(year == 2010) %>%
    na.omit() ->
    modeldata

  comparedata <- c(0.000000, 331.982760,  43.296600,  85.523100,  94.471800, 3263.48926,
                   392.29993, 41.582560, 331.204490, 430.38414, 37.888590,   0.000000,
                   2469.70985,  14.629500,  34.11813 , 4.569620,  738.80971,
                   8.229700, 240.67582, 186.82978, 192.709000, 0.000000)

  expect_equal(sort(modeldata$model), sort(comparedata))
})

test_that("model results can be exported as CSV", {
    export_results(test.info)
    file <- file.path(basepath, paste0("output_", test.info$mScenarioName,
                                       ".csv"))
    expect_true(file.exists(file))
    ## TODO:  test the contents of the file.
})
