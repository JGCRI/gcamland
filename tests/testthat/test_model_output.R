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
  compareData <- read_csv("./comparison-data/HistLandAllocation.csv")
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
    select(-yield, -expectedPrice, -expectedYield) %>%
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
    separate(name, into=c("type", "AEZ"), sep="AEZ") %>%
    group_by(AEZ, year) %>%
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
    select(AEZ, year, land.allocation) ->
    compareData

  expect_equivalent(round_df(outputData), round_df(compareData),
                    info = paste("total land area changes over time"))

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

  if (test.info$mScenarioName == "Reference_Perfect") {

    # Get comparison data
    compareData <- read_csv("./comparison-data/LandAllocation_Reference_Perfect.csv", skip = 1)
    compareData %>%
      filter(region == test.info$mRegion, year %in% YEARS[[scentype]]) ->
      compareData

    # Look for output data in outputs under top level
    # (as this code will be run in tests/testthat)
    path <- basepath
    file <- file.path(path, paste0("output_", test.info$mScenarioName, ".rds"))
    readRDS(file) %>%
      select(-yield, -expectedYield, -expectedPrice) %>%
      mutate(region = test.info$mRegion) ->
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

test_that("log-likelihood is calculated correctly", {
    ## Comparison data
    ll_ref <-
        structure(list(xi = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                       ll_ = c(-107.815639492531, -107.524496758494, -108.420661612172,
                       -109.539941147891, -110.682083919338, -111.790184816942, -112.847577781434,
                       -113.85112936962, -114.802588313354,
                       -115.70539810398)),
                  .Names = c("xi", "ll_"), class = c("tbl_df", "tbl", "data.frame"))


    histland <- get_historical_land_data(test.info$mRegion)
    modland <- get_scenario_land_data(list(test.info))[[test.info$mScenarioName]]
    test.info <- calc_post(test.info, histland, modland,
                           lpdf=get_lpdf(1), lprior=uniform_prior)

    ll_out <- test.info$mLogPost


    ## Not sure why the data frames refuse to compare as equal, when the
    ## individual data columns do.  Whatever.
    expect_equal(ll_out$xi, ll_ref$xi)
    expect_equal(ll_out$lp_, ll_ref$ll_)
})

test_that("posterior pdf table is calculated correctly", {
    ## This test possibly obviates the need for the log-likelihood test.
    gt_ref <- structure(list(xi = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
                1), lp_ = c(-107.815639492531, -107.524496758494, -108.420661612172,
                -109.539941147891, -110.682083919338, -111.790184816942, -112.847577781434,
                -113.85112936962, -114.802588313354, -115.70539810398), expectation.type =
                c("Perfect", "Perfect", "Perfect", "Perfect", "Perfect", "Perfect", "Perfect",
                "Perfect", "Perfect", "Perfect"), share.old = c(NA, NA, NA, NA, NA, NA, NA, NA,
                NA, NA), linear.years = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                logit.agforest = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), logit.afnonpast =
                c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), logit.crop = c(NA, NA, NA, NA, NA,
                NA, NA, NA, NA, NA), region = c("USA", "USA", "USA", "USA", "USA", "USA", "USA",
                "USA", "USA", "USA")), class = c("tbl_df", "tbl", "data.frame" ), row.names =
                c(NA, -10L), .Names = c("xi", "lp_", "expectation.type", "share.old",
                "linear.years", "logit.agforest", "logit.afnonpast", "logit.crop", "region"))

    gt <- grand_table(list(test.info))

    expect_equal(gt$xi, gt_ref$xi)
    expect_equal(gt$lp_, gt_ref$lp_)

})
