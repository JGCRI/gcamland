# Test that yield expectation calculations are working

context("yield expectations")

basepath <- file.path(tempdir(), "outputs")

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

# Note: these tests ensure all calculations work in a simple case
test_that("yield expectation calculation works", {
  # First, create a land leaf
  testLeaf <- LandLeaf("test")
  testLeaf$mYield[1] <- 1
  testLeaf$mYield[2] <- 2
  testLeaf$mYield[3] <- 3
  testLeaf$mYield[4] <- 4
  testLeaf$mExpectedYield[1] <- 1
  testLeaf$mExpectedYield[2] <- 1
  testLeaf$mExpectedYield[3] <- 1.5

  # Next, create a temp scenarioInfo object with the right parameters
  tempScen <- ScenarioInfo()
  tempScen$mLaggedShareOld <- 0.5
  tempScen$mLinearYears <- 2

  # Now, calculate expectations using all methods
  perfectYield <- PerfectExpectation_calcExpectedYield(testLeaf, 4)
  laggedYield <- LaggedExpectation_calcExpectedYield(testLeaf, 4, tempScen)
  linearYield1 <- LinearExpectation_calcExpectedYield(testLeaf, 4, tempScen)

  # Compare to expectations
  expect_equivalent(perfectYield, 4,
                    info = paste("PerfectExpectation_calcExpectedYield not producing expected values"))
  expect_equivalent(laggedYield, 2.25,
                    info = paste("LaggedExpectation_calcExpectedYield not producing expected values"))
  expect_equivalent(linearYield1, 3,
                    info = paste("LinearExpectation_calcExpectedYield not producing expected values (1)"))

})

