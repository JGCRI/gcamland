# Test that FAO yield data is read and processed correctly
# This is the data used for hindcasts, as well as for yield
# information prior to 1975

context("yields")

test_that("historical yields are calculated correctly", {
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

  # Get Comparison Data
  compareData <- read_csv("./comparison-data/FAO_yields_R_C_Y.csv", col_types = "ccid")
  compareData <- compareData[with(compareData, order(region, GCAM_commodity, year)), ]

  # Calculate data from the model
  modelData <- get_historic_yields()
  modelData <- modelData[with(modelData, order(region, GCAM_commodity, year)), ]

  expect_equal(compareData$yield, modelData$yield, tolerance = 0.01,
                    info = paste("Historical yields aren't calculated correctly"))

})

test_that("yield ratios are 1 for base year", {
  # First, get model calculated values
  get_historic_yield_ratios() %>%
    filter(year == min(YEARS$Hindcast)) ->
    base_ratio

  # Next, define expected values
  compare_ratio <- data.frame(region = rep("USA", 10),
                              GCAM_commodity = c("Corn", "FiberCrop", "MiscCrop", "OilCrop",
                                                 "OtherGrain", "PalmFruit", "Rice", "Root_Tuber", "SugarCrop", "Wheat"),
                              year = rep(as.integer(1975), 10),
                              yieldRatio = rep(1.0, 10))
  compare_ratio$region <- as.character(compare_ratio$region)
  compare_ratio$GCAM_commodity <- as.character(compare_ratio$GCAM_commodity)

  expect_equivalent(base_ratio, compare_ratio,
                    info = paste("Yield ratios are not 1 in the base year"))

})

