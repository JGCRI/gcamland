# Test shares for all nodes add to 1

context("shares")

library(readr)

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
  path <- normalizePath(file.path("./outputs/"))
  file <- paste0(path, "/landShares.csv")
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

test_that("share normalization works - case 1", {
  # First, define expected values
  expectedShare <- data.frame(name = c("Type1", "Type2", "Type3", "Type4"),
                             share = c(0.25, 0.25, 0.25, 0.25))

  # Next, calculate the values using gcamland
  # Create a data frame with unnormalized shares
  calculatedShare <- data.frame(name = c("Type1", "Type2", "Type3", "Type4"),
                                unnormalized.share = c(15, 15, 15, 15))

  # Calculate shares
  TEMP <- SectorUtils_normalizeShares(calculatedShare)
  calculatedShare <- TEMP$normalizedShares

  expect_equivalent(calculatedShare, expectedShare,
                    info = paste("SectorUtils_normalizeShares not producing expected values - case 1"))

})

test_that("share normalization works - case 2", {
  # First, define expected values
  expectedShare <- data.frame(name = c("Type1", "Type2", "Type3", "Type4"),
                              share = c(1, 0, 0, 0))

  # Next, calculate the values using gcamland
  # Create a data frame with unnormalized shares
  calculatedShare <- data.frame(name = c("Type1", "Type2", "Type3", "Type4"),
                                unnormalized.share = c(15, 0, 0, 0))

  # Calculate shares
  TEMP <- SectorUtils_normalizeShares(calculatedShare)
  calculatedShare <- TEMP$normalizedShares

  expect_equivalent(calculatedShare, expectedShare,
                    info = paste("SectorUtils_normalizeShares not producing expected values - case 2"))

})

