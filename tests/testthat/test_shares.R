# Test shares for all nodes add to 1

context("shares")

library(readr)

basepath <- file.path(tempdir(), "outputs")


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

