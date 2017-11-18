# Test land area doesn't change over time

context("landarea")

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
  if(EXPECTATION.TYPE == "Linear") {
    expectations <- paste(EXPECTATION.TYPE, LINEAR.YEARS, sep="")
  } else if (EXPECTATION.TYPE == "Lagged") {
    expectations <- paste(EXPECTATION.TYPE, LAGGED.TAU, sep="")
  } else {
    expectations <- EXPECTATION.TYPE
  }
  file <- paste("../../outputs/landAllocation_", SCENARIO, "_", expectations, ".csv", sep="")

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
