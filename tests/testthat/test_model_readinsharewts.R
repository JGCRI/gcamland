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
