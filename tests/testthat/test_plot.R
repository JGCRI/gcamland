## Test plotting functions.

context("plot")

basepath <- file.path(tempdir(), "outputs")
test.info <- SCENARIO.INFO
test.info$mOutputDir <- basepath

## This ensures that the land allocation plotting works
test_that("Plotting functions work", {
  expect_error(plotLandAllocation(test.info), NA,
                 info='Failure to plot land')
  expect_error(plotRegionalLandAllocation(test.info), NA,
                 info='Failure to plot regional land')
})
