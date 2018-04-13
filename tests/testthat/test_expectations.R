# Test that yield expectation calculations are working

context("yield expectations")

basepath <- file.path(tempdir(), "outputs")

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

