# Test that yield expectation calculations are working

context("yield expectations")

basepath <- file.path(tempdir(), "outputs")

# Note: these tests ensure all calculations work in a simple case
test_that("yield expectation calculation works", {
  # First, create a land leaf
  testLeaf <- LandLeaf("test", TIME.PARAMS[[DEFAULT.SCENARIO.TYPE]]$FINAL_CALIBRATION_PERIOD,
                       max(PERIODS[[DEFAULT.SCENARIO.TYPE]]))
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

test_that('lagged expectation function is equivalent to loop', {
  year <- c(1971, 1972, 1973)
  qty <- c(1.0, 2.0, 3.0)
  pricetable <- data.frame(year=year, qty=qty)
  ntbl <- nrow(pricetable)
  loopcalc <- function(t, alpha) {
    i <- year[1]
    y <- qty[1]
    maxyear <- year[ntbl]
    while(i <= t) {
      if(i <= maxyear) {
        y <- alpha*y + (1-alpha) * qty[year==i]
      }
      else {
        y <- alpha*y + (1-alpha) * qty[ntbl]
      }
      i <- i+1
    }
    y
  }

  ## for alpha==0, and years in the table, we should just get back the original value
  for(t in year) {
    expect_equal(calc_lagged_expectation(t, 0, pricetable, 'qty'),
                 qty[year==t],
                 info=paste('t=',t))
  }

  ## Compare the package implementation with the loop version above.
  for(alpha in c(0.1, 0.5, 0.9)) {
    for(t in c(1900, year, 1975)) {
      expect_equal(calc_lagged_expectation(t, alpha, pricetable, 'qty'),
                   loopcalc(t, alpha),
                   info=paste('alpha=',alpha,' t=',t))
    }
  }
})
