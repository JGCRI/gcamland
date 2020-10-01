## Test alternate scenario types.
## TODO:  Add some more meaningful tests here, besides just failure to crash

context("Alternate scenario types")


basepath <- file.path(tempdir(), "outputs")
runperiods <- 1:4

test_that('Alternate scenario types run successfully', {
    types <- c("Reference", "Hindcast")
    expectations <- c("Perfect", "Adaptive", "Linear", "HybridPerfectAdaptive")

    for (type in types) {
        for (expectation in expectations) {
            if(type=="Reference" && expectation=="Perfect")
                next                    # already tested in test_model_output.R
            testscen <- ScenarioInfo(aExpectationType = expectation,
                                     aLinearYears1 = 2,
                                     aLinearYears2 = 2,
                                     aLinearYears3 = 2,
                                     aLaggedShareOld1 = 0.5,
                                     aLaggedShareOld2 = 0.5,
                                     aLaggedShareOld3 = 0.5,
                                     aLogitUseDefault = TRUE,
                                     aScenarioType = type,
                                     aScenarioName = paste(type, expectation, sep='-'),
                                     aFileName = paste(type, expectation, sep='-'),
                                     aOutputDir = basepath)
            expect_message(run_model(testscen, runperiods),
                           info=paste('Failure in scenario type=', type,
                           'expectation= ', expectation))
        }
    }
})

test_that('Sub region runs successfully', {
  expectations <- c("Perfect")

  for (expectation in expectations) {
    testscen <- SRB.SCENARIO.INFO
    testscen$mExpectationType <- expectation
    testscen$mLinearYears1 <- 2
    testscen$mLinearYears2 <- 2
    testscen$mLinearYears3 <- 2
    testscen$mLaggedShareOld1 <- 0.5
    testscen$mLaggedShareOld2 <- 0.5
    testscen$mLaggedShareOld3 <- 0.5

    expect_message(run_model(testscen, runperiods),
                   info=paste('Failure in scenario type=', type,
                              'expectation= ', expectation,
                              'subregion= ', subregion))
  }
})
