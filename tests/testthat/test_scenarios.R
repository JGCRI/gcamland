## Test alternate scenario types.
## TODO:  Add some more meaningful tests here, besides just failure to crash

context("Alternate scenario types")


basepath <- file.path(tempdir(), "outputs")
runperiods <- 1:4

test_that('Alternate scenario types run successfully', {
    types <- c("Reference", "Hindcast")
    expectations <- c("Perfect", "Lagged", "Linear", "LaggedCurr")

    for (type in types) {
        for (expectation in expectations) {
            if(type=="Reference" && expectation=="Perfect")
                next                    # already tested in test_model_output.R
            testscen <- ScenarioInfo(aExpectationType = expectation,
                                     aLinearYears = 2,
                                     aLaggedShareOld = 0.5,
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

