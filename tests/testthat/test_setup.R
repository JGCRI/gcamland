## Test setup functions.

context("setup")

basepath <- file.path(tempdir(), "outputs")
test.info <- SCENARIO.INFO
test.info$mOutputDir <- basepath

## This catches the case where the model output tests run because the
## directories already exist, but the setup code isn't actually creating them
## automatically for some reason.
test_that("Output directories are created automatically when model is run", {
    if(file.exists(basepath)) {
        unlink(basepath, recursive=TRUE)
    }
    expect_false(file.exists(basepath))
    run_model(test.info, integer(0))    # do the setup, but don't actually run
                                        # the model.
    dirs <- c(basepath, file.path(basepath, REQD.SUBDIRS))

    for (dir in dirs) {
        expect_true(dir.exists(dir),
                    info = paste("Failed to create ", dir))
    }

    unlink(basepath, recursive=TRUE)
})


test_that("Incompatible agData generates an error", {
    scentype <- test.info$mScenarioType
    if(scentype == 'Reference') {
        wrongtype <- 'Hindcast'
    }
    else {
        wrongtype <- 'Reference'
    }
    agData_bad <- ReadData_AgProd(test.info$mRegion, wrongtype, test.info$mSubRegion)
    expect_error(run_model(test.info, 4, agData=agData_bad))

    agData_worse <- ReadData_AgProd('European Free Trade Association', scentype, test.info$mSubRegion)
    expect_error(run_model(test.info, 4, agData=agData_worse))

})

test_that("Results are the same whether agData is read or passed in.", {
    r1 <- run_model(test.info, 1:4)
    agData <- ReadData_AgProd(test.info$mRegion, test.info$mScenarioType, test.info$mSubRegion)
    r2 <- run_model(test.info, 1:4, agData=agData)

    expect_equal(r1, r2)
})
