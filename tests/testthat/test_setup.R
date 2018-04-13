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
