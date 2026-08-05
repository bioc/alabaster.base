# library(testthat); library(alabaster.base); source("test-getSaveEnvironment.R")

test_that("formatSaveEnvironment works correctly", {
    env <- formatSaveEnvironment()
    expect_identical(env$type, "R")
    expect_type(env$platform, "character")
    expect_type(env$version, "character")
    expect_identical(env$packages$alabaster.base, as.character(packageVersion("alabaster.base")))

    expect_identical(env$packages$jsonlite, as.character(packageVersion("jsonlite")))
})

test_that("getSaveEnvironment works correctly inside readObject", {
    expect_null(getSaveEnvironment())

    info <- formatSaveEnvironment()
    tmp <- tempfile()
    dir.create(tmp)
    saveObjectFile(tmp, "foobar")
    write(
        file=file.path(tmp, "_environment.json"),
        jsonlite::toJSON(info, pretty=4, auto_unbox=TRUE)
    )

    registerReadObjectFunction("foobar", function(x, metadata, ...) {
        getSaveEnvironment()
    })
    on.exit(registerReadObjectFunction("foobar", NULL), add=TRUE, after=FALSE)

    expect_identical(readObject(tmp)$type, "R")
    expect_identical(altReadObject(tmp)$type, "R")
    expect_null(getSaveEnvironment()) # correctly unset after all calls have finished.

    unlink(file.path(tmp, "_environment.json"))
    expect_null(readObject(tmp))

    write(file=file.path(tmp, "_environment.json"), "foo")
    expect_warning(readObject(tmp), "failed to read")
})

library(S4Vectors)
test_that("environment information is saved correctly", {
    df <- DataFrame(A=LETTERS)
    df$B <- DataFrame(B=runif(nrow(df)))

    tmp <- tempfile()
    saveObject(df, tmp)
    expect_true(file.exists(alabaster.base:::.get_environment_path(tmp)))
    expect_true(file.exists(file.path(tmp, "other_columns", "1")))
    expect_false(file.exists(alabaster.base:::.get_environment_path(file.path(tmp, "other_columns", "1"))))

    # Child inherits the parent environment. 
    oldfun <- readDataFrame
    registerReadObjectFunction("data_frame", function(...) {
        expect_identical(getSaveEnvironment()$type, "R")
        X <- oldfun(...)
        X$foo <- 2
        X
    }, existing="new")
    on.exit(registerReadObjectFunction("data_frame", oldfun, existing="new"), add=TRUE, after=FALSE)

    expect_error(out <- readObject(tmp), NA)
    expect_identical(out$foo, rep(2, nrow(out)))
    expect_identical(out$B$foo, rep(2, nrow(out)))
})

test_that("we can disable saving of environment information", {
    old <- recordSaveEnvironment(FALSE)
    on.exit(recordSaveEnvironment(old), add=TRUE, after=FALSE)

    df <- DataFrame(A=LETTERS)
    df$B <- DataFrame(B=runif(nrow(df)))

    tmp <- tempfile()
    saveObject(df, tmp)
    expect_false(file.exists(alabaster.base:::.get_environment_path(tmp)))
    expect_true(file.exists(file.path(tmp, "other_columns", "1")))
    expect_false(file.exists(alabaster.base:::.get_environment_path(file.path(tmp, "other_columns", "1"))))

    oldfun <- readDataFrame
    registerReadObjectFunction("data_frame", function(...) {
        expect_null(getSaveEnvironment())
        X <- oldfun(...)
        X$bar <- 3
        X
    }, existing="new")
    on.exit(registerReadObjectFunction("data_frame", oldfun, existing="new"), add=TRUE, after=FALSE)

    expect_error(out <- readObject(tmp), NA)
    expect_identical(out$bar, rep(3, nrow(out)))
    expect_identical(out$B$bar, rep(3, nrow(out)))
})
