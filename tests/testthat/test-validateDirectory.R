# library(testthat); library(alabaster.base); source("test-validateDirectory.R")

library(S4Vectors)
ncols <- 123
df <- DataFrame(
    X = rep(LETTERS[1:3], length.out=ncols),
    Y = runif(ncols)
)
df$Z <- DataFrame(AA = sample(ncols))

test_that("validateDirectory works as expected", {
    tmp <- tempfile()
    dir.create(tmp, recursive=TRUE)

    # Mocking up a directory.
    saveObject(df, file.path(tmp, "foo"))
    dir.create(file.path(tmp, "whee"))
    saveObject(df, file.path(tmp, "whee", "stuff"))

    expect_error(validateDirectory(tmp), NA)
})

test_that("validateDirectory throws in the new world", {
    tmp <- tempfile()
    dir.create(tmp, recursive=TRUE)

    # Mocking up a directory.
    saveObject(df, file.path(tmp, "foo"))
    dir.create(file.path(tmp, "bar"))
    write(file=file.path(tmp, "bar", "OBJECT"), '[ "WHEEE" ]')

    expect_error(validateDirectory(tmp), "JSON object")
})
