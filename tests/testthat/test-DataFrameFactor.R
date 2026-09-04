# library(testthat); library(alabaster.base); source("test-DataFrameFactor.R")

library(S4Vectors)

test_that("saving and reading of data frame factors work correctly", {
    tmp <- tempfile()
    dir.create(tmp)

    df <- DataFrame(A=sample(3, 100, replace=TRUE), B=sample(letters[1:3], 100, replace=TRUE))
    X <- DataFrameFactor(x=df)
    saveObject(X, file.path(tmp, "test1"))
    Y <- readObject(file.path(tmp, "test1"))
    expect_identical(X, Y)

    # Works with factor names.
    rownames(df) <- 1:100
    X <- DataFrameFactor(x=df)
    saveObject(X, file.path(tmp, "test2"))
    Y <- readDataFrameFactor(file.path(tmp, "test2"))
    expect_identical(X, Y)

    # Works with internal rownames.
    rownames(X@levels) <- paste0("INTERNAL_", seq_len(nrow(X@levels)))
    saveObject(X, file.path(tmp, "test3"))
    Y <- readDataFrameFactor(file.path(tmp, "test3"))
    expect_identical(X, Y)
})

test_that("validation of data frame factors catches non-unique levels", {
    tmp <- tempfile()

    df <- DataFrame(A=sample(3, 100, replace=TRUE), B=sample(letters[1:3], 100, replace=TRUE))
    X <- DataFrameFactor(x=df)
    saveObject(X, tmp)

    leveldir <- file.path(tmp, "levels")
    unlink(leveldir, recursive=TRUE)
    saveObject(rbind(levels(X), levels(X)), leveldir)
    expect_error(validateObject(tmp), "duplicated rows")
})
