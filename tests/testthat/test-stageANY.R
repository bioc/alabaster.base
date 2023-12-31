# This tests the staging hints in stageObject's ANY method.
# library(alabaster.base); library(testthat); source("test-stageANY.R")

test_that("stageObject redirects to an appropriate hint with name checks", {
    a <- matrix(runif(100), 10, 10)
    tmp <- tempfile()
    dir.create(tmp) 

    if (alabaster.base:::package.exists("alabaster.matrix")) {
        expect_error(info <- stageObject(a, tmp, "foo"), NA)
        expect_identical(as.character(info[["$schema"]]), "hdf5_dense_array/v1.json")
    } else {
        expect_error(expect_warning(stageObject(a, tmp, "foo"), "alabaster.matrix"), "signature 'matrix'")
    }
})

test_that("stageObject redirects to an appropriate hint via is()", {
    library(Matrix)
    x <- rsparsematrix(100, 10, 0.05)
    tmp <- tempfile()
    dir.create(tmp) 

    if (alabaster.base:::package.exists("alabaster.matrix")) {
        expect_error(info <- stageObject(x, tmp, "foo"), NA)
        expect_identical(as.character(info[["$schema"]]), "hdf5_sparse_matrix/v1.json")
    } else {
        expect_error(expect_warning(stageObject(x, tmp, "foo"), "alabaster.matrix"), "signature 'dgCMatrix'")
    }
})

test_that("stageObject fails for unknown classes", {
    setClass("MyClass", slots=c(x = "integer"))
    a <- new("MyClass", x = 1L)

    tmp <- tempfile()
    dir.create(tmp) 
    expect_error(stageObject(a, tmp, "foo"), "MyClass")

    tmp <- tempfile()
    expect_error(saveObject(a, tmp), "MyClass")
})

test_that("stageObject fails for existing paths", {
    a <- S4Vectors::DataFrame(X = 1L)

    tmp <- tempfile()
    dir.create(tmp) 
    stageObject(a, tmp, "foo")
    expect_error(stageObject(a, tmp, "foo"), "existing path")

    tmp <- tempfile()
    dir.create(tmp) 
    expect_error(info <- stageObject(a, tmp, "."), NA)
    expect_identical(info$path, "./simple.csv.gz")

    # Fails with the new world.
    tmp <- tempfile()
    dir.create(tmp) 
    expect_error(saveObject(a, tmp), "existing path")
})

test_that("stageObject fails for saving non-child objects in other object's subdirectories", {
    a <- S4Vectors::DataFrame(X = 1L)

    tmp <- tempfile()
    dir.create(tmp) 
    meta <- stageObject(a, tmp, "foo")
    writeMetadata(meta, tmp)

    expect_error(stageObject(a, tmp, "foo/bar"), "non-child object")
})

