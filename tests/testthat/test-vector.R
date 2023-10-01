# Test stageObject on simple vectors.
# library(testthat); library(alabaster.base); source("test-vector.R")

test_that("vectors work correctly without names", {
    tmp <- tempfile()
    dir.create(tmp, recursive=TRUE)

    input <- LETTERS
    meta <- stageObject(input, tmp, path="foo")
    expect_identical(meta$atomic_vector$type, "string")
    writeMetadata(meta, tmp)
    expect_identical(loadAtomicVector(meta, tmp), input)

    vals <- runif(25)
    meta <- stageObject(vals, tmp, path="bar")
    expect_identical(meta$atomic_vector$type, "number")
    writeMetadata(meta, tmp)
    expect_equal(loadAtomicVector(meta, tmp), vals)

    vals <- as.integer(rpois(99, 10))
    meta <- stageObject(vals, tmp, path="whee")
    expect_identical(meta$atomic_vector$type, "integer")
    writeMetadata(meta, tmp)
    expect_identical(loadAtomicVector(meta, tmp), vals)

    vals <- rbinom(1000, 1, 0.5) > 0
    meta <- stageObject(vals, tmp, path="stuff")
    expect_identical(meta$atomic_vector$type, "boolean")
    writeMetadata(meta, tmp)
    expect_identical(loadAtomicVector(meta, tmp), vals)

    vals <- c(Sys.Date(), Sys.Date() + 100, Sys.Date() - 100)
    meta <- stageObject(vals, tmp, path="blah")
    expect_identical(meta$atomic_vector$type, "string")
    expect_identical(meta$atomic_vector$format, "date")
    writeMetadata(meta, tmp)
    expect_identical(loadAtomicVector(meta, tmp), vals)

    vals <- c(Sys.time(), Sys.time() + 100, Sys.time() - 100)
    meta <- stageObject(vals, tmp, path="gunk")
    expect_identical(meta$atomic_vector$type, "string")
    expect_identical(meta$atomic_vector$format, "date-time")
    writeMetadata(meta, tmp)
    expect_true(all(abs(loadAtomicVector(meta, tmp) - vals) < 1)) # sub-second resolution on the strings.
})

test_that("vectors work correctly with names", {
    tmp <- tempfile()
    dir.create(tmp, recursive=TRUE)

    vals <- setNames(runif(26), LETTERS)
    meta <- stageObject(vals, tmp, path="bar")
    expect_identical(meta$atomic_vector$type, "number")
    writeMetadata(meta, tmp)
    expect_equal(loadAtomicVector(meta, tmp), vals)
})