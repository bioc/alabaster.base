# library(alabaster.base); library(testthat); source("test-saveObject.R")

test_that("saveObject fails for unknown classes", {
    setClass("MyClass", slots=c(x = "integer"))
    a <- new("MyClass", x = 1L)

    tmp <- tempfile()
    expect_error(saveObject(a, tmp), "MyClass")
})

test_that("saveObject fails for existing paths", {
    a <- S4Vectors::DataFrame(X = 1L)

    tmp <- tempfile()
    dir.create(tmp) 
    expect_error(saveObject(a, tmp), "existing path")
})
