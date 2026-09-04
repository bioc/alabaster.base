# library(testthat); library(alabaster.base); source("test-list.R")

library(S4Vectors)

vals <- list(
    A = 1:5,
    B = (1:5) * 0.5, # numeric...
    C = LETTERS[1:5],
    D1 = factor(LETTERS[1:5], LETTERS),
    D2 = factor(LETTERS[1:5], rev(LETTERS), ordered=TRUE),
    E = c(Sys.Date(), Sys.Date() - 1, Sys.Date() + 1)
)

test_that("lists handle complex types correctly", {
    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp <- tempfile()
    saveObject(vals, tmp, list.format = "hdf5")
    expect_identical(readObject(tmp), vals)

    # Preserve non-alphabetical ordering.
    rvals <- rev(vals)
    tmp2 <- tempfile()
    saveObject(rvals, tmp2)
    expect_identical(readObject(tmp2), rvals)

    tmp2 <- tempfile()
    saveObject(rvals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), rvals)
})

test_that("S4 Lists can also be staged", {
    vals <- List(
        A = 1:5,
        B = (1:5) * 0.5,
        C = LETTERS[1:5]
    )

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), as.list(vals))

    tmp <- tempfile()
    saveObject(vals, tmp, list.format = "hdf5")
    expect_identical(readObject(tmp), as.list(vals))
})

test_that("names are properly supported", {
    vals <- list(
        X = setNames((1:5) * 0.5, LETTERS[1:5]),
        Y = setNames(factor(letters[6:15]), 1:10)
    )

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp <- tempfile()
    saveObject(vals, tmp, list.format = "hdf5")
    expect_identical(readObject(tmp), vals)
})

test_that("data.frames cause dispatch to external objects", {
    vals <- list(
        X = data.frame(X1 = runif(2), X2 = rnorm(2), X3 = LETTERS[1:2]),
        Y = list(data.frame(Y1 = runif(5), Y2 = letters[1:5], row.names=as.character(5:1))),
        Z = data.frame(Z1 = runif(5), row.names=c("alpha", "bravo", "charlie", "delta", "echo"))
    )

    tmp <- tempfile()
    saveObject(vals, tmp)
    roundtrip <- readObject(tmp)
    roundtrip$X <- as.data.frame(roundtrip$X)
    roundtrip$Y[[1]] <- as.data.frame(roundtrip$Y[[1]])
    roundtrip$Z <- as.data.frame(roundtrip$Z)
    expect_identical(roundtrip, vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    roundtrip <- readObject(tmp2)
    roundtrip$X <- as.data.frame(roundtrip$X)
    roundtrip$Y[[1]] <- as.data.frame(roundtrip$Y[[1]])
    roundtrip$Z <- as.data.frame(roundtrip$Z)
    expect_identical(roundtrip, vals)
})

test_that("unnamed lists are properly supported", {
    tmp <- tempfile()
    dir.create(tmp)

    vals <- list("A", 1.5, 2.3, list("C", list(DataFrame(X=1:10)), 3.5), (2:6)*1.5)

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), vals)
})

test_that("partially named or duplicate named lists fail", {
    tmp <- tempfile()
    expect_error(saveObject(list(1, A=2), tmp), "non-empty")
    tmp <- tempfile()
    expect_error(saveObject(list(A=1, A=2), tmp), "multiple instances of 'A'")
})

test_that("external references work correctly", {
    vals <- list(
        A = 1:5,
        B = list(
            C = DataFrame(X = 1:10),
            D = DataFrame(Y = 2:5)
        ),
        E = DataFrame(Z = runif(5))
    )

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), vals)
})

test_that("external references work correctly with lots of objects", {
    tmp <- tempfile()
    dir.create(tmp)

    # We use lots of objects to check that the sorting order is reproduced;
    # this is not always safe to assume, as the directories in other_content
    # are sorted by string and not number, e.g., 10 sorts before 2.
    vals <- list()
    for (i in 1:20) {
        vals[[i]] <- DataFrame(X = i)
    }

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), vals)
})

test_that("we handle lists with NAs", {
    vals <- list(
        A=NA, 
        B1=c(1,2,3,NA), 
        B2=c(4L, 5L, NA), 
        C=c("A", "B", NA), 
        D=factor(c("A", NA, "C"), c("a", "A", "b", "B", "c", "C"))
    )

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), vals)

    # More difficult NAs.
    vals$C <- c(vals$C, "NA")

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), vals)

    # Avoid unnecessary NA attributes for NaNs, unless they're mixed in with NAs.
    revals <- list(A=c(NaN, 1.0, 3, NaN), B=c(NaN, 1.0, NA, NaN))

    tmp <- tempfile()
    saveObject(revals, tmp)
    expect_identical(readObject(tmp), revals)

    tmp2 <- tempfile()
    saveObject(revals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), revals)
})

test_that("we handle lists with minimum integers", {
    vals <- list(A=c(1L,2L,3L,NA))

    tmp <- tempfile()
    saveObject(vals, tmp)

    fpath <- file.path(tmp, "list_contents.json.gz")
    x <- jsonlite::fromJSON(fpath, simplifyVector=FALSE)
    x$values[[1]]$values[4] <- -2^31
    write(jsonlite::toJSON(x, auto_unbox=TRUE), file=gzfile(fpath))

    roundtrip <- readObject(tmp)
    expect_equal(roundtrip$A, c(1,2,3,-2^31))

    # Works for HDF5.
    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")

    fpath <- file.path(tmp2, "list_contents.h5")
    rhdf5::h5deleteAttribute(fpath, "simple_list/data/0/data", "missing-value-placeholder")

    roundtrip <- readObject(tmp2)
    expect_equal(roundtrip$A, c(1,2,3,-2^31))
})

test_that("loaders work correctly from HDF5 with non-default placeholders", {
    vals <- list(a=c(1,2,3), b=c(4L, 5L, 6L), c=c(7, 8, NaN))

    tmp <- tempfile()
    saveObject(vals, tmp, list.format = "hdf5")

    fpath <- file.path(tmp, "list_contents.h5")
    addMissingPlaceholderAttributeForHdf5(fpath, "simple_list/data/0/data", 1)
    addMissingPlaceholderAttributeForHdf5(fpath, "simple_list/data/1/data", 5L)
    addMissingPlaceholderAttributeForHdf5(fpath, "simple_list/data/2/data", NaN)

    roundtrip <- readObject(tmp)
    expect_identical(roundtrip$a, c(NA, 2, 3))
    expect_identical(roundtrip$b, c(4L, NA, 6L))
    expect_identical(roundtrip$c, c(7, 8, NA))
})

test_that("we handle the various float specials", {
    vals <- list(XXX=c(1.2, Inf, 2.3, -Inf, 3.4, NaN, 4.5, NA))

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp), vals)
})

test_that("we handle lists with NULLs", {
    vals <- list(NULL, list(list(NULL), NULL))

    tmp <- tempfile()
    saveObject(vals, tmp)
    expect_identical(readObject(tmp), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    expect_identical(readObject(tmp2), vals)
})

test_that("we handle lists with times", {
    now <- as.POSIXct(round(Sys.time()), tz="")
    vals <- list(now, list(list(now + 10000), c(X=now + 400000, Y=now + 1000000)))
    to_posix <- function(x, fun) {
        if (is.list(x)) {
            lapply(x, to_posix, fun=fun)
        } else {
            fun(x)
        }
    }

    tmp <- tempfile()
    saveObject(vals, tmp)
    reloaded <- readObject(tmp)
    expect_s3_class(reloaded[[1]], "Rfc3339")
    expect_equal(to_posix(reloaded, as.POSIXct), vals)

    tmp2 <- tempfile()
    saveObject(vals, tmp2, list.format = "hdf5")
    reloaded <- readObject(tmp2)
    expect_equal(to_posix(reloaded, as.POSIXct), vals)

    # Works with POSIXlt objects, though these lose some precision when they go to POSIXct on back-conversion.
    now2 <- as.POSIXlt(now, tz="")
    vals2 <- list(now2, list(list(now2 + 10000), c(X=now2 + 400000, Y=now2 + 1000000)))

    tmp <- tempfile()
    saveObject(vals2, tmp)
    reloaded <- readObject(tmp)
    expect_equal(to_posix(reloaded, as.POSIXlt), vals2)

    tmp2 <- tempfile()
    saveObject(vals2, tmp2, list.format = "hdf5")
    reloaded <- readObject(tmp2)
    expect_equal(to_posix(reloaded, as.POSIXlt), vals2)
})

test_that("lists correctly distinguish between scalars and length-1 vectors", {
    ll <- list(
        A1 = 1L,
        A2 = I(2L),
        B1 = 1.5,
        B2 = I(2.5),
        C1 = "foo",
        C2 = I("bar"),
        D1 = TRUE,
        D2 = I(FALSE),
        E1 = Sys.Date(),
        E2 = I(Sys.Date()),
        F1 = as.Rfc3339(Sys.time()),
        F2 = I(as.Rfc3339(Sys.time()))
    )

    tmp <- tempfile()
    saveObject(ll, tmp)
    reloaded <- readObject(tmp)
    expect_equal(ll, reloaded)

    # Checking that the values are indeed scalar.
    y <- jsonlite::fromJSON(file.path(tmp, "list_contents.json.gz"), simplifyVector=FALSE)
    expect_type(y$values[[1]]$values, "integer")
    expect_type(y$values[[2]]$values, "list")
    expect_type(y$values[[3]]$values, "double")
    expect_type(y$values[[4]]$values, "list")
    expect_type(y$values[[5]]$values, "character")
    expect_type(y$values[[6]]$values, "list")
    expect_type(y$values[[7]]$values, "logical")
    expect_type(y$values[[8]]$values, "list")
    expect_type(y$values[[9]]$values, "character")
    expect_type(y$values[[10]]$values, "list")
    expect_type(y$values[[11]]$values, "character")
    expect_type(y$values[[12]]$values, "list")

    # Same for HDF5.
    tmp2 <- tempfile()
    saveObject(ll, tmp2, list.format="hdf5")
    reloaded2 <- readObject(tmp2)
    expect_equal(ll, reloaded2)

    # Checking that the values are indeed scalar.
    fhandle <- rhdf5::H5Fopen(file.path(tmp2, "list_contents.h5"))
    ghandle <- rhdf5::H5Gopen(fhandle, "simple_list")
    dhandle <- rhdf5::H5Gopen(ghandle, "data")
    peek_at_shape <- function(name) {
        ihandle <- rhdf5::H5Gopen(dhandle, name)
        on.exit(rhdf5::H5Gclose(ihandle), add=TRUE, after=FALSE)
        xhandle <- rhdf5::H5Dopen(ihandle, "data")
        on.exit(rhdf5::H5Dclose(xhandle), add=TRUE, after=FALSE)
        shandle <- rhdf5::H5Dget_space(xhandle)
        on.exit(rhdf5::H5Sclose(shandle), add=TRUE, after=FALSE)
        rhdf5::H5Sget_simple_extent_dims(shandle)$size
    }
    expect_identical(peek_at_shape("0"), integer(0))
    expect_identical(peek_at_shape("1"), 1L)
    expect_identical(peek_at_shape("2"), integer(0))
    expect_identical(peek_at_shape("3"), 1L)
    expect_identical(peek_at_shape("4"), integer(0))
    expect_identical(peek_at_shape("5"), 1L)
    expect_identical(peek_at_shape("6"), integer(0))
    expect_identical(peek_at_shape("7"), 1L)
    expect_identical(peek_at_shape("8"), integer(0))
    expect_identical(peek_at_shape("9"), 1L)
    expect_identical(peek_at_shape("10"), integer(0))
    expect_identical(peek_at_shape("11"), 1L)
})

test_that("lists convert package versions to strings", {
    vals <- list(foo = as.package_version(c("1.0", "1.0.0", "1.2.1")), bar = package_version("1.2.3.4"))

    tmp <- tempfile()
    dir.create(tmp)

    saveObject(vals, file.path(tmp, "gunk"))
    reloaded <- readObject(file.path(tmp, "gunk"))
    expect_identical(reloaded, lapply(vals, as.character))

    saveObject(vals, file.path(tmp, "gunk.h5"), list.format="hdf5")
    reloaded <- readObject(file.path(tmp, "gunk.h5"))
    expect_identical(reloaded, lapply(vals, as.character))
})

test_that("lists work in VLS mode", {
    tmp <- tempfile()
    dir.create(tmp)

    x <- list(u=runif(5), w=c("A", "BC", "DEFG"), x=TRUE, y=list(a=c("HIJKL", "MNOP", "QRS", "TU", "V")), z=2:10)
    saveObject(x, file.path(tmp, "basic"), list.format="hdf5", list.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "basic")) 
    expect_identical(x, reloaded)

    copy <- x
    copy$w[3] <- NA
    saveObject(copy, file.path(tmp, "with_missing"), list.format="hdf5", list.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "with_missing")) 
    expect_identical(copy, reloaded)

    copy <- x
    names(copy$w) <- seq_along(copy$w)
    saveObject(copy, file.path(tmp, "named"), list.format="hdf5", list.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "named")) 
    expect_identical(copy, reloaded)

    copy <- x
    copy$w <- c(copy$w, strrep("XXXXX", 100))
    saveObject(copy, file.path(tmp, "auto"), list.format="hdf5", list.character.vls=NULL)
    reloaded <- readObject(file.path(tmp, "auto")) 
    expect_identical(copy, reloaded)

    copy <- x
    copy$w <- "ABCDEFGHIJK"
    saveObject(copy, file.path(tmp, "scalar"), list.format="hdf5", list.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "scalar")) 
    expect_identical(copy, reloaded)

    copy <- x
    copy$w <- I("ABCDEFGHIJK")
    saveObject(copy, file.path(tmp, "not_scalar"), list.format="hdf5", list.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "not_scalar")) 
    expect_identical(copy, reloaded)
})
