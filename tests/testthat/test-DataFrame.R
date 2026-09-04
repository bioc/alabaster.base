# library(testthat); library(alabaster.base); source("test-DataFrame.R")

library(S4Vectors)

test_that("DFs handle their column types correctly", {
    ncols <- 123
    df <- DataFrame(
        stuff = rep(LETTERS[1:3], length.out=ncols),
        blah = 0, # placeholder
        foo = seq_len(ncols),
        whee = as.numeric(10 + seq_len(ncols)),
        rabbit = 1,
        birthday = rep(Sys.Date(), ncols) - sample(100, ncols, replace=TRUE)
    )
    df$blah <- factor(df$stuff, LETTERS[10:1])
    df$rabbit <- factor(df$stuff, LETTERS[1:3], ordered=TRUE)

    tmp <- tempfile()
    saveObject(df, tmp)
    expect_identical(readObjectFile(tmp)$type, "data_frame")
    round2 <- readDataFrame(tmp)
    expect_identical(round2, df)
})

test_that("saving of weird objects within DFs works correctly", {
    tmp <- tempfile()
    dir.create(tmp)

    df <- DataFrame(A=sample(3, 100, replace=TRUE), B=sample(letters[1:3], 100, replace=TRUE))
    Y <- DataFrameFactor(x=df)

    input <- DataFrame(A=1:3, X=0, B=letters[1:3], Y=0)
    input$X <- df[1:3,] # nested DF.
    input$Y <- Y[1:3] # nested DFFactor.
    input$Z <- DataFrame(whee=4:6) # another nested DF.
    input$AA <- list("AA", "BB", "CC") # nested list

    tmp2 <- tempfile()
    saveObject(input, tmp2)
    expect_identical(readObjectFile(tmp2)$type, "data_frame")
    round2 <- readDataFrame(tmp2)
    expect_identical(round2, input)
})

test_that("saving DFs with row names works correctly", {
    df <- DataFrame(payload = 1:26)
    rownames(df) <- LETTERS

    tmp <- tempfile()
    saveObject(df, tmp)
    expect_identical(readObjectFile(tmp)$type, "data_frame")
    round2 <- readDataFrame(tmp)
    expect_identical(df, round2)
})

test_that("saving empty DFs works correctly", {
    df <- DataFrame(matrix(100, 100, 0))
    tmpa <- tempfile()
    saveObject(df, tmpa)
    round <- readDataFrame(tmpa)
    expect_identical(df, round)

    # Plus some row names.
    df2 <- df
    rownames(df2) <- sprintf("WHEE_%s", seq_len(nrow(df2)))
    tmpb <- tempfile()
    saveObject(df2, tmpb)
    round <- readDataFrame(tmpb)
    expect_identical(df2, round)
})

test_that("handling of NAs works correctly", {
    df <- DataFrame(
        a=c("A", "B", NA), 
        b=c("A", "NA", NA), 
        c=factor(c("A", "B", NA)), 
        d=factor(c("A", "B", "NA")),
        e=c(1L,2L,NA),
        f=c(1.5,2.5,NA),
        g=c(TRUE,FALSE,NA),
        h=1:3,
        i=c(TRUE,FALSE,TRUE),
        j=letters[1:3]
    )

    tmp <- tempfile()
    saveObject(df, tmp)

    fpath <- file.path(tmp, "basic_columns.h5")
    attrs <- rhdf5::h5readAttributes(fpath, "data_frame/data/2/codes")
    expect_identical(attrs[["missing-value-placeholder"]], 2L)
    attrs <- rhdf5::h5readAttributes(fpath, "data_frame/data/3/codes")
    expect_null(attrs[["missing-value-placeholder"]])

    round2 <- readDataFrame(tmp)
    expect_identical(df, round2)
})

test_that("handling of the integer minimum limit works correctly", {
    df <- DataFrame(foobar=c(1L,2L,NA))
    actual <- c(1, 2, -2^31)

    tmp2 <- tempfile()
    saveObject(df, tmp2)

    fpath <- file.path(tmp2, "basic_columns.h5")
    rhdf5::h5deleteAttribute(fpath, "data_frame/data/0", "missing-value-placeholder")
    round2 <- readDataFrame(tmp2)
    expect_identical(round2$foobar, actual)

    fhandle <- rhdf5::H5Fopen(fpath)
    dhandle <- rhdf5::H5Dopen(fhandle, "data_frame/data/0")
    rhdf5::h5writeAttribute(1, dhandle, "missing-value-placeholder", asScalar=TRUE)
    rhdf5::H5Dclose(dhandle)
    rhdf5::H5Fclose(fhandle)

    round <- readDataFrame(tmp2)
    expect_identical(round$foobar, c(NA, 2, -2^31))
})

test_that("handling of IEEE special values work correctly", {
    df <- DataFrame(specials=c(NaN, 1, 2.12345678, Inf, NA, -Inf))

    tmpa <- tempfile()
    saveObject(df, tmpa)
    expect_identical(readObjectFile(tmpa)$type, "data_frame")
    round2 <- readDataFrame(tmpa)
    expect_identical(round2, df)

    # Other columns are not affected.
    df2 <- cbind(df, normals=1:6) # check that quoting works
    df2 <- cbind(df2, more_normals=LETTERS[1:6])

    tmpb <- tempfile()
    saveObject(df2, tmpb)
    expect_identical(readObjectFile(tmpb)$type, "data_frame")
    round2 <- readDataFrame(tmpb)
    expect_identical(round2, df2)
})

test_that("readDataFrame works correctly with non-default placeholders", {
    df <- DataFrame(
        a=c(1L,2L,3L),
        b=c(1.5,2.5,3.5),
        c=c(1.5,2.5,NaN)
    )

    # Works in the new world.
    tmp <- tempfile()
    saveObject(df, tmp)

    fpath <- file.path(tmp, "basic_columns.h5")
    addMissingPlaceholderAttributeForHdf5(fpath, "data_frame/data/0", 1L)
    addMissingPlaceholderAttributeForHdf5(fpath, "data_frame/data/1", 2.5)
    addMissingPlaceholderAttributeForHdf5(fpath, "data_frame/data/2", NaN)

    round2 <- readDataFrame(tmp)
    expect_identical(round2$a, c(NA, 2L, 3L))
    expect_identical(round2$b, c(1.5, NA, 3.5))
    expect_identical(round2$c, c(1.5, 2.5, NA))
})

test_that("saveDataFrame works with extra mcols", {
    df <- DataFrame(A=sample(3, 100, replace=TRUE), B=sample(letters[1:3], 100, replace=TRUE))

    # Ignores it when the mcols have no columns.
    mcols(df) <- make_zero_col_DFrame(2)

    tmp <- tempfile()
    saveObject(df, tmp)
    round <- readDataFrame(tmp)
    expect_null(mcols(round))
    mcols(round) <- mcols(df)
    expect_identical(df, round)

    # Alright, adding some mcols.
    mcols(df)$stuff <- runif(ncol(df))
    mcols(df)$foo <- sample(LETTERS, ncol(df), replace=TRUE)
    metadata(df) <- list(WHEE="foo")

    tmp <- tempfile()
    saveObject(df, tmp)
    round <- readDataFrame(tmp)
    expect_identical(df, round)

    # Eliminates redundant row names.
    mc <- mcols(df)
    rownames(mc) <- c("C", "D")
    mcols(df, use.names=FALSE) <- mc

    tmp <- tempfile()
    saveObject(df, tmp)
    round <- readDataFrame(tmp)
    expect_identical(df, round)
})

test_that("DF staging preserves odd colnames", {
    tmp <- tempfile()
    dir.create(tmp)

    ncols <- 123
    df <- DataFrame(
        `foo bar` = seq_len(ncols),
        `rabbit+2+3/5` = as.numeric(10 + seq_len(ncols)),
        check.names=FALSE
    )

    tmp <- tempfile()
    saveObject(df, tmp)
    round <- readDataFrame(tmp)
    expect_identical(df, round)
})

test_that("DFs fails with duplicate or empty colnames", {
    ncols <- 123
    df <- DataFrame(
        foo = seq_len(ncols),
        rabbit = as.numeric(10 + seq_len(ncols)),
        rabbit = factor(sample(LETTERS, ncols, replace=TRUE)),
        check.names=FALSE
    )

    tmp2 <- tempfile()
    expect_error(saveObject(df, tmp2), "duplicate")

    df2 <- df
    colnames(df2)[2] <- ""
    tmp2 <- tempfile()
    expect_error(saveObject(df2, tmp2), "empty")
})

test_that("DFs handle POSIX times correctly", {
    tmp <- tempfile()
    dir.create(tmp, recursive=TRUE)

    df <- DataFrame(
        foo = as.POSIXct(c(123123, 124235235, 96546546)),
        bar = as.POSIXct(c(123123, 124235235, 96546546)) # TODO: should be POSIXlt, but see Bioconductor/S4Vectors#113
    )

    tmp <- tempfile()
    saveObject(df, tmp)
    round <- readDataFrame(tmp)
    expect_s3_class(round$foo, "Rfc3339")
    expect_s3_class(round$bar, "Rfc3339")
    expect_identical(df$foo, as.POSIXct(round$foo))
    expect_identical(df$bar, as.POSIXct(round$bar))

    # Rfc3339 objects are also correctly saved.
    tmp <- tempfile()
    saveObject(round, tmp)
    expect_identical(readDataFrame(tmp), round)
})

test_that("saving of arrays within DFs works correctly", {
    skip_if_not_installed("alabaster.matrix")

    input <- DataFrame(A=1:3, X=0, Y=0)
    input$X <- array(runif(3)) # 1D array
    input$Y <- cbind(runif(3)) # matrix with 1 column 
    input$Z <- cbind(V=runif(3), Z=rnorm(3)) # matrix with 2 columns.

    tmp <- tempfile()
    saveObject(input, tmp)
    roundtrip <- readObject(tmp)
    expect_s4_class(roundtrip$X, "ReloadedArray")
    roundtrip$X <- as.array(roundtrip$X)
    expect_s4_class(roundtrip$Y, "ReloadedArray")
    roundtrip$Y <- as.array(roundtrip$Y)
    expect_s4_class(roundtrip$Z, "ReloadedArray")
    roundtrip$Z <- as.array(roundtrip$Z)
    expect_identical(roundtrip, input)
})

test_that("staging of arrays continues to work with character matrices", {
    skip_if_not_installed("alabaster.matrix")

    input <- DataFrame(A=1:3, X=0, Y=0)
    input$X <- array(letters[1:3]) # 1D array
    input$Y <- cbind(LETTERS[1:3]) # matrix with 1 column 
    input$Z <- cbind(V=letters[4:6], Z=LETTERS[4:6]) # matrix with 2 columns.

    tmp <- tempfile()
    saveObject(input, tmp)
    roundtrip <- readObject(tmp)
    expect_s4_class(roundtrip$X, "ReloadedArray")
    roundtrip$X <- as.array(roundtrip$X)
    expect_s4_class(roundtrip$Y, "ReloadedArray")
    roundtrip$Y <- as.array(roundtrip$Y)
    expect_s4_class(roundtrip$Z, "ReloadedArray")
    roundtrip$Z <- as.array(roundtrip$Z)
    expect_identical(roundtrip, input)
})

test_that("saving works for base data.frames", {
    nrows <- 123
    df <- data.frame(
        stuff = rep(LETTERS[1:3], length.out=nrows),
        foo = seq_len(nrows),
        whee = as.numeric(10 + seq_len(nrows))
    )
    df$blah <- factor(df$stuff, LETTERS[10:1])
    df$rabbit <- factor(df$stuff, LETTERS[1:3], ordered=TRUE)

    tmp <- tempfile()
    saveObject(df, tmp)
    roundtrip <- readObject(tmp)
    expect_null(rownames(roundtrip))
    expect_identical(as.data.frame(roundtrip), df)

    # Respects row names.
    rownames(df) <- sprintf("GENE_%i", seq_len(nrows))
    tmp <- tempfile()
    saveObject(df, tmp)
    roundtrip <- readObject(tmp)
    expect_identical(as.data.frame(roundtrip), df)
})

test_that("saving works for data.frames containing package_version columns", {
    df <- data.frame(
        stuff = as.package_version(c("1.2", "2.3")),
        whee = as.package_version(c("2.3.4", "4.5.6"))
    )

    tmp <- tempfile()
    saveObject(df, tmp)
    roundtrip <- readObject(tmp)

    expect_identical(roundtrip$stuff, as.character(df$stuff))
    expect_identical(roundtrip$whee, as.character(df$whee))
})

test_that("DataFrame character columns work in VLS mode", {
    tmp <- tempfile()
    dir.create(tmp)

    has_vls <- function(file, column) {
        fhandle <- rhdf5::H5Fopen(file, "H5F_ACC_RDONLY")
        on.exit(rhdf5::H5Fclose(fhandle), add=TRUE, after=FALSE)
        dfhandle <- rhdf5::H5Gopen(fhandle, "data_frame")
        on.exit(rhdf5::H5Gclose(dfhandle), add=TRUE, after=FALSE)
        dhandle <- rhdf5::H5Gopen(dfhandle, "data")
        on.exit(rhdf5::H5Gclose(dhandle), add=TRUE, after=FALSE)
        precolhandle <- rhdf5::H5Oopen(dhandle, column)
        on.exit(rhdf5::H5Oclose(precolhandle), add=TRUE, after=FALSE)
        alabaster.base:::h5_read_attribute(precolhandle, "type") == "vls"
    }

    df <- DataFrame(x = runif(8), y = c("A", "BC", "DEFG", "HIJKL", "MNOP", "QRS", "TU", "V"), z = seq_len(8))
    saveObject(df, file.path(tmp, "basic"), DataFrame.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "basic")) 
    expect_identical(df, reloaded)
    expect_true(has_vls(file.path(tmp, 'basic', 'basic_columns.h5'), '1'))

    copy <- df
    copy$y[2] <- NA
    saveObject(copy, file.path(tmp, "with_missing"), DataFrame.character.vls=TRUE)
    reloaded <- readObject(file.path(tmp, "with_missing")) 
    expect_identical(copy, reloaded)
    expect_true(has_vls(file.path(tmp, 'with_missing', 'basic_columns.h5'), '1'))

    copy <- df
    copy$aa <- copy$y
    copy$aa[4] <- strrep("HIJKL", 100)
    saveObject(copy, file.path(tmp, "auto"), DataFrame.character.vls=NULL)
    reloaded <- readObject(file.path(tmp, "auto")) 
    expect_identical(copy, reloaded)
    expect_false(has_vls(file.path(tmp, 'auto', 'basic_columns.h5'), '1'))
    expect_true(has_vls(file.path(tmp, 'auto', 'basic_columns.h5'), '3'))
})
