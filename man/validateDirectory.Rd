% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateDirectory.R
\name{validateDirectory}
\alias{validateDirectory}
\alias{checkValidDirectory}
\title{Validate a directory of objects}
\usage{
validateDirectory(dir, legacy = NULL, ...)
}
\arguments{
\item{dir}{String containing the path to a directory with subdirectories populated by \code{\link{saveObject}}.}

\item{legacy}{Logical scalar indicating whether to validate a directory with legacy objects (created by the old \code{stageObject}).
If \code{NULL}, this is auto-detected from the contents of \code{dir}.}

\item{...}{Further arguments to use when \code{legacy=TRUE}, for back-compatibility only.}
}
\value{
Character vector of the paths inside \code{dir} that were validated, invisibly.
If any validation failed, an error is raised.
}
\description{
Check whether each object in a directory is valid by calling \code{\link{validateObject}} on each non-nested object.
}
\details{
We assume that the process of validating an object will call \code{\link{validateObject}} on any nested objects.
This allows us to skip explicit calls to \code{\link{validateObject}} on each component of a complex object.
}
\examples{
# Mocking up an object:
library(S4Vectors)
ncols <- 123
df <- DataFrame(
    X = rep(LETTERS[1:3], length.out=ncols),
    Y = runif(ncols)
)
df$Z <- DataFrame(AA = sample(ncols))

# Mocking up the directory:
tmp <- tempfile()
dir.create(tmp, recursive=TRUE)
saveObject(df, file.path(tmp, "foo"))

# Checking that it's valid:
validateDirectory(tmp)

# Adding an invalid object:
dir.create(file.path(tmp, "bar"))
write(file=file.path(tmp, "bar", "OBJECT"), '[ "WHEEE" ]')
try(validateDirectory(tmp))

}
\author{
Aaron Lun
}
