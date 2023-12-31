% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveAtomicVector.R
\name{saveAtomicVector}
\alias{saveAtomicVector}
\alias{stageObject,integer-method}
\alias{stageObject,numeric-method}
\alias{stageObject,logical-method}
\alias{stageObject,character-method}
\alias{stageObject,double-method}
\alias{stageObject,POSIXct-method}
\alias{stageObject,POSIXlt-method}
\alias{stageObject,Date-method}
\alias{saveObject,integer-method}
\alias{saveObject,character-method}
\alias{saveObject,logical-method}
\alias{saveObject,double-method}
\alias{saveObject,numeric-method}
\alias{saveObject,Date-method}
\alias{saveObject,POSIXlt-method}
\alias{saveObject,POSIXct-method}
\title{Save atomic vectors to disk}
\usage{
\S4method{saveObject}{integer}(x, path, ...)

\S4method{saveObject}{character}(x, path, ...)

\S4method{saveObject}{logical}(x, path, ...)

\S4method{saveObject}{double}(x, path, ...)

\S4method{saveObject}{numeric}(x, path, ...)

\S4method{saveObject}{Date}(x, path, ...)

\S4method{saveObject}{POSIXlt}(x, path, ...)

\S4method{saveObject}{POSIXct}(x, path, ...)
}
\arguments{
\item{x}{Any of the atomic vector types, or \link{Date} objects, or time objects, e.g., \link{POSIXct}.}

\item{path}{String containing the path to a directory in which to save \code{x}.}

\item{...}{Further arguments that are ignored.}
}
\value{
\code{x} is saved inside \code{path}.
\code{NULL} is invisibly returned.
}
\description{
Save vectors containing atomic elements (or values that can be cast as such, e.g., dates and times) to an on-disk representation.
}
\examples{
tmp <- tempfile()
dir.create(tmp)
saveObject(LETTERS, file.path(tmp, "foo"))
saveObject(setNames(runif(26), letters), file.path(tmp, "bar"))
list.files(tmp, recursive=TRUE)

}
\seealso{
\code{\link{readAtomicVector}}, to read the files back into the session.
}
\author{
Aaron Lun
}
