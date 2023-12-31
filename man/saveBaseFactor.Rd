% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveBaseFactor.R
\name{saveBaseFactor}
\alias{saveBaseFactor}
\alias{saveObject,factor-method}
\alias{stageObject,factor-method}
\title{Save a factor to disk}
\usage{
\S4method{saveObject}{factor}(x, path, ...)
}
\arguments{
\item{x}{A factor.}

\item{path}{String containing the path to a directory in which to save \code{x}.}

\item{...}{Further arguments that are ignored.}
}
\value{
\code{x} is saved inside \code{path}.
\code{NULL} is invisibly returned.
}
\description{
Pretty much as it says, let's save a base R \link{factor} to an on-disk representation.
}
\examples{
tmp <- tempfile()
saveObject(factor(1:10, 1:30), tmp)
list.files(tmp, recursive=TRUE)

}
\seealso{
\code{\link{readBaseFactor}}, to read the files back into the session.
}
\author{
Aaron Lun
}
