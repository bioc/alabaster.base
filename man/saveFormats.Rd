% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveFormats.R
\name{saveFormats}
\alias{saveFormats}
\alias{.saveDataFrameFormat}
\alias{saveDataFrameFormat}
\title{Choose the format for certain objects}
\usage{
saveDataFrameFormat(format)
}
\arguments{
\item{format}{String containing the format to use.
Tbe \code{"csv"}, \code{"csv.gz"} (default) or \code{"hdf5"}.
Alternatively \code{NULL}, to use the default format.}
}
\value{
If \code{format} is missing, a string containing the current format is returned, or \code{NULL} to use the default format.

If \code{format} is supplied, it is used to define the current format, and the \emph{previous} format is returned.
}
\description{
Alter the format used to save DataFrames in its \code{\link{stageObject}} methods.
}
\details{
\code{\link{stageObject}} methods will treat a \code{format=NULL} in the same manner as the default format.
The distinction exists to allow downstream applications to set their own defaults while still responding to user specification.
For example, an application can detect if the existing format is \code{NULL}, and if so, apply another default via \code{.saveDataFrameFormat}.
On the other hand, if the format is not \code{NULL}, this is presumably specified by the user explicitly and should be respected by the application.
}
\examples{
(old <- .saveDataFrameFormat())

.saveDataFrameFormat("hdf5")
.saveDataFrameFormat()

# Setting it back.
.saveDataFrameFormat(old)

}
\author{
Aaron Lun
}
