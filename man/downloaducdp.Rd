% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloaducdp.R
\name{downloaducdp}
\alias{downloaducdp}
\title{Helper function (internal) to download UCDP datasets}
\usage{
downloaducdp(dataset)
}
\arguments{
\item{dataset}{Which of four datasets to download: can be acd, ged, osv, nsv}
}
\value{
The dataset as an object (dataframe) in the global environment
}
\description{
Helper function (internal) to download UCDP datasets
}
\examples{
\dontrun{
  downloaducdp("acd")
}
}
\keyword{internal}
