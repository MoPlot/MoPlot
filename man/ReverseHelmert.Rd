% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReverseHelmert.R
\name{ReverseHelmert}
\alias{ReverseHelmert}
\title{Generate Reverse Helmert Contrast Matrix}
\usage{
ReverseHelmert(n)
}
\arguments{
\item{n}{Integer. The number of levels of a factor (must be >= 2).}
}
\value{
A numeric matrix
}
\description{
Creates a reverse Helmert contrast matrix of size \code{n x (n - 1)}.
For internal usage
}
\keyword{internal}
