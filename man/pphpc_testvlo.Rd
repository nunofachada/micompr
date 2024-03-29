% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pphpc_testvlo.R
\docType{data}
\name{pphpc_testvlo}
\alias{pphpc_testvlo}
\title{Data for testing variable length outputs}
\format{
A \code{\link{grpoutputs}} object containing simulation output data
from 6 runs of the PPHPC model, 3 runs from different implementations. The
model has six outputs, but the object contains a seventh output corresponding
to the concatenation of the six outputs
}
\source{
Runs are obtained from the NetLogo and Java (EX with 8 threads)
implementations of the PPHPC model available at
\url{https://github.com/nunofachada/pphpc}.
}
\usage{
pphpc_testvlo
}
\description{
A dataset with six outputs of different lengths for testing purposes only.
}
\keyword{internal}
