% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_num.R
\name{format_num}
\alias{format_num}
\title{Format numbers}
\usage{
format_num(x)
}
\arguments{
\item{No}{arguments necessary}
}
\description{
This function will round numbers according to APA7 guidelines and express as scientific notation where necessary
}
\examples{
mat <- matrix(data = c(0.0025, 37.812, 3002.79, 0.021, 54.0422222, 0.000000546), ncol = 2, nrow = 3)
format_num(mat)
}
\keyword{round}
