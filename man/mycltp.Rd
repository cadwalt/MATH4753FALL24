% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltp.R
\name{mycltp}
\alias{mycltp}
\title{CLT Poisson}
\usage{
mycltp(n, iter, lambda = 10, ...)
}
\arguments{
\item{n}{sample size}

\item{iter}{number of iterations, corresponds to columns}

\item{lambda}{lambda}

\item{...}{}
}
\value{
a discrete barplot showing the poisson distribution
}
\description{
CLT Poisson
}
\examples{
mycltp(n=2,iter=10000, lambda=4)
}
