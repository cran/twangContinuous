\name{get.weights}
\alias{get.weights}

\title{Extract propensity score weights}
\description{Extracts propensity score weights from a \code{ps.cont} object.}
\usage{get.weights(ps1, stop.method = "wcor", withSampW = TRUE)}

\arguments{
  \item{ps1}{a \code{ps.cont} object}
  \item{stop.method}{indicates which set of weights to retrieve from the \code{ps.cont} object}
  \item{withSampW}{Returns weights with sample weights multiplied in, if they were provided in the original \code{ps.cont} call.}
}

\value{a vector of weights}

\author{Donna L. Coffman}

\seealso{\code{\link{ps.cont}}}

\keyword{ utilities }
