% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/left.R
\name{left}
\alias{left}
\alias{right}
\alias{right}
\title{Left- or right-most element(s)}
\usage{
left(x, n = 1)

right(x, n = 1)
}
\arguments{
\item{x}{a vector that you wish to extract from.}

\item{n}{an optional integer indicating the number of
elements you wish to extract from \code{x} starting from the
left-most (\code{left}) or right-most (\code{right}) element.}
}
\value{
A character vector containing the \code{n} left-most
(\code{left}) or right-most (\code{right}) elements of
\code{x}.
}
\description{
Extract the \code{n} left-most (\code{left}) or right-most
(\code{right}) elements of \code{x}.
}
\examples{

  x <- c("abcde", "12345")
  left(x)
  left(x, n = 3)
  right(x)
  right(x, n = 2)
}
\seealso{
\code{\link{substr}}, \code{\link{nchar}}
}
\author{
Gerhard Nachtmann \email{kpm.nachtmann@gmail.com}
}
\keyword{left}
\keyword{right}
