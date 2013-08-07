\name{signedSquareRoots}
\alias{signedSquareRoots.profileModel}
\alias{signedSquareRoots}
\title{Get the signed square roots of the profiles in `profileModel'}
\description{
  Convert a \code{"profileModel"} object to contain the signed square
  roots of the profiles.
}
\usage{
\method{signedSquareRoots}{profileModel}(prof)
}
\arguments{
  \item{prof}{a \code{"profileModel"} object.}
}
\details{
  \code{signedSquareRoots} takes as input a \code{"profileModel"} object
  and  results to another \code{"profileModel"} object that contains the
  signed square roots of the profiled differences. The method only
  applies if \code{agreement} is set to \code{TRUE} in \code{prof$call}.
}
\value{
  an object of class \code{"profileModel"}.
}
\seealso{
  \code{\link{plot.profileModel}}.
}
\author{Ioannis Kosmidis <email: i.kosmidis@ucl.ac.uk>}
\keyword{manip}