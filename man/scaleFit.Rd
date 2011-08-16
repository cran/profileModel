\name{scaleFit}
\alias{scaleFit}
\title{Scales the estimates of a fitted in scale-invariant fits.}
\description{
Scales the estimates of a fitted object. Each column
of the model matrix is divided by the maximum absolute value of the
column and the model is re-fitted using the new model matrix.
}
\usage{
scaleFit(fitted)
}
\arguments{
  \item{fitted}{a \code{\link{glm}}-like fitted object with \bold{linear
      predictor} that supports the method \code{\link{model.matrix}}.}
}
\details{
  \code{scale.fit} should only be used when the inferences using the
  model that \code{fitted} corresponds to, are invariant on the scale
  of the parameters (e.g. a GLM). In other words, \code{scaleFit} should
  only affect the estimated parameters and not the fit and the
  inferences made.
}
\value{
  An object of the same class as \code{fitted}.
}
\section{Side Effects}{
  Causes creation of the matrix \code{.the.scaled.} and the vector
  \code{.the.offs.} in the global environment if they do not already
  exist, otherwise their values are updated. This is done in order to
  ensure that the fitting procedure that resulted \code{fitted} will
  find these quantities.
}
\examples{
## Begin Example
y <- rpois(100,1)
x <- rnorm(100,0,0.2)*1e+6
m1 <- glm(y ~ -1 + x, family = poisson)
m2 <- scaleFit(m1)
## m1 and m2 are the same fits but on a different scale
sum(abs(m1$fitted-m2$fitted))
## End Example
}
\author{Ioannis Kosmidis <email: ioannis@stats.ucl.ac.uk>}
\keyword{models}
