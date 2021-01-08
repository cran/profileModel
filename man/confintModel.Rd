\name{confintModel}
\alias{confintModel}
\alias{profSmooth.profileModel}
\alias{profConfint.profileModel}
\alias{profZoom.profileModel}
\alias{profSmooth}
\alias{profConfint}
\alias{profZoom}
\title{Confidence intervals for model parameters}
\description{
  Computes confidence intervals for one or more parameters in a fitted
  model, based on the profiles of a specified objective.
}
\usage{
confintModel(fitted, quantile = qchisq(0.95, 1), verbose = TRUE,
             endpoint.tolerance = 1e-3, max.zoom = 100,
             zero.bound = 1e-08, stepsize = 0.5, stdn = 5,
             gridsize = 20, scale = FALSE, which = 1:length(coef(fitted)),
             objective = stop("'objective' is missing."),
             agreement = TRUE, method = "smooth",
             n.interpolations = 100, ...)


\method{profConfint}{profileModel}(prof, method = "smooth",
endpoint.tolerance = 1e-3, max.zoom = 100, n.interpolations = 100,
verbose = FALSE, ...)


\method{profZoom}{profileModel}(prof, max.zoom = 100, endpoint.tolerance = 1e-03,
        verbose = FALSE, ...)

\method{profSmooth}{profileModel}(prof, n.interpolations = 100, ...)
}
\arguments{
  \item{fitted}{a \code{\link{glm}}-like fitted object with \bold{linear
      predictor} (see Details of \code{\link{profileModel}} for the methods
    that have to be supported by \code{fitted}).}
  \item{prof}{a \code{"profileModel"} object with non-\code{NULL}
  quantile.}  \item{quantile}{The quantile to be used for the
  construction of the confidence intervals. The default is qchisq(0.95,
  1).}
  \item{verbose}{if \code{TRUE} (default) progress indicators are
  printed during the progress of calculating the confidence intervals.}
  \item{endpoint.tolerance}{the tolerance on the absolute difference of
  the value of the profile at the endpoints from the quantile used. Only
  relevant when confidence intervals are constructed via the "profZoom"
  method (see Details).}
  \item{max.zoom}{the maximum number of iterations that the binary
  search algorithm will take towards the achievement of
  \code{endpoint.tolerance}.}
  \item{zero.bound}{same as in \code{\link{profileModel}}.}
  \item{stepsize}{same as in \code{\link{profileModel}}.}
  \item{stdn}{same as in \code{\link{profileModel}}.}
  \item{gridsize}{same as in \code{\link{profileModel}}.}
  \item{scale}{same as in \code{\link{profileModel}}.}  \item{which}{for
  which parameters should the confidence intervals be calculated?}
  \item{objective}{same as in \code{\link{profileModel}}.}
  \item{agreement}{same as in \code{\link{profileModel}}.}
  \item{method}{the method to be used for the calculation of the
  confidence intervals. Possible values are "smooth", which is the
  default, and "zoom" (see Details).}  \item{n.interpolations}{if
  \code{method="smooth"} the number of interpolations to be used for
  spline smoothing. The default is 100.}  \item{\dots}{for
  \code{confintModel}, further arguments passed to the specified
  objective. For the methods \code{profZoom}, \code{profSmooth} and
  \code{profConfint}, further arguments passed to or from other
  functions.}
}
\details{
  The confidence intervals methods refer to convex
  objectives. Objectives that result in disjoint confidence regions are
  not currently supported.

  When the profile object is available and was called with the
  specification of the appropriate quantile then
  \code{profConfint} should be used. \code{confintModel}
  applies directly to the fitted model and calls \code{profileModel}.

  When \code{method="zoom"} the \code{profZoom} method is applied to the
  \code{"profileModel"} object.   When \code{method="smooth"} the
  \code{profSmooth} method is applied to the \code{"profileModel"} object.

  The \code{profZoom} method relies on a binary search and can find the
  endpoints of the confidence intervals for a pre-specified tolerance
  for the absolute difference of the value of the profile at each endpoint
  from the quantile used. It is a computationally intensive method and is
  useful in cases where the estimate is infinite and in coverage related
  simulations.

  The \code{profSmooth} method, fits a smoothing spline on the points
  specified by the \code{"profileModel"} object and then interpolates the
  endpoints of the confidence intervals at the specified
  \code{quantile}. It is much faster than \code{profZoom} and can safely
  be used in cases where the profiled objective is nearly quadratic in
  shape, but could be misleading otherwise.

  Both methods can report an infinite endpoint. The detection is based on the
  \code{intersects} component of the \code{"profileModel"} object.

  \code{profConfint} is a wrapper method that collects the capabilities
  of \code{profZoom} and \code{profSmooth}.

  \code{profSmooth}, \code{profZoom} and \code{profConfint} use the
  quantile that comes with the \code{"profileModel"} object \code{prof}.
}
\value{
  All the functions return a matrix with columns the endpoints of the
  confidence intervals for the specified (or profiled) parameters.

  Additionally, \code{confintModel} and \code{profConfint} have
  an attribute carrying the name of the fitted object and the name of the
  \code{"profileModel"} object, respectively.
}
\author{Ioannis Kosmidis <email: ioannis.kosmidis@warwick.ac.uk>}
\seealso{\code{\link{confint}}, \code{\link{profileModel}}.}
\examples{
\dontrun{
## Begin Example: quasi likelihood estimation.
## Incidence of leaf-blotch on barley
## McCullagh and Nelder (1989), pp. 328--332
library(gnm)
data(barley)
logitModel <- glm(y ~ site + variety, family = wedderburn, data = barley)
profQuasi <- profileModel(logitModel, objective = "ordinaryDeviance",
                          quantile=qchisq(0.95, 1),
                          which = paste("variety",c(2:9,"X"),sep=""))
# very accurate confidence intervals (with endpoints accurate up to 10
# decimals) for the variety parameters using profConfint with
# method="zoom":
c1 <- profConfint(profQuasi, endpoint.tolerance = 1e-10, maxit = 100,
                  method="zoom" )
# confidence intervals using smoothing:
c2 <- profConfint(profQuasi, method="smooth" )
# c2 has accurate endpoints at least up to four decimals
# this is because of the quadratic shape of the profiles
plot(profQuasi, cis = c1)
plot(profQuasi, cis = c1, signed = TRUE, print.grid.points = TRUE)
# pairs plot
pairs(profQuasi)
# Notice the direction of the pairs plots. The fact that the
# correlations among the estimates are 1/2 is clear.

# profiling using the Rao score statistic
# This can be used as deviance in cases were a quasi likelihood does not
# exist.
profRao <- update(profQuasi, objective = "RaoScoreStatistic",
                  X = model.matrix(logitModel))
## End Example
}
}
\keyword{htest}
\keyword{models}
\keyword{smooth}
