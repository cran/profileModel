\name{objectives-profileModel}
\alias{ordinaryDeviance}
\alias{RaoScoreStatistic}
\alias{objectives}
\title{Objectives to be profiled}
\description{
  Objectives to be used in \pkg{profileModel}.
}
\usage{
ordinaryDeviance(fm, dispersion = 1)

RaoScoreStatistic(fm, X, dispersion = 1)
%penalizedDeviance(fm, X, dispersion = 1)
}
\arguments{
  \item{fm}{the \bold{restricted} fit.}
  \item{X}{the model matrix of the fit on all parameters.}
  \item{dispersion}{the dispersion parameter.}
}
\details{
  The objectives used in \pkg{profileModel} have to be functions of the
  \bold{restricted} fit. Given a fitted object, the restricted fit is an
  object resulted by restricting a parameter to a specific value and
  then estimating the remaining parameters. Additional arguments
  could be used and are passed to the objective matching the \dots in
  \code{profileModel} or in other associated functions. An objective
  function should return a scalar which is the value of the objective at the
  restricted fit.

  The construction of a custom objective should follow the above simple
  guidelines (see also Example 3 in \code{\link{profileModel}} and the
  sources of either \code{ordinaryDeviance} or \code{RaoScoreStatistic}).

  \code{ordinaryDeviance} refers to \code{\link{glm}}-like objects. It takes as
  input the restricted fit \code{fm} and optionally the value of the
  dispersion parameter and returns the deviance corresponding to the
  restricted fit divided by \code{dispersion}.

  \code{RaoScoreStatistic} refers to \code{\link{glm}}-like objects. It returns
  the value of the Rao score statistic
  \eqn{s(\beta)^Ti^{-1}(\beta)s(\beta)/\phi}, where \eqn{s} is the vector of
  estimating equations, \eqn{\phi} is the dispersion parameter and

  \deqn{i(\beta) = cov(s(\beta)) = X^T W(\beta) X/\phi ,}{i(\beta) = cov(s(\beta)) = X' W(\beta) X/\phi ,}

  in standard GLM notation. The additional argument \code{X} is
  the model  matrix of the full (\bold{not} the restricted) fit. In this
  way the original fit has always smaller or equal Rao score statistic
  from any restricted fit. The Rao score statistic could be used for the
  construction of confidence intervals when quasi-likelihood estimation
  is used (see Lindsay and Qu, 2003).
}
\value{
  A scalar.
}
\references{
  Lindsay, B. G. and Qu, A. (2003). Inference functions and quadratic
  score tests. \emph{Statistical Science} \bold{18}, 394--410.
}
\author{Ioannis Kosmidis <email: ioannis.kosmidis@warwick.ac.uk>}
\note{
  Because the objective functions are evaluated many times in
  \code{\link{profiling}}, \code{\link{prelim.profiling}} and
  \code{\link{profileModel}}, they should be as computationally
  efficient as possible.
}
\seealso{\code{\link{profiling}}, \code{\link{prelim.profiling}}, \code{\link{profileModel}}.}
\keyword{models}
\keyword{htest}
