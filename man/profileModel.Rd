\name{profileModel}
\alias{profileModel}
\alias{prelim.profiling}
\alias{profiling}
\title{Get the profiles of arbitrary objectives for arbitrary `glm'-like
  models}
\description{
  Calculates the profiles of \bold{arbitrary} objectives (inference functions
  in the terminology of Lindsay and Qu, 2003) for the parameters of
  \bold{arbitrary} \code{\link{glm}}-like models with linear
  predictor. It provides a variety of options such as profiling over a
  pre-specified grid, profiling until the profile of the objective
  reaches the values of a quantile, calculating the profile traces along
  with the profiled objectives, and others.
}
\usage{
profileModel(fitted, gridsize = 20, stdn = 5, stepsize = 0.5,
             grid.bounds = NULL, quantile = NULL,
             objective = stop("'objective' is missing."),
             agreement = TRUE, verbose = TRUE, trace.prelim = FALSE,
             which = 1:length(coef(fitted)), profTraces = TRUE,
             zero.bound = 1e-08, scale = FALSE,
             stdErrors = NULL, ...)

prelim.profiling(fitted, quantile = qchisq(0.95, 1),
                 objective = stop("'objective' is missing."),
                 verbose = TRUE, which = 1:length(coef(fitted)),
                 stepsize = 0.5, stdn = 5, agreement = TRUE,
                 trace.prelim = FALSE,
                 stdErrors = NULL, ...)

profiling(fitted, grid.bounds, gridsize = 20, verbose = TRUE,
          objective = stop("'objective' is missing."),
          agreement = TRUE, which = 1:length(coef(fitted)),
          profTraces = TRUE, zero.bound = 1e-08, ...)
}
\arguments{
  \item{fitted}{a \code{\link{glm}}-like fitted object with \bold{linear
      predictor} (see Details for the methods that have to be supported
    by \code{fitted}).}
  \item{which}{which parameters should be profiled? Has to be a vector
    of integers for \code{profiling} and \code{prelim.profiling} but for
    \code{profileModel} it  could also be a vector of parameter
    names. The default is \code{1:length(coef(fitted))}, i.e. all the
    parameters estimated in \code{fitted}.}
  \item{grid.bounds}{a matrix of dimension \code{length(which)} by \code{2} or a
    \code{2*length(which)} vector that specifies the range of values in which
    profiling takes place for each parameter. It has to be set for
    \code{profiling} and the default is \code{NULL} for \code{profileModel}}
  \item{gridsize}{The number of equidistant parameter values to be taken
    between the values specified in the entries of \code{grid.bounds}.}
  \item{stepsize}{a positive integer that is used in
    \code{prelim.profiling} to penalize the size of the steps taken to the left
    and to the right of the estimate. The default value is 0.5.}
  \item{stdn}{in \code{profileModel}, the number of estimated standard
    deviations to move left or right from the estimated parameter
    value, when both \code{quantile} and \code{grid.bounds} are \code{NULL}. In
    \code{prelim.profiling}, \code{stdn/stepsize} is the maximum number of
    steps that are taken to the left and to the right of the estimate. The
    default value of \code{stdn} is 5 (see Details).}
  \item{quantile}{a quantile, indicating the range that the profile must
    cover. The default value in \code{profileModel} is \code{NULL} and in
    \code{prelim.profiling}, \code{qchisq(0.95,1)} (see Details).}
  \item{objective}{the function to be profiled. It is a function of the
    \bold{restricted} fitted object and other arguments (see
    \code{\link{objectives}}). It should be of class \code{function} for
    \code{profiling} and \code{prelim.profiling} but it could also be a
    character string to be matched for \code{profileModel}.}
  \item{agreement}{logical indicating whether the fitting method used
    for \code{fitting} agrees  with the specified objective, i.e. whether
    the objective is minimized at \code{coef(fitted)}. The default is
    \code{TRUE}.}
  \item{verbose}{logical. If \code{TRUE} (default) progress indicators
    are printed during  the profiling progress.}
  \item{trace.prelim}{logical. If \code{TRUE} the preliminary iteration is
    traced. The default is \code{FALSE}.}
  \item{profTraces}{logical indicating whether the profile traces should
    be returned. The default is \code{TRUE}.}
  \item{zero.bound}{a small positive constant. The difference of the
    objective at the \bold{restricted} fit from the objective at
    \code{fitted} takes value zero if it is smaller than
    \code{zero.bound}. \code{zero.bound} is only used when
    \code{agreement=TRUE} and the default value is \code{1e-08}.}
  \item{scale}{logical. The
    default is \code{FALSE}. Currently has no effect. Only available in \code{profileModel}.}
  \item{stdErrors}{The vector estimated asymptotic standard errors reported
    from the fitting procedure. The default is \code{NULL} (see Details).}
  \item{\dots}{further arguments passed to the specified objective.}
}
\details{
  \code{fitted} has to be an object which supports the method
  %  \code{\link{coef}}, \code{\link{formula}}, \code{\link{model.matrix}}
  \code{\link{coef}} and which has \code{fitted$terms} with the same
  meaning as, for example, in \code{\link{lm}} and
  \code{\link{glm}} (see also \code{\link{terms}}). \code{coef(fitted)}
  has to be a \bold{vector} of coefficients with each component
  corresponding to a column of the model matrix returned by

  \code{mf <- model.frame(fitted$terms,data=eval(fitted$call$data)) ;
  model.matrix(fitted$terms,mf,contrasts = fitted$contrasts)}

  (or just \code{model.matrix(fitted)}, for \code{fitted} objects that
  support the \code{\link{model.matrix}} method.)

  Exception to this are objects returned by \code{BTm} of the
  \pkg{BradleyTerry} package, where some special handling of the required
  objects takes place.

  Note that any or both of \code{data} and \code{contrasts} could be
  \code{NULL}. This depends whether the \code{data} argument has been
  supplied to the procedure and whether \code{fitted$contrast} exists.
 % Also, it should return a vector of
 % coefficients, each corresponding to a column of
 % \code{model.matrix(fitted)}.

  The fitting procedure that resulted \code{fitted} has to support
  \code{\link{offset}} in \code{\link{formula}}.
%  and the object returned
%  by \code{model.frame(fitted)} has to support the methods
%  \code{\link{model.offset}} and \code{\link{model.response}}.
  Also, \code{fitted$call} has to be the call that generated \code{fitted}.

  If the fitting procedure that resulted \code{fitted}  supports an
  \code{etastart} argument (see \code{\link{glm}}) and
  \code{fitted$linear.predictor} contains the estimated linear
  predictors then during profiling, the appropriate starting values
  are supplied to the fitting procedure. In this way, the iteration is
  accelerated and is more stable, numerically. However, it is not necessary
  that \code{etastart} is supported. In the latter case no starting
  values are supplied to the fitting procedure during profiling.

  Support for a \code{\link{summary}} method is
  optional. \code{\link{summary}} is only used for obtaining the
  estimated asymptotic standard errors associated to the coefficients in
  \code{fitted}. If \code{stdErrors=NULL} the standard errors are taken
  to be \code{summary(fitted)$coefficients[,2]} which is the place where
  the estimated asymptotic standard errors usually are for
  \code{\link{glm}}-like objects. If this this is not the case then
  \code{stdErrors} should be set appropriately. \cr \cr

  \code{profiling} is the workhorse function that does the basic operation of
  profiling objectives over a user-specified grid of values. For a given
  parameter \eqn{\beta}, the \bold{restricted} fit
  \eqn{F_{\beta=b}}{F(b)} is calculated by constraining
  \eqn{\beta} to a point \eqn{b} of the grid. Then the difference

  \deqn{D(F_{\beta=b}) = P(F_{\beta=b}) - P(F_0),}{D(F(b)) = P(F(b)) - P(G),}

  is calculated, where \eqn{P} is the objective specified by the user
  and \eqn{G} is the original fit (\code{fitted}). For convex
  objectives that are minimized at the estimates of \eqn{G} (see
  \code{agreement}), \eqn{D(G)=0}.

  \code{prelim.profiling} refers only to convex objectives and searches for
  and returns the grid bounds (\code{grid.bounds}) for each
  profiled parameter that should be used in order the profile to cover
  \code{quantile}. For a given parameter \eqn{\beta},
  \code{prelim.profiling} also checks whether  such enclosure can be
  found and returns a logical matrix \code{intersects} of dimension
  \code{length(which)} by \code{2} that indicates if the profile covers the
  quantile to the left and to the right of the estimate in
  \code{fitted}. At step \code{i} of the search a value \eqn{b_i} is
  proposed for \eqn{\beta} and \eqn{D(F_{\beta=b_i})}{D(F(b_i))} is calculated. If
  \eqn{D(F_{\beta=b_i})<q}{D(F(b_i))<q}, where \eqn{q} is \code{quantile}, the next
  proposed value is

  \deqn{b_{i+1} = b_{i} \pm (i+1) C \min(s,30)/|L| ,}{b_{i+1} = b_i +-
  (i+1) C min(s, 30) /abs(L) ,}

  where \eqn{C} is \code{stepsize}, \eqn{s} is the
  estimated asymptotic standard error of \eqn{\beta} from \eqn{G} and
  \eqn{L} is the slope of the line segment connecting the points
  \eqn{(b_i, D(F_{\beta=b_i}))}{[b_i, D(F(b_i))]} and
  \eqn{(b_{i-1}, D(F_{\beta=b_{i-1}}))}{[b_{i-1},
  D(F(b_{i-1}))]}. \eqn{\pm}{+-} is \eqn{+} if the search is on the
  right of the estimate of \eqn{\beta} and \eqn{-} on the left. If an
  increase of \eqn{D} is expected then the step slows down. If
  \eqn{|L|<1}{abs(L)<1} then \eqn{|L|}{abs(L)} is set to 1 and if
  \eqn{|L|>500}{abs(L)>500} then \eqn{|L|}{abs(L)}  is set to 500. In
  this way the iteration is conservative by avoiding very small steps
  but not over-conservative by avoiding very large steps.

  If the maximum number of steps \code{stdn/stepsize} (call this \eqn{M})
  was taken and the quantile was not covered by the profile but the three
  last absolute slopes where  positive then the iteration is restarted
  form \eqn{b_{M-1}} with \eqn{2C} instead of \eqn{C} in the step
  calculation. If the three last slopes were less than \code{1e-8} in
  absolute value then the iteration stops and it is considered that
  \eqn{D} has an asymptote at the corresponding direction (left or right).
  Note that when the latter takes place the iteration has already moved
  \eqn{6 C\min(s,30)}{6 C min(s, 30)} units on the scale of \eqn{\beta},
  since the first value of \eqn{b} were a slope of 1e-8 in absolute value
  was detected. Thus we could safely say that an asymptote has been
  detected and avoid calculation of \eqn{F_{beta=b}}{F(beta=b)} for
  extremely large \eqn{b}'s.

  Very small values of \code{stepsize} make \code{prelim.profiling} take
  very small steps with the effect of slowing down the execution
  time. Large values of \code{stepsize} are only recommended when the
  estimated asymptotic standard errors are very small in \code{fitted}.

  \code{profileModel} is a wrapper function that collects and combines
  the capabilities of \code{profiling} and \code{prelim.profiling} by
  providing a unified interface for their functions, as well as appropriateness
  checks on the arguments. When both \code{quantile}  and
  \code{grid.bounds} are \code{NULL} then \code{profiling} is called and
  profiling takes place for \code{stdn} estimated asymptotic standard
  errors on the left and on the right of the estimates in
  \code{fitted}. This could be used for taking a quick look of the
  profiles around the estimate. With only the \code{quantile} being
  \code{NULL}, profiling is performed on the the specified grid of
  values. When \code{quantile} is specified and \code{grid.bounds} is
  \code{NULL}, \code{prelim.profiling} is  called and its result is
  passed to \code{profiling}. If both \code{quantile} and
  \code{grid.bounds} then \code{grid.bounds} prevails and profiling is
  performed on the specified grid.
}
\value{
  \code{profiling} returns a list of profiles, with one named component
  for each parameter profiled. Each component of
  the list contains the profiled parameter values and the corresponding
  differences of the objective at the \bold{restricted} fit from the
  objective at \code{fitted}. When \code{profTraces=TRUE} the corresponding
  profile traces are \code{\link{cbind}}'ed to each component of the
  list.

  \code{prelim.profiling} returns a list with components
  \code{intersects} and \code{grid.bounds}.

  \code{profileModel} returns an object of class \code{"profileModel"}
  that has the attribute \code{includes.traces} corresponding to the
  value of the \code{profTraces} argument. The \code{"profileModel"}
  object is a  list of the following components:
  \item{profiles}{the result of \code{profiling}.}
  \item{fit}{the \code{fitted} object that was passed to
    \code{profileModel}.}
  \item{quantile}{the \code{quantile} that was passed to
    \code{profileModel}.}
  \item{gridsize}{the \code{gridsize} that was passed to
    \code{profileModel}.}
  \item{intersects}{if \code{quantile=NULL} then
  \code{intersects=NULL} else \code{intersects} is as for
    \code{prelim.profiling}.}
  \item{profiled.parameters}{a vector of integers indicating which
    parameters were profiled.}
  \item{profiled.objective}{the profiled objective with any additional
    arguments passed through \code{\dots} evaluated.}
  \item{isNA}{a logical vector indicating which of the parameters in
    \code{which} were \code{NA} in \code{fitted}.}
  \item{agreement}{the \code{agreement} that was passed to
    \code{profileModel}.}
  \item{zero.bound}{the \code{zero.bound} that was passed to
    \code{profileModel}.}
  \item{grid.bounds}{the grid bounds that were used for profiling.}
  \item{call}{the matched call.}
}
\note{
  Methods specific to objects of class \code{"profileModel"} are
  \itemize{
    \item \code{print}, see \code{\link{print.profileModel}}.
    \item \code{signedSquareRoots}, see \code{\link{signedSquareRoots}}.
    \item \code{profConfint}, see \code{\link{profConfint}}.
    \item \code{plot}, see \code{\link{plot.profileModel}}.
    \item \code{pairs}, see \code{\link{pairs.profileModel}}.
  }

  \code{profileModel} has been tested and is known to work for fitted
  objects resulting from  \code{\link{lm}}, \code{\link{glm}},
  \code{polr}, \code{gee}, \code{geeglm}, \code{brglm} and \code{BTm}.
}

\references{
  Lindsay, B. G. and Qu, A. (2003). Inference functions and quadratic
  score tests. \emph{Statistical Science} \bold{18}, 394--410.

  Chambers, J. M. and Hastie, T. J. (1992) \emph{Statistical Models in
  S}. Chapman \& Hall/CRC.

}
\author{Ioannis Kosmidis <email: ioannis.kosmidis@warwick.ac.uk>}
\seealso{\code{\link{confintModel}}, \code{\link{plot.profileModel}}.}
\examples{
## Begin Example 1
library(MASS)
m1 <- glm(Claims ~ District + Group + Age + offset(log(Holders)),
          data = Insurance, family = poisson)
# profile deviance +-5 estimated standard errors from the estimate
prof0 <- profileModel(m1, objective = "ordinaryDeviance")
# profile deviance over a grid of values
gridd <- rep(c(-1,1), length(coef(m1)))
prof1 <- profileModel(m1, grid.bounds = gridd,
                      objective = "ordinaryDeviance")
# profile deviance until the profile reaches qchisq(0.95,1)
prof2 <- profileModel(m1, quantile = qchisq(0.95,1) ,
                      objective = "ordinaryDeviance")
# plot the profiles of the deviance
plot(prof2)
# quite quadratic in shape. Just to make sure:
plot(prof2, signed = TRUE)
# Ok straight lines. So we expect first order asymptotics to work well;
\dontrun{
# plot the profiles of the Rao score statistic
# profile Rao's score statistic
prof3 <- update(prof2, objective = "RaoScoreStatistic",
                X = model.matrix(m1))
plot(prof3)
# The 95\% confidence intervals based on prof2 and prof3 and the simple Wald
# confidence intervals:
profConfint(prof2)
profConfint(prof3)
stdErrors <- coef(summary(m1))[,2]
coef(m1)+ qnorm(0.975) * cbind(-stdErrors,stdErrors)
# They are all quite similar in value. The result of a quadratic likelihood.
## End Example
}
## Begin Example 2: Monotone likelihood; data separation;
library(MASS)
y <- c(0, 0, 1, 0)
tots <- c(2, 2, 5, 2)
x1 <- c(1, 0, 1, 0)
x2 <- c(1, 1, 0, 0)
m2 <- glm(y/tots ~ x1 + x2, weights = tots,
          family = binomial)
prof <- profileModel(m2, quantile=qchisq(0.95,1),
                     objective = "ordinaryDeviance")
plot(prof)
profConfint(prof)
# profile.glm fails to detect the finite endpoints
confint(m2)
## End Example
\dontrun{
## Begin Example 3: polr
library(MASS)
options(contrasts = c("contr.treatment", "contr.poly"))
house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
prof.plr0 <- profileModel(house.plr, objective = function(fm) fm$deviance)
plot(prof.plr0)
# do it with a quantile
prof.plr1 <- update(prof.plr0, quantile = qchisq(0.95, 1))
plot(prof.plr1)
## End Example
}
}
\keyword{models}
