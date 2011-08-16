\name{print.profileModel}
\alias{print.profileModel}
\title{Printing `profileModel' objects}
\description{
  Print method for objects of class \code{profileModel}.
}
\usage{
\method{print}{profileModel}(x, print.fit = FALSE,  ...)
}
\arguments{
  \item{x}{a \code{"profileModel"} object.}
  \item{print.fit}{logical indicating whether the fitted object supplied
  in \code{\link{profileModel}} should be printed. The default value is
  \code{FALSE}.}
  \item{\dots}{additional arguments to \code{\link{print}}.}
}
\details{
  This is the \code{print} method for objects inheriting from class
  \code{"profileModel"}.
}
\seealso{
  \code{\link{print}}, \code{\link{profileModel}}.
}
\author{Ioannis Kosmidis <email: ioannis@stats.ucl.ac.uk>}
\examples{
## Begin Example
y <- c(1,1,0,0)
x1 <- c(1,0,1,0)
x2 <- c(1,1,0,0)
prof1 <- profileModel(glm(y ~ x1 + x2, family = binomial),
                      objective = "ordinaryDeviance",
                      grid.bounds = rep(c(-1,1),3))
print(prof1)
prof2 <- update(prof1, quantile = qchisq(0.95,1), grid.bounds=NULL)
print(prof2, print.fit = TRUE)
## End Example
}
\keyword{print}