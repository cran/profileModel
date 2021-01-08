\name{plot.profileModel}
\alias{plot.profileModel}
\alias{pairs.profileModel}
\title{Plot methods for `profileModel' objects}
\description{
  \code{plot.profileModel} plots the profiles contained in the profiled
  object. \code{pairs.profileModel} is a diagnostic tool that plots
  pairwise profile traces.
}
\usage{
\method{plot}{profileModel}(x, cis = NULL, signed = FALSE, interpolate = TRUE,
                  n.interpolations = 100, print.grid.points = FALSE,
                  title = NULL, ...)

\method{pairs}{profileModel}(x, colours = 2:3, title=NULL, ...)
}
\arguments{
  \item{x}{a \code{"profileModel"} object.}
  \item{cis}{the confidence intervals resulted from
  \code{profConfint(prof)}. The default is \code{NULL} where no
  intervals are plotted. Only used in \code{plot.profileModel}.}
  \item{signed}{if \code{TRUE} the signed square roots of the values of
  the profiled objective are plotted. The default is
  \code{FALSE}. Available only in \code{plot.profileModel}.}
  \item{interpolate}{if \code{TRUE} spline interpolation is used in
  order to get a smooth plot of the profiled objective. If \code{FALSE}
  the points that are contained in the \code{"profileModel"} object are simply
  joint by segments. The default is \code{TRUE}. Available only in
  \code{plot.profileModel}.}
  \item{n.interpolations}{The number of interpolations to take place in
  the profile range of each parameter. The default value is 100. It is
  only used when \code{interpolate=TRUE}. Available only  in
  \code{plot.profileModel}.}
  \item{print.grid.points}{logical indicating whether the points
  contained in the \code{"profileModel"} object should be printed along
  with the objective. The default is \code{FALSE}. Available only in
  \code{plot.profileModel}.}
  \item{colours}{A vector of two elements indicating the colours to be used
  for plotting pairwise profile traces. Available only in
  \code{pairs.profileModel}.}
  \item{title}{A character string to be displayed at the top of the
  resultant plotting device. The default is \code{NULL} where nothing is
  printed.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{pairs.profileModel} is a minor modification of
  \code{pairs.profile} in \pkg{MASS} library. The modification was done
  under the GPL licence 2 or greater and after the permission of the
  authors, in order to comply with objects of class
  \code{"profileModel"}. As in the description of \code{pairs.profile}
  in Venables and Ripley (2002b),  \code{pairs.profileModel} shows the
  lines that would join up the  points where the contours have
  horizontal and vertical tangents, respectively, and the fine `hairs'
  cutting the lines in the pairs plot are an indication of those
  tangents.

  The pair plots should only be used for diagnostic purposes.
}
\references{
Venables, W. N. and Ripley, B. D. (2002a). \emph{Modern applied
  statistics with S} (4th Edition). Springer.

Venables, W. N. and Ripley, B. D. (2002b). Statistics complements
  to modern applied statistics with S (4th Edition). \cr
  \url{http://www.stats.ox.ac.uk/pub/MASS4/VR4stat.pdf}.

Chambers, J. M. and Hastie, T. J. (1992) \emph{Statistical Models in
S}. Chapman \& Hall/CRC.

}
\author{Ioannis Kosmidis <email: ioannis.kosmidis@warwick.ac.uk>}
\seealso{\code{\link{profileModel}}, \code{\link{confintModel}}, \code{\link[MASS]{profile.glm}}}
\examples{
# see example in 'confintModel'.
}
\keyword{dplot}
\keyword{hplot}
