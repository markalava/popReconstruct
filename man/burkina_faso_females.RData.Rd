\name{burkina_faso_females.RData}
\alias{burkina_faso_females}
\alias{burkina.faso.females}
\alias{burkina.faso.prop.vars}
\docType{data}
\title{
Data for the Vignette burkina-faso-females
}
\description{
Contains initial estimates of fertility, mortality, migration and
population counts for the reconstruction of the female population of
Burkina Faso from 1960--2000. See the vignette
\pkg{burkina-faso-females} for an illustration of its use.
}
\usage{data(burkina_faso_females)}
\format{
  The format is:
 chr "burkina-faso-females.RData"
}
\details{
  The dataset contains two lists.

  \code{burkina.faso.females} has components:
  \describe{
    \item{fertility.rates}{Matrix of initial estimates of average annual
      single-year age-specific fertility rates.}
    \item{survival.proportions}{Matrix of initial estimates of
      age-specific survival proportions.}
    \item{migration.proportions}{Matrix of initial estimates of
  age-specific average annual net migration as a proportion of the size
  of the same age-group in the receiving population.}
\item{baseline.pop.counts}{Single column matrix of age-specific
  population counts in the baseline year (1960).}
\item{census.pop.counts}{Matrix of age-specific census counts in years
  after the baseline year (1975, 1985, 1995, 2005).}
}

\code{burkina.faso.prop.vars} has components \code{fert.rate},
\code{surv.prop}, \code{mig.prop}, \code{population.count} which contain
variances for the Gaussian densities used as proposals in the random walk
Metropolis-Hastings algorithm used to sample from the joint posterior.
}
\references{
Wheldon, M. C., Raftery, A. E., Clark, S. J. and Gerland, P. (2011)
\emph{Estimating Demographic Parameters with Uncertainty}. Working Paper
No. 108, Center for Statistics and the Social Sciences, University of
Washington, Seattle. \url{http://www.csss.washington.edu/Papers/wp108.pdf}.
}
\examples{
data(burkina_faso_females.RData)
}
\section{Vignettes}{\pkg{burkina-faso-females}}
\keyword{datasets}
