\name{popRecon.ccmp.female}
\alias{popRecon.ccmp.female}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Female Dominant Cohort Component Projection
}
\description{
Implements the deterministic female dominant cohort component method of
population projection.
}
\usage{
popRecon.ccmp.female(pop, surv, fert, srb = 1.05,
mig, proj.steps, age.int = 5, label.dims = FALSE, base.year = "1960")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pop}{
Population count at baseline.
}
  \item{surv}{
Survivorship probabilities: the probability of reaching the age at the
start of the interval.
}
  \item{fert}{
Matrix of average annual, single-year age-specific fertility rates (see Details).
}
  \item{srb}{
Sex ratio at birth matrix
}
  \item{mig}{
Net number of migrants as a \emph{proportion} of prev time period's population.

}
  \item{proj.steps}{
Number of time periods to project forward. If missing, set to ncol(fert).
}
  \item{age.int}{
Width of the age intervals; needed for correct interpretation of
survival probabilities and fertility rates.
}
  \item{label.dims}{
Should row and column names be set? Aesthetic.
}
  \item{base.year}{
Baseline year for popualtion projections (aesthetic).
}
}
\details{
  The first row of \code{surv} should be
\eqn{{}_nL_0/(nl_0)}{nL0/(n*l0)}, where \eqn{{}_nL_0}{nL0} and
\eqn{l_0}{l0} are the usual life table parameters.  The last row is
survival for age.int years in the open interval.

The elements of the \code{fert} argument should not be multiplied by
  \code{age.int}.  In other words, the average annual total fertility
  rates (\acronym{TFR}s) corresponding to the columns of \code{fert} are
  \code{age.int * colSums(fert)}.

  The elements of \code{mig} give the average annual net number of
  migrants, as proportion of the size of the same age-group in the
  receiving population.  Thus, the total net number of migrants to the
  population over the period
}
\value{
A matrix of projected population counts, age groups as rows, time
  periods as columns.
}
\references{
Preston, S. H., Heuveline, P. and Guillot, M. (2001) \emph{Demography},
chapter 6. Malden, MA: Blackwell.
}
\author{
Mark C. Wheldon
}
\seealso{
\code{\link{make.leslie.matrix}}, \code{\link{net.number.migrants}}
}
\examples{

data(burkina_faso_females)

(pop.input.mat <-
    popRecon.ccmp.female(pop=burkina.faso.females$baseline.pop.counts
                      ,surv=burkina.faso.females$survival.proportions
                      ,fert=burkina.faso.females$fertility.rates
                      ,mig=burkina.faso.females$migration.proportions
                      ))
}
\section{Vignettes}{\code{burkina-faso-females}}
