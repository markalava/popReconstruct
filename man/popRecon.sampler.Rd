\name{popRecon.sampler}
\alias{popRecon.sampler}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MCMC Sampler for the popReconstruct Package
}
\description{
Draws an \acronym{MCMC} sample from the joint posterior distribution of
age-specific fertility rates, survival and migration proportions and
baseline population counts.
}
\usage{
popRecon.sampler(n.iter, burn.in = 0, thin.by = 1, al.f = 1, be.f = 0.0109,
al.s = 1, be.s = 0.0109, al.g = 1, be.g = 0.0436, al.n = 1, be.n = 0.0109,
mean.f, mean.s, mean.g, mean.b, start.f = mean.f, start.s = mean.s,
start.g = mean.g, start.b = mean.b, start.sigmasq.f = 5, start.sigmasq.s = 5,
start.sigmasq.g = 5, start.sigmasq.n = 5, pop.data, prop.vars,
ccmp.function = popRecon.ccmp.female, proj.periods = ncol(mean.f),
age.size = 5, verb = FALSE, s.tol = 10^(-10))
}
\arguments{
  \item{n.iter}{
Number of iterations to save.
}
  \item{burn.in}{
Number of burn-in iterations; not saved.
}
  \item{thin.by}{
Thinning parameter; save every 'thin'th iteration.
}
\item{al.f}{
  Shape parameter of the distribution of the variance of the
  age-specific ferility rate initial estimates.
}
  \item{be.f}{
  Scale parameter of the distribution of the variance of the
  age-specific ferility rate initial estimates.
}
  \item{al.s}{
  Shape parameter of the distribution of the variance of the
  age-specific survival proportion initial estimates.
}
  \item{be.s}{
  Scale parameter of the distribution of the variance of the
  age-specific survival proportion initial estimates.
}
  \item{al.g}{
  Shape parameter of the distribution of the variance of the
  age-specific migration proportion initial estimates.
}
  \item{be.g}{
  Scale parameter of the distribution of the variance of the
  age-specific migration proportion initial estimates.
}
  \item{al.n}{
  Shape parameter of the distribution of the variance of the
  age-specific population count initial estimates and census counts.
}
  \item{be.n}{
  Scale parameter of the distribution of the variance of the
  age-specific population count initial estimates and census counts.
}
\item{mean.f}{
  Matrix of initial estimates of age-specific fertility rates with age
  groups as rows, time periods as columns.
}
  \item{mean.s}{
  Matrix of initial estimates of age-specific survival proportions with age
  groups as rows, time periods as columns.
  }
  \item{mean.g}{
  Matrix of initial estimates of age-specific migration proportions with age
  groups as rows, time periods as columns.
}
  \item{mean.b}{
  Matrix of initial estimates of age-specific baseline population counts
  with age groups as rows, time periods as columns.
}
  \item{start.f}{
Matrix of MCMC start values of age-specific fertility rates with age
  groups as rows, time periods as columns.
}
  \item{start.s}{
Matrix of MCMC start values of age-specific survival proportions with age
  groups as rows, time periods as columns.
}
  \item{start.g}{
Matrix of MCMC start values of age-specific migration proportions with age
  groups as rows, time periods as columns.
}
  \item{start.b}{
Matrix of MCMC start values of age-specific baseline population counts with age
  groups as rows, time periods as columns.
}
  \item{start.sigmasq.f}{
MCMC start value for variance of the age-specific fertility rate initial
estimates.
}
  \item{start.sigmasq.s}{
MCMC start value for variance of the age-specific survival proportions initial
estimates.
}
  \item{start.sigmasq.g}{
MCMC start value for variance of the age-specific migration proportions
initial estimates.
}
  \item{start.sigmasq.n}{
MCMC start value for variance of the age-specific baseline count
initial estimates.
}
  \item{pop.data}{
Matrix fo census counts, with age groups as rows, time periods as columns.
}
  \item{prop.vars}{
List of matrices of variances for Gaussian Metropolis random walk
proposals. Components are
}
  \item{ccmp.function}{
Function to do cohort component projection.
}
\item{proj.periods}{
  Number of age.size-year periods to over which to do the
  reconstruction, beginning at the baseline year.
}
\item{age.size}{
  Width of the age groups and time periods in years.
}
\item{verb}{
  Logical; should progress messages be printed.
}
\item{s.tol}{
  A tolerance parameter to stop over-/under-flow when taking logits of
  survival proportions.
}
}
\details{
  This is the main function of the \pkg{popReconstruct} package.  It takes
  initial estimates of age-specific vital rates, migration proportions
  and population counts in the baseline year and draws an MCMC sample
  from the joint posterior distribution defined in Wheldon et
  al. (2011).  See the vignette \pkg{burkina-faso-females} for an
  extended illustration of its use.
}
\value{
  A list with components;
  \item{fert.rate.mcmc}{mcmc object containing MCMC chains for
    age-specific fertility rates.}
  \item{surv.prop.mcmc}{mcmc object containing MCMC chains for
    age-specific survival proportions.}
  \item{mig.prop.mcmc}{mcmc object containing MCMC chains for
    age-specific migration proportions.}
  \item{baseline.count.mcmc}{mcmc object containing MCMC chains for
    age-specific baseline counts.}
  \item{lx.mcmc}{mcmc object containing MCMC chains for
    age-specific population counts at years subsequent to baseline.}
  \item{variances.mcmc}{mcmc object containing MCMC chains for
    the vital rate variance parameters.}
  \item{alg.stats}{\dQuote{algorithm statistics}; a list with components
    \code{acceptance.proportions} giving Metropolis acceptance
    proportions for each vital rate parameter,
    \code{pop.went.negative} giving the number of times Metropolis
    proposals resulted in negative poplation counts, \code{run.time}
    giving the time taken to run the sampler and some other, as yet
    undocumented, components.}
  \item{fixed.params}{A list containing the initial estimates and
    hyperparmaters of the variance distributions.}
  \item{start.vals}{A list containing Metropolis start values for the
    vital rates, migration proportions, baseline counts and variance
    parameters.}
  \item{alg.params}{\dQuote{algorithm parameters}; a list returning some
    of the arguments supplied for this run, saved for later
    reference. For example, Metropolis proposal variances
    (\code{prop.vars}) and the number
    of iterations (\code{iters}) are stored here.
  }
}
\section{Vignettes}{burkina-faso-females}
\references{
Wheldon, M. C., Raftery, A. E., Clark, S. J. and Gerland, P. (2011)
\emph{Estimating Demographic Parameters with Uncertainty}. Working Paper
No. 108, Center for Statistics and the Social Sciences, University of
Washington, Seattle. \url{http://www.csss.washington.edu/Papers/wp108.pdf}.
}
\author{
Mark C. Wheldon
}