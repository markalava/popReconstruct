% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proposal_variances.R
\name{conditional.variances}
\alias{conditional.variances}
\title{Calculate conditional variances from MCMC chains of a population reconstruction}
\usage{
conditional.variances(results.recon, plot = TRUE, return.res = TRUE)
}
\arguments{
\item{results.recon}{Output from \code{\link{popRecon.sampler.two.sex}}.}

\item{plot}{Logical: plot variances?}

\item{return.res}{Logical: return calculated conditional variances
and suggested proposal variances (\cite{Roberts & Rosenthal, 2001}) in a list?}
}
\value{
If \code{isTRUE{return.res}}, a list with conditional
    variances for each parameter, otherwise nothing. A plot is
    generated if \code{isTRUE{plot}}.
}
\description{
To aid tuning of the Metropolis-Hastings algorithm, conditional
variances can be computed and inspected.
}
\references{
Roberts, G. O., and Rosenthal, J. S. (2001),
    "Optimal Scaling for Various Metropolis-Hastings Algorithms",
    Statistical Science, 16, 351-367.
}
\author{
Mark C. Wheldon
}
