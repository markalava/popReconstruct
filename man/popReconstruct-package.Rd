\name{popReconstruct-package}
\alias{popReconstruct-package}
\alias{popReconstruct}
\docType{package}
\title{
\packageTitle{popReconstruct}
}
\description{
\pkg{popReconstruct} is a package for reconstructing populations of the
recent past.  It simultaneously estimates age-specific population
counts, fertility rates, mortality rates and net international migration
flows from fragmentary data, and incorporates measurement error.
Informative priors are required for vital rates, migration rates,
population counts at baseline, and their respective measurement error
variances.  Inference is based on the joint posterior probability
distribution which yields fully probabilistic interval estimates.  A
sample from this distribution is drawn using a Markov chain Monte Carlo
algorithm.
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{popReconstruct}
\packageIndices{popReconstruct}

See the vignette \pkg{burkina-faso-females} for a detailed illustration.
}
\author{
\packageAuthor{popReconstruct}

Maintainer: \packageMaintainer{popReconstruct}
}
\section{Vignettes}{\code{burkina-faso-females}}
\references{
Wheldon, M. C., Raftery, A. E., Clark, S. J. and Gerland,
P. (2013). Reconstructing Past Populations With Uncertainty From
Fragmentary Data. \emph{Journal of the American Statistical
Association}, 108:501, 96-110. \doi{10.1080/01621459.2012.737729}.
}