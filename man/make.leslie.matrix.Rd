\name{make.leslie.matrix}
\alias{make.leslie.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make Leslie Matrix
}
\description{
Constructs the Leslie Matrix needed for cohort component projection.
}
\usage{
make.leslie.matrix(pop, surv, fert, srb = 1.05, age.int = 5, label.dims = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pop}{
Population count at baseline.
}
  \item{surv}{
Survivorship probabilities: the probability of reaching the age at the
start of the interval. The first row should be nL0/(n*l0). The last row
is survival for age.int years in the open interval.
}
  \item{fert}{
Matrix of age specific fertility rates NOT yet mulitplied by age.int.
}
  \item{srb}{
Sex ratio at birth (matrix or scalar).
}
  \item{age.int}{
Width of the age intervals; needed for correct interpretation of
survival probabilities and fertility rates.
}
  \item{label.dims}{
Should row and column names be set? Aesthetic.
}
}
\details{
This function is used in the calculation of the average annual net
number of migrants. See the vignette \code{burkina-faso-females}
for an example of its use.
}
\value{
A Leslie matrix as a matrix object.
}
\references{
Preston, S. H., Heuveline, P. and Guillot, M. (2001) \emph{Demography},
chapter 6. Malden, MA: Blackwell.
}
\author{
Mark C. Wheldon
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{popRecon.ccmp.female}}, \code{\link{net.number.migrants}}
}
\examples{
example(popRecon.ccmp.female)

(Lk <- make.leslie.matrix(pop = pop.input.mat[,1]
                       ,surv = burkina.faso.females$survival.proportions[,1]
                       ,fert = burkina.faso.females$fertility.rates[,1]
                       ,srb = 1.05
                       ,age.int = 5))
}
\section{Vignettes}{\code{burkina-faso-females}}