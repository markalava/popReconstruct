\name{net.number.migrants}
\alias{net.number.migrants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Net Number of Migrants
}
\description{
Calculates the net number of migrants from two population vectors and
the Leslie matrix describing the amount of change attributable to fertility and mortality.
}
\usage{
net.number.migrants(n1, n2, L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n1}{
Population vector at time \eqn{t}.
}
  \item{n2}{
Populatin vectors at time \eqn{t+\delta}.
}
  \item{L}{
The Leslie matrix (see \code{\link{make.leslie.matrix}}).
}
}
\details{
This function is used in the calculation of the average annual net
number of migrants.  See the vignette \pkg{burkina-faso-females}
for an example of its use.
}
\value{
Vector of net number of migrants in each age group to the population
between times \eqn{t} and \eqn{t+\delta}.
}
\author{
Mark C. Wheldon
}
\seealso{
\code{\link{popRecon.ccmp.female}}, \code{\link{make.leslie.matrix}}
}
\examples{

## Can take several minutes to run

\dontrun{
example(popRecon.ccmp.female)

example(make.leslie.matrix)

net.number.migrants(n1 = pop.input.mat[,k]
                                ,n2 = pop.input.mat[,k+1]
                                ,L = Lk)
}
}
\section{Vignettes}{\code{burkina-faso-females}}