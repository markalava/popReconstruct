\name{life.expectancy.stationary}
\alias{life.expectancy.stationary}
\title{
  Life Expectancy At Birth in a Stationary Population
}
\description{
  Calculates life expectancy at birth from age-specific survival
  proportions in a stationary population.  See the vignette
  \code{burkina-faso-females} for an illustration of its use.
}
\usage{
life.expectancy.stationary(z)
}
\arguments{
  \item{z}{
Vector of age specific survival proportions for age groups of width five
years.
}
}
\details{
  This function calculates the life expectancy at birth in a stationary
  population subject to the survival proportions supplied.
}
\value{
  Life expectancy at birth.
}
\author{
  Mark C. Wheldon
}
\examples{

## Can take a few minutes

\dontrun{data(burkina_faso_females)
life.expectancy.stationary(burkina.faso.females$survival.proportions[,1])
}

}
\section{Vignettes}{\code{burkina-faso-females}}
