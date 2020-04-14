################################################################################
###
### DATE CREATED: 2020-04-14
###
### DATE LAST SAVED: Time-stamp: <2020-04-14 11:53:24 (Mark.Wheldon)>
###
### AUTHOR: Mark Wheldon
###
### PROJECT: popReconstruct
###
### DESCRIPTION: Full reconstruction for Burkina Faso females.
###
###-----------------------------------------------------------------------------
###
################################################################################

setwd("C:/Users/Mark.Wheldon/Documents/repos/markalava/popReconstruct/inst/slowTests")

ptm <- proc.time()

###-----------------------------------------------------------------------------
### * Set Up

library(popReconstruct)

###-----------------------------------------------------------------------------
### * Data

data(burkina_faso_females)

###-----------------------------------------------------------------------------
### * Reconstruction

set.seed(1)

BKFem.Recon.MCMC <-
    popRecon.sampler(## Size of the MCMC sample and burn in
                     n.iter = 17,#4E4,
                     burn.in = 3,#500,
                     thin.by = 1,#50,

                     ## initial estimates and census counts
                     mean.f = burkina.faso.females$fertility.rates,
                     mean.s = burkina.faso.females$survival.proportions,
                     mean.g = burkina.faso.females$migration.proportions,
                     mean.b = burkina.faso.females$baseline.pop.counts,
                     pop.data = burkina.faso.females$census.pop.counts,

                     ## Metropolis proposal variances
                     prop.vars = burkina.faso.prop.vars,
                     verb=TRUE
                     )

###-----------------------------------------------------------------------------
### * Results

apply(BKFem.Recon.MCMC$fert.rate.mcmc, 2, "quantile", c(0.025, 0.5, 0.975))[,1:7]


proc.time() - ptm
