
test.sampler <- function() {
    set.seed(1)
    data(burkina_faso_females)
    BKFem.Recon.MCMC <- popRecon.sampler(
        n.iter = 10,
        burn.in = 2,
        mean.f = burkina.faso.females$fertility.rates,
        mean.s = burkina.faso.females$survival.proportions,
        mean.g = burkina.faso.females$migration.proportions,
        mean.b = burkina.faso.females$baseline.pop.counts,
        pop.data = burkina.faso.females$census.pop.counts,
        prop.vars = burkina.faso.prop.vars
    )
    stopifnot(identical(dim(BKFem.Recon.MCMC$fert.rate.mcmc)[[1]], 10L))
    stopifnot(identical(dim(BKFem.Recon.MCMC$surv.prop.mcmc)[[1]], 10L))
    stopifnot(identical(dim(BKFem.Recon.MCMC$mig.prop.mcmc)[[1]], 10L))
    stopifnot(identical(dim(BKFem.Recon.MCMC$baseline.count.mcmc)[[1]], 10L))
    stopifnot(identical(dim(BKFem.Recon.MCMC$lx.mcmc)[[1]], 10L))
}



test.sampler.two.sex <- function() {
    set.seed(1)

    data(thailand_two_sex)

    invGam.params <- make.hyper.params(absDev = list(fert = 0.1, surv = 0.1, mig = 0.2
             ,pop = 0.1, srb = 0.1)
             ,prob = list(fert = 0.9, surv = 0.9, mig = 0.9, pop = 0.9, srb = 0.9)
             ,alpha = list(fert = 0.5, surv = 0.5, mig = 0.5, pop = 0.5, srb = 0.5)
             ,s.star = unlist(asSurvTHAI.mat)
             )

    n.iter <- 3
    burn.in <- 3

estModArgs <-
    list(## inverse gamma parameters
        al.f = invGam.params$al.f,
       be.f = invGam.params$be.f,
       al.s = invGam.params$al.s,
       be.s = invGam.params$be.s,
       al.g = invGam.params$al.g,
       be.g = invGam.params$be.g,
       al.n = invGam.params$al.n,
       be.n = invGam.params$be.n,
       al.srb = invGam.params$al.srb,
       be.srb = invGam.params$be.srb,

        ## the rest
       n.iter = n.iter, burn.in = burn.in,
       start.f = asFertTHAI.mat,
       start.s = asSurvTHAI.mat,
       start.g = asMigTHAI.mat,
       start.b = baselineTHAI.mat,
       start.srb = srbTHAI.mat,
       start.sigmasq.f = 5,
       start.sigmasq.s = 5,
       start.sigmasq.g = 5,
       start.sigmasq.n = 5,
       start.sigmasq.srb = 5,
       mean.f = asFertTHAI.mat,
       mean.s = asSurvTHAI.mat,
       mean.g = asMigTHAI.mat,
       mean.b = baselineTHAI.mat,
       mean.srb = srbTHAI.mat,
       fert.rows = as.logical(apply(asFertTHAI.mat == 0L, 1, function(z) !all(z))),
       prop.varcovar = thai.propvar,
       pop.data = censusTHAI.mat,
       proj.periods = ncol(asFertTHAI.mat),
       age.size = 5,
       verb = TRUE, progress.step = 1E3
    )

ThaiMcmc <- do.call(popRecon.sampler.two.sex, args = estModArgs)

    stopifnot(identical(dim(ThaiMcmc$fert.rate.mcmc)[[1]], as.integer(n.iter)))
    stopifnot(identical(dim(ThaiMcmc$surv.prop.mcmc[[1]])[[1]], as.integer(n.iter)))
    stopifnot(identical(dim(ThaiMcmc$surv.prop.mcmc[[1]])[[2]], 144L))
    stopifnot(identical(dim(ThaiMcmc$mig.prop.mcmc[[1]])[[1]], as.integer(n.iter)))
    stopifnot(identical(dim(ThaiMcmc$baseline.count.mcmc[[1]])[[1]], as.integer(n.iter)))
    stopifnot(identical(dim(ThaiMcmc$lx.mcmc[[1]])[[1]], as.integer(n.iter)))
    stopifnot(identical(dim(ThaiMcmc$lx.mcmc[[1]])[[2]], 136L))
}
