################################################################################
###
### TITLE: Thailand Example
###
### AUTHOR: Mark Wheldon
###
### DESC: Two-sex reconstruction demonstration using Thailand.
###
###-----------------------------------------------------------------------------
###
### SYNOPSIS:
###
### Example of population reconstruction for ALAP 2018 workshop, Puebla, Mexico.
###
###-----------------------------------------------------------------------------
###
################################################################################

library(popReconstruct)
library(ggplot2)

###-----------------------------------------------------------------------------
### 1. Get Data

data("thailand_two_sex")

asFertTHAI.mat[1:12, 1:5]

lapply(asSurvTHAI.mat, "[", i = 1:3, j = 1:5)

###-----------------------------------------------------------------------------
### 2. Proposal Variances

invGam.params <- make.hyper.params(absDev = list(fert = 0.1, surv = 0.1, mig = 0.2
             ,pop = 0.1, srb = 0.1)
             ,prob = list(fert = 0.9, surv = 0.9, mig = 0.9, pop = 0.9, srb = 0.9)
             ,alpha = list(fert = 0.5, surv = 0.5, mig = 0.5, pop = 0.5, srb = 0.5)
             ,s.star = unlist(asSurvTHAI.mat)
             )

invGam.params[1:2]

###-----------------------------------------------------------------------------
### 3. Run Reconstruction

n.iter <- 85e3
burn.in <- 10e3

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
       verb = TRUE, progress.step = n.iter
    )

ThaiMcmc <- do.call(popRecon.sampler.two.sex, args = estModArgs)
save(ThaiMcmc, file = file.path("thai_mcmc.RData"))

###-----------------------------------------------------------------------------
### 4. Acceptance Proportions

plot.acceptance.props(ThaiMcmc)

try(
    conditional.variances(ThaiMcmc)
    )

###-----------------------------------------------------------------------------
### 5. Convert to Counts

ThaiCounts <- post.process.recon(ThaiMcmc
           ,sep.factors = list(female = thaiFemale.sf
                                 ,male = thaiMale.sf)
           ,name.pref = "Thai."
           ,name.suf = ""
             )

###-----------------------------------------------------------------------------
### 6. Prior Distribution

Thailand_Example_Prior <-
    sample.from.prior(n.iter = 1e4,
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
                      mean.f = asFertTHAI.mat,
                      mean.s = asSurvTHAI.mat,
                      mean.g = asMigTHAI.mat,
                      mean.b = baselineTHAI.mat,
                      mean.srb = srbTHAI.mat,
                      age.size = 5,
                     ,name.pref = "Thai."
                     ,name.suf = "")
save(Thailand_Example_Prior, file = "Thailand_Example_Prior_small.RData")

ThaiPriorCounts <- post.process.recon(Thailand_Example_Prior
           ,sep.factors = list(female = thaiFemale.sf
                                 ,male = thaiMale.sf)
            ,name.pref = "Thai."
            ,name.suf = "Prior"
             )

###-----------------------------------------------------------------------------
### 7. Quantiles

qrecon <-
    get.quantiles.recon(param = c("srb", "e0", "tfr",
                                  "total.birth.count", "total.birth.death.count",
                                  "total.mig.count",
                                  "cohort.nq0", "period.nq0", "IMR",
                                  "mort.rate", "mig.rate", "birth.count", "death.count",
                                  "surv.prop", "mig.prop", "srb", "baseline.count"
                                  ),
                        results.recon = ThaiMcmc,
                        results.post.process.recon = ThaiCounts
                        )

qrecon <-
    get.quantiles.recon(param = c("srb", "e0", "tfr",
                                  "total.birth.count", "total.birth.death.count",
                                  "total.mig.count",
                                  "cohort.nq0", "period.nq0", "IMR",
                                  "mort.rate", "mig.rate", "birth.count", "death.count",
                                  "surv.prop", "mig.prop", "srb", "baseline.count"
                                  ),
                        results.recon = ThaiMcmc,
                        results.post.process.recon = ThaiCounts,
                        results.prior = Thailand_Example_Prior,
                        results.post.process.prior = ThaiPriorCounts
                        )

###-----------------------------------------------------------------------------
### 8. Plots

gp <- ggplot(subset(qrecon, param == "tfr"), aes(x = year, y = param.50pctl)) +
        geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
        geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2)
ggsave(paste0("Thai_tfr.pdf"), plot = gp)

for(x in c("total.birth.count", "total.birth.death.count", "total.mig.count",
           "cohort.nq0", "period.nq0", "IMR", "e0", "tfr")) {
    gp <- ggplot(subset(qrecon, param == x), aes(x = year, y = param.50pctl)) +
        geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
        geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
        facet_wrap(facet = "sex", nrow = 2, ncol = 1)
    ggsave(paste0("Thai_", x, ".pdf"), plot = gp)
}

gp <- ggplot(subset(qrecon, param == "mig.rate"), aes(x = age, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
    facet_grid(sex ~ year)
ggsave("Thai_mig_rate.pdf", plot = gp)

gp <- ggplot(subset(qrecon, param == "birth.count"), aes(x = age, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
    facet_grid(sex ~ year)
ggsave("Thai_birth_count.pdf", plot = gp)

gp <- ggplot(subset(qrecon, param == "death.count"), aes(x = age, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
    facet_grid(sex ~ year)
ggsave("Thai_death_count.pdf", plot = gp)

gp <- ggplot(subset(qrecon, param == "mort.rate"), aes(x = age, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
    scale_y_continuous(trans = "log") +
    facet_grid(sex ~ year)
ggsave("Thai_mort_rate.pdf", plot = gp)

gp <- ggplot(subset(qrecon, param == "surv.prop"), aes(x = year, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
    facet_grid(sex ~ age)
ggsave("Thai_surv_prop.pdf", plot = gp)

gp <- ggplot(subset(qrecon, param == "mig.prop"), aes(x = year, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
    facet_wrap(~ age)
ggsave("Thai_mig_prop.pdf", plot = gp)

for(x in c("baseline.count")) {
gp <- ggplot(subset(qrecon, param == x), aes(x = age, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2) +
    facet_wrap(~ sex)
    ggsave(paste0("Thai_", gsub(".", "_", x, fixed = TRUE), ".pdf"), plot = gp)
    }

gp <- ggplot(subset(qrecon, param == "srb"), aes(x = year, y = param.50pctl)) +
    geom_line(aes(col = legend), size = 1.1) + geom_point(aes(col = legend)) +
    geom_ribbon(aes(ymin = param.2.5pctl, ymax = param.97.5pctl, fill = legend), alpha = 0.2)
ggsave("Thai_srb.pdf", plot = gp)

