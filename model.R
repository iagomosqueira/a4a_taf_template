# Run analysis, write model results

# Before:
# After:

library(TAF)
mkdir("model")

library(FLa4a)
library(ggplotFL)

# LOAD data
load("data/data.rda")

# SETTINGS
its <- 500

# -- SET submodels

# F model
fmod <- ~ s(age, k = 6) + s(year, k=14) + ti(age, year, k = c(4, 10))

# SR model
srmod <- ~factor(year)

# N1 model
n1mod <- ~s(age, k = 3)

# Q model
qmod <- list(~s(age, k = 6), ~s(age, k = 5), ~s(age, k = 6))

# -- FIT model

fit <- sca(stk, idx, fmodel=fmod, qmodel=qmod, srmodel=srmod, n1model=n1mod)

# -- DIAGNOSE model

# CHECK convergence
fitSumm(fit)['maxgrad',]
fitSumm(fit)['convergence',]

# RUN retrospective
n <- 5
yrs <- seq(dims(stk)$maxyear, length=n+1, by=-1)

ret <- FLStocks(lapply(setNames(nm=yrs), function(x) {
  window(stk, end=x) + sca(window(stk, end=x), window(idx, end=x),
    fmodel=fmod, qmodel=qmod, srmodel=srmod, n1model=n1mod)
}))

plot(ret, metrics=list(Rec=rec, SSB=ssb, F=fbar))

mohnMatrix(ret, 'ssb')
mohnMatrix(ret, 'fbar')

library(icesAdvice)

mohn(mohnMatrix(ret, 'rec'))
mohn(mohnMatrix(ret, 'ssb'))
mohn(mohnMatrix(ret, 'fbar'))

# RUNS test

plotRunstest(fit, idx, combine = F) + theme_bw() + facet_wrap(~age)

plotRunstest(catch.n(stk), catch.n(stk + fit), combine = F) + theme_bw() + facet_wrap(~age)

# HINDCAST prediction skill

library(a4adiags)
library(doFuture)
plan(multicore, workers = 4)

nsq <- 3

hc <- a4ahcxval(stk, idx, nyears = n, nsq = nsq)


# UPDATE
run <- stk + fit

# COMPUTE residuals
res <- residuals(fit, stk, idx)

# RUN simulation
sim <- simulate(fit, its)

# BUILD OM
om <- stk + sim


plot(run)
plot(om)

# COMPARE simulated (OM) SSB and Fbar ranges with the estimated stock
com <- function(x) apply(x@.Data, 2, quantile, probs = c(0.05, 0.5, 0.95))
com(ssb(om))
ssb(run)
com(fbar(om))
fbar(run)

resid <- residuals(fit, stk, idx)

catch_diags <- computeCatchDiagnostics(fit, stk)

