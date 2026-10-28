# model.R - Run analysis, write model results
# a4a_taf_template/model.R

# Author: Ernesto JARDIM <ernesto.jardim@ipma.pt>
#         Colin MILLAR <colin.millar@ices.dk>
#         Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# Before: data/data.rda

# After: model/model.rda
#   - fit: model fit, a4aFitSA
#   - resid: git residuals, a4aFitResiduals
#   - hind_idx: hindcasted indices, list (FLIndices)
#   - retro_stks: retrospectiove runs, FLStocks
#   - sim: simulated fit, a4aFitSA
#   - diags_catch: catch diagnostics, a4aFitCatchDiagn

library(TAF)
mkdir("model")

library(FLa4a)
library(a4adiags)
library(icesAdvice)

# LOAD data
load("data/data.rda")

# SETTINGS
its <- 500
nyr <- 5
nsq <- 3

# -- SET submodels

# F model
fmod <- ~ s(age, k = 6) + s(year, k=12) + ti(age, year, k = c(4, 10))

# SR model
srmod <- ~factor(year)

# N1 model
n1mod <- ~s(age, k = 3)

# Q model
qmod <- list(~s(age, k = 6), ~s(age, k = 5), ~s(age, k = 6))

# -- FIT model

fit <- sca(stk, idx, fmodel=fmod, qmodel=qmod, srmodel=srmod, n1model=n1mod)

# -- RUN retrospective and hindcast analyses

library(doFuture)
plan(multisession, workers = 3)

hind <- a4ahcxval(stk, idx, nyears = nyr, nsq = nsq,
  fmodel=fmod, qmodel=qmod, srmodel=srmod, n1model=n1mod)

# EXTRACT retro FLStocks and hindcast FLIndices
retro_stks <- hind$stocks
hind_idx <- hind$indices

# CHECK Mohn's rho
mohn(mohnMatrix(retro_stks, 'ssb'))
mohn(mohnMatrix(retro_stks, 'fbar'))
mohn(mohnMatrix(retro_stks, 'rec'))

# -- DIAGNOSE model

# CHECK convergence
fitSumm(fit)['convergence',]
fitSumm(fit)['maxgrad',]

# COMPUTE residuals
resid <- residuals(fit, stk, idx)

# COMPUTE catch diagnostics
diags_catch <- computeCatchDiagnostics(fit, stk)

# RUN simulation
sim <- simulate(fit, its)

# -- SAVE model output

save(stk, idx, fit, resid, hind_idx, retro_stks, sim, diags_catch,
  file = "model/model.rda", compress = "xz")
