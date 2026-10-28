# model_forecast.R - Run short-terem forecast
# a4a_taf_template/model_forecast.R

# Author: Ernesto JARDIM <ernesto.jardim@ipma.pt>
#         Colin MILLAR <colin.millar@ices.dk>
#         Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# Before: model/model.rda
# After:

library(FLasher)

# SETTINGS
set.seed(697)
dy <- dims(run)$maxyear
iy <- dy + 1
ay <- iy + 1
fy <- ay + 1

# LOAD model results and refpts

load("model/model.rda")

# NOTE: BUILD (fake) refpts
refpts <- FLPar(Btrigger=483000, Fmsy=0.22, Blim=350000, Bpa=483000,
  Flim=0.30, Fpa=0.22, lFmsy=0.18, uFmsy=0.22)

# -- SETUP assumptions

# TAC current year (iy)
tac <- FLQuant(110000, dimnames=list(age='all', year=iy), units="tonnes")

# Advice current year (iy)
advice <- FLQuant(110000, dimnames=list(age='all', year=iy), units="tonnes")

# GEOMEAN recruitment but last year
rec0gm <- exp(mean(log(window(rec(run), end=-1))))

# GENERATE targets from refpts
targets <- expand(as(refpts, 'FLQuant'), year=ay)

# -- SETUP future

# EXTEND for 3 years, 5 year mean wts/selex, 3 year mean discards
fut <- stf(run, nyears=3, wts.nyears=5, fbar.nyears=5, disc.nyears=3)

# SET future SRR: geomean all years but first
gmsrr <- FLQuant(rec0gm, dimnames=list(age=0, year=seq(iy, fy)))

# -- INTERMEDIATE year assumption

# (1) TAC full uptake: C iy = TAC iy
fut <- fwd(fut, sr=gmsrr, catch=tac)

# (2) Fsqo: F iy = F dy
# fut <- fwd(fut, sr=gmsrr, fbar=expand(fbar(run)[, ac(dy)], year=iy))

# UPDATE iy advice rule can compute
fut <- fwd(fut, sr=gmsrr, fbar=targets["Fmsy",])

# -- PROJECT catch options

# DEFINE catch options
catch_options <- list(

  # advice
  advice=FLQuants(fbar=targets["Fmsy",] *
    min((ssb(fut)[, ac(ay)] / refpts$Btrigger), 1)),

  # FMSY
  Fmsy=FLQuants(fbar=targets["Fmsy",]),

  # lowFMSY
  lFmsy=FLQuants(fbar=targets["lFmsy",]),

  # uppFMSY
  uFmsy=FLQuants(fbar=targets["uFmsy",]),

  # lowFMSYadvice
  lFmsyadvice=FLQuants(fbar=targets["lFmsy",] *
    min((ssb(fut)[, ac(ay)] / refpts$Btrigger), 1)),

  # F0
  F0=FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=ay))),

  # Fpa
  Fpa=FLQuants(fbar=targets["Fpa",]),

  # Flim
  Flim=FLQuants(fbar=targets["Flim",]),

  # Bpa
  Bpa=FLQuants(ssb_flash=targets["Bpa",]),

  # Blim
  Blim=FLQuants(ssb_flash=targets["Blim",]),

  # MSYBtrigger
  MSYBtrigger=FLQuants(ssb_flash=targets["Btrigger",]),

  # F iy
  lastF=FLQuants(fbar=expand(fbar(fut)[, ac(iy)], year=ay)),
                 
  # Fmp 0.20
  Fmp=FLQuants(fbar=FLQuant(0.20, dimnames=list(age='all', year=ay))),

  # TAC iy
  rotac=FLQuants(catch=expand(tac, year=ay)),

  # Advice iy
  roadv=FLQuants(catch=expand(advice, year=ay))
)

# C0, fy + 1
F0 <- FLQuants(fbar=FLQuant(0, dimnames=list(age='all', year=fy)))

# CONVERT to fwdControl: intermediate year + catch option + F0

fctls <- lapply(catch_options, function(x)
  as(FLQuants(c(x, F0)), "fwdControl")
)

# RUN!
futs <- FLStocks(lapply(fctls, function(x) fwd(fut, sr=gmsrr, control=x)))

# COMPARE
Map(compare, futs, fctls)

# CHECK advice ay
catch(futs$advice)[, ac(iy:ay)]

# SAVE
save(futs, file="model/forecast.rda", compress="xz")

# -- PROJECT at multiple F levels

flevels <- seq(0, 0.50, by=0.01)

# BUILD control with targets as iters
control <- as(FLQuants(fbar=append(FLQuant(flevels,
  dimnames=list(age='all', year=ay, iter=seq(flevels))), F0[[1]])), "fwdControl")

# RUN
ffuts <- divide(fwd(fut, sr=gmsrr, control=control), names=flevels)

ffutsum <- Reduce(rbind, Map(function(x, y)
  cbind(F2026=y, model.frame(metrics(window(x, start=2020)), drop=TRUE)),
  x=ffuts, y=names(ffuts)))


# - SAVE

save(ffuts, file="model/forecast_f_levels.rda", compress="xz")
write.csv(ffutsum, file="model/forecast_f_levels.csv")
