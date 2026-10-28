# model.R - Run forecast
# 2024_sol.27.4_forecast/model.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLasher)

set.seed(697)

# LOAD model results and refpts

load("model/model.rda")
load("boot/data/refpts.rda")


# --- SETUP assumptions

# TAC current year (2025)
tac <- FLQuant(10000, dimnames=list(age='all', year=2025), units="tonnes")

# Advice current year (2025)
advice <- FLQuant(10696, dimnames=list(age='all', year=2025), units="tonnes")

# data, intermediate, advice and final YEARS
dy <- dims(run)$maxyear
iy <- dy + 1
ay <- iy + 1
fy <- ay + 1

# GEOMEAN recruitment but last year
rec0gm <- exp(mean(log(window(stock.n(run)["0",], end=-1))))

# GENERATE targets from refpts
targets <- expand(as(refpts, 'FLQuant'), year=ay)


# --- SETUP future

# EXTEND for 3 years, 5 year mean wts/selex, 3 year mean discards
fut <- stf(run, nyears=3, wts.nyears=5, fbar.nyears=5, disc.nyears=3)

# SET geomean SRR
gmsrr <- predictModel(model=rec~a, params=FLPar(c(rec0gm), units="thousands",
  dimnames=list(params="a", year=seq(iy, length=3), iter=1)))
gmsrr <- FLQuant(rec0gm, dimnames=list(age=0, year=2025:2027))


# --- INTERMEDIATE year assumption

# https://www.visned.nl/voor-vissers/update-overzicht-benutting-visquota-20-03-2024
# 2025                - 2024
# Tong 6.521 787 12 % - 3.222 765 24 % 

# C 2025 = TAC 2025 * 0.50
fut <- fwd(fut, sr=gmsrr, catch=tac * 0.50)

# F 2025 = F 2024
# fut <- fwd(fut, sr=gmsrr, fbar=expand(fbar(run)[, '2024'], year=2025))

# UPDATE 2025
fut <- fwd(fut, sr=gmsrr, fbar=targets["Fmsy",])


# --- PROJECT catch options

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


# --- PROJECT F levels

flevels <- seq(0, 0.50, by=0.01)

# BUILD control with targets as iters
control <- as(FLQuants(fbar=append(FLQuant(flevels,
  dimnames=list(age='all', year=ay, iter=seq(flevels))), F0[[1]])), "fwdControl")

# RUN
ffuts <- divide(fwd(fut, sr=gmsrr, control=control), names=flevels)

ffutsum <- Reduce(rbind, Map(function(x, y)
  cbind(F2026=y, model.frame(metrics(window(x, start=2020)), drop=TRUE)),
  x=ffuts, y=names(ffuts)))


# SAVE
save(ffuts, file="model/forecast_f_levels.rda", compress="xz")

write.csv(ffutsum, file="model/forecast_f_levels.csv")
