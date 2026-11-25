# data.R - Prepare data, write CSV data tables
# a4a_taf_template/data.R

# Authors: Ernesto JARDIM <ernesto.jardim@ipma.pt>
#         Colin MILLAR <colin.millar@ices.dk>
#         Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# Before:
# After:

library(TAF)
mkdir("data")

library(FLCore)

# LOAD example data

data(ple4)
data(ple4.indices)

# SUBSET
stk <- window(ple4, start=1990)
idx <- window(ple4.indices[c(2,3,5)], start=1990)

# BUILD (fake) refpts
refpts <- FLPar(Btrigger=483000, Fmsy=0.22, Blim=350000, Bpa=483000,
  Flim=0.30, Fpa=0.22, lFmsy=0.18, uFmsy=0.22)

# SAVE

save(stk, idx, refpts, file="data/data.rda", compress="xz")
