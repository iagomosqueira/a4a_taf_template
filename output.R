# output.R - Extract results of interest, write tables
# a4a_taf_template/output.R

# Author: Ernesto JARDIM <ernesto.jardim@ipma.pt>
#         Colin MILLAR <colin.millar@ices.dk>
#         Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# Before: data/data.rda, model/model.rda
# After: output/output.rda

library(TAF)
mkdir("output")

library(FLa4a)

# LOAD model results
load("model/model.rda")

# -- UPDATE stk

# RUN with estimated catch
run <- stk + fit

# RUN with input catch
run_input <- transform(stk, stock.n=stock.n(fit), harvest=harvest(fit))

# SIMULATION run
run_sim <- stk + sim

# SAVE output
save(run, run_input, run_sim, file="output/output.rda", compress="xz")

# -- CREATE report tables

# -- SAVE FLStocks for WGMIXFISH

ple27420 <- FLStocks(input=run_input, prediction=run)

save(ple27420, file="output/ple27420_2026_WGMIXFISH.rda", compress="xz")
