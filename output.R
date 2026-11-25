# output.R - Extract results of interest, write tables
# a4a_taf_template/output.R

# Authors: Ernesto JARDIM <ernesto.jardim@ipma.pt>
#          Colin MILLAR <colin.millar@ices.dk>
#          Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# Before: data/data.rda, model/model.rda
# After: output/output.rda

library(TAF)
mkdir("output")

library(flextable)
library(FLa4a)
library(FLWG)

# LOAD model results
load("model/model.rda")
load("model/forecast.rda")

# SET dims
y0 <- dims(run)$minyear
y <- dims(run)$maxyear
dy <- ac(y)
iy <- ac(y + 1)
ay <- ac(y + 2)

# -- BUILD output

# RUN with input catch
run_input <- transform(stk, stock.n=stock.n(fit), harvest=harvest(fit))

# SIMULATION run
run_sim <- stk + sim

# SAVE output
save(run, run_input, run_sim, futs, refpts, file="output/output.rda", compress="xz")

# RUN SAG TODO: FIX years from sim and futs
# source("output_sag.R", echo=TRUE)

# --- TABLES {{{

mkdir("output/tables")

# -- Report tables

# SELECT metrics
mets <- list(catch_n=catch.n, landings_n=landings.n, stock_n=stock.n)

# EXTRACT metrics
run_metrics <- metrics(run, metrics=mets)

# COERCE to flextable
tables <- lapply(run_metrics, as_flextable)

# -- Advice sheet tables

# - Interim table
tables$interim <- interim_table(run)

# - Catch options table

# SELECT projections to include, 'advice' must be present
dat <- FLStocks(futs[c('advice', 'uFmsy', 'lFmsy', 'F0', 'Fpa',
  'MSYBtrigger', 'Blim', 'lastF')])

# SET table options#: ages, last year advice and TAC
tab <- catch_options(dat, tac=10000, advice=10696, ages=2:6, discards.ages=1:3)

# FORMAT options: footnotes
fmt <- format_catch_options(
  footnotes=list(list(i=1, j=c(2, 4, 5, 9, 11), part="header",
      ref_symbols=c("*", "**", "^", "^^", "##"),
      value=as_paragraph(c(
        "Differences between the total catch and the sum of projected landings and discards result from rounding.",
        "Including BMS landings, assuming average estimated discard rate by age 2020-2022.",
        "SSB 2026 relative to SSB 2025.",
        "Total catch in 2024 relative to the advice value 2024 and TAC (both 3675 tonnes).",
        "Bpa and MSY Btrigger cannot be achieved in 2025, even with zero catches."))),
  list(i=1, j=10, part="header", ref_symbols="#",
      value=as_paragraph("F", as_sub("projected landings"), " and F",
        as_sub("projected discards"), " do not add up to F", as_sub("total"),
        " as they are calculated using different ages."))))

# BUILD flextable
tables$catch_options <- flex_catch_options(tab, fmt)

# -- SAVE tables

save(tables, file="output/tables.rda", compress="xz")

# EXPORT tables to Word
lapply(names(tables), function(x) save_as_docx(tables[[x]],
  path=file.path('output', 'tables', paste0(x, '.docx'))))

# }}}


# -- SAVE FLStocks for WGMIXFISH

ple27420 <- FLStocks(input=run_input, prediction=run)

save(ple27420, file="output/ple27420_2026_WGMIXFISH.rda", compress="xz")
