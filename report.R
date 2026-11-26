# report.R - Prepare plots and tables for report
# a4a_taf_template/report.R

# Authors: Ernesto JARDIM <ernesto.jardim@ipma.pt>
#         Colin MILLAR <colin.millar@ices.dk>
#         Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# Before:
# After:

library(TAF)
mkdir("report")

library(FLa4a)
library(ggplotFL)
library(patchwork)
library(ggrepel)
library(icesAdvice)

# LOAD model results
load("model/model.rda")

# SETTINGS
y0 <- dims(stk)$minyear
dy <- dims(stk)$maxyear

# -- data {{{ ----

load("data/data.rda")

# PLOT bubble catch.n(data_catchnb)
taf.png("data_catchnb.png")
print(ggplot(catch.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE, fill='lightgrey', alpha=0.8) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Catch (", units(catch.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1)))
dev.off()

# PLOT bubble landings.n(data_landingsnb)
taf.png("data_landingsnb.png")
print(ggplot(landings.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE, fill='lightgrey', alpha=0.8) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Landings (", units(landings.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1)))
dev.off()

# PLOT bubble discards.n(data_discardsnb)
taf.png("data_discardsnb.png")
print(ggplot(discards.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE, fill='lightgrey', alpha=0.8) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Discards (", units(discards.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1)))
dev.off()

# PLOT discard proportion at age (data_discardsp)
taf.png("data_discardsp.png")
print(ggplot(window(discards.n(stk), start=2002) /
  window(catch.n(stk), start=2002),
  aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data), fill=data), shape=21, na.rm=TRUE) +
  scale_size_continuous(range = c(0.1, 8), name="p") +
  scale_fill_gradient(low = "white", high = "gray50", name="p", guide = "legend") +
  ylab(paste0("Proportion discarded")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1)))
dev.off()

# PLOT discard ratio (data_discardratio)
taf.png("data_discardratio.png")
print(ggplot(window(discards.n(stk) / catch.n(stk), start=y0),
  aes(x=year, y=data, colour=factor(age))) + geom_line() +
  geom_label_repel(data=function(d) d[d$year == dy, ], aes(label=age),
    colour="black") +
  guides(colour="none") + ylab("Discard ratio (discards/catch)") +
  xlab("") + ylab(""))
dev.off()

# Time series of catch by cohort (data_catchcoh)
taf.png("data_catchcoh.png")
print(ggplot(as.data.frame(FLCohort(catch.n(stk))), aes(x=cohort, y=data, group=age)) +
  geom_line(aes(colour=factor(age))) +
  geom_label_repel(data=function(d) d[d$cohort + d$age == dy, ], 
    aes(label=cohort), colour="black") +
  xlab("Cohort") + ylab("Catch (thousands)") +
  theme(legend.position="none"))
dev.off()

# Time series of catch by cohort facetted (data_catchcoh_facets)
taf.png("data_catchcoh_facets.png")
print(ggplot(as.data.frame(FLCohort(catch.n(stk))), aes(x=cohort, y=data, group=age)) +
  geom_line(aes(colour=factor(age))) +
  facet_grid(age~., scales="free") +
  xlab("") + ylab("Catch (thousands)") +
  theme(legend.position="none"))
dev.off()

# }}}

# -- model {{{ ----

# PLOT residuals



# }}}

# -- output {{{ ----

load("output/output.rda")

# PLOT model run
taf.png("model_stock.png")
print(plot(run))
dev.off()

taf.png("model_stock_uncertainty.png")
print(plot(run_sim))
dev.off()

# CHECK changes in CV by year
plot(iterCVs(ssb(run_sim)) %/% iterCVs(ssb(run_sim))[,1]) +
  geom_hline(yintercept=1, linetype="dashed", color="red") +
  ylab("CV ratio")

plot(iterCVs(rec(run_sim)) %/% iterCVs(rec(run_sim))[,1]) +
  geom_hline(yintercept=1, linetype="dashed", color="red") +
  ylab("CV ratio")

# PLOT retrospective results
taf.png("model_retro.png")
print(plot(run_sim, retros[-1], metrics=list(SSB=ssb, `bar(F)`=fbar, Recruits=rec)) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_colour_flr(labels=c(paste(dy, '(final)'),
    seq(dy - 1, dy - length(retros) +  1))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)),
    labels=scales::label_comma()) +
  geom_text(data=data.frame(qname=factor(c("Rec", "SSB", "bar(F)"),
    levels=c("Rec", "SSB", "bar(F)")),
    label=paste0("\"Mohn\"*\"'\"*\"s\"~rho == ", c(
      round(mohn(mohnMatrix(retros, 'rec')), 2),
      round(mohn(mohnMatrix(retros, 'ssb')), 2),
      round(mohn(mohnMatrix(retros, 'fbar')), 2)))),
    aes(x=Inf, y=Inf, label=label),
    vjust=1.5, hjust=1.1, size=4, parse=TRUE,
    inherit.aes = FALSE) +
    theme(legend.box.spacing=unit(2, "pt"),
      legend.margin=margin(0, 0, 0, 0)))
dev.off()

# PLOT hindcasting indices
taf.png("model_hindcast_indices.png")
print(plotXval(idx, hind_idx) +
  guides(colour = guide_legend(nrow = 1)))
dev.off()

# }}}
