# report.R - Prepare plots and tables for report
# a4a_taf_template/report.R

# Author: Ernesto JARDIM <ernesto.jardim@ipma.pt>
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

# LOAD model results
load("model/model.rda")

# SETTINGS
iy <- dims(stk)$minyear
dy <- dims(stk)$maxyear

# -- data {{{ ----

load("data/data.rda")

# PLOT bubble catch.n(data_catchnb)
taf.png("data_catchnb.png")
ggplot(catch.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE, fill='lightgrey', alpha=0.8) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Catch (", units(catch.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# PLOT bubble landings.n(data_landingsnb)
taf.png("data_landingsnb.png")
ggplot(landings.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE, fill='lightgrey', alpha=0.8) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Landings (", units(landings.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# PLOT bubble discards.n(data_discardsnb)
taf.png("data_discardsnb.png")
ggplot(discards.n(stk), aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data)), shape=21, na.rm=TRUE, fill='lightgrey', alpha=0.8) +
  scale_size(range = c(0.1, 12)) +
  ylab(paste0("Discards (", units(discards.n(stk)),")")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# PLOT discard proportion at age (data_discardsp)
taf.png("data_discardsp.png")
ggplot(window(discards.n(stk), start=2002) /
  window(catch.n(stk), start=2002),
  aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(data), fill=data), shape=21, na.rm=TRUE) +
  scale_size_continuous(range = c(0.1, 8), name="p") +
  scale_fill_gradient(low = "white", high = "gray50", name="p", guide = "legend") +
  ylab(paste0("Proportion discarded")) + xlab("") +
  theme(legend.title=element_blank(), legend.position="bottom") +
  guides(size = guide_legend(nrow = 1))
dev.off()

# PLOT discard ratio (data_discardratio)
taf.png("data_discardratio.png")
ggplot(window(discards.n(stk) / catch.n(stk), start=iy),
  aes(x=year, y=data, colour=factor(age))) + geom_line() +
  geom_label_repel(data=function(d) d[d$year == dy, ], aes(label=age),
    colour="black") +
  guides(colour="none") + ylab("Discard ratio (discards/catch)") +
  xlab("") + ylab("")
dev.off()

# Time series of catch by cohort (data_catchcoh)
taf.png("data_catchcoh.png")
ggplot(as.data.frame(FLCohort(catch.n(stk))), aes(x=cohort, y=data, group=age)) +
  geom_line(aes(colour=factor(age))) +
  geom_label_repel(data=function(d) d[d$cohort + d$age == dy, ], 
    aes(label=cohort), colour="black") +
  xlab("Cohort") + ylab("Catch (thousands)") +
  theme(legend.position="none")
dev.off()

# Time series of catch by cohort facetted (data_catchcoh_facets)
taf.png("data_catchcoh_facets.png")
ggplot(as.data.frame(FLCohort(catch.n(stk))), aes(x=cohort, y=data, group=age)) +
  geom_line(aes(colour=factor(age))) +
  facet_grid(age~., scales="free") +
  xlab("") + ylab("Catch (thousands)") +
  theme(legend.position="none")
dev.off()

# }}}

# -- model {{{ ----

# PLOT residuals



# }}}

# -- output {{{ ----

# PLOT model run
taf.png("model_stock.png")
plot(run)
dev.off()

taf.png("model_stock_uncertainty.png")
plot(run_sim)
dev.off()

# NOTE: Is variance growing more than itn should?
# CHECK changes in CV by year
plot(iterCVs(ssb(run_sim)) %/% iterCVs(ssb(run_sim))[,1]) +
  geom_hline(yintercept=1, linetype="dashed", color="red") +
  ylab("CV ratio")

plot(iterCVs(rec(run_sim)) %/% iterCVs(rec(run_sim))[,1]) +
  geom_hline(yintercept=1, linetype="dashed", color="red") +
  ylab("CV ratio")

# PLOT retrospective results
taf.png("model_retro.png")
plot(run_sim, retro_stks[-1], metrics=list(Rec=rec, SSB=ssb, F=fbar)) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_color_discrete(labels=c(paste(dy, '(final)'),
    seq(dy - 1, dy - length(retro_stks) +  1))) +
  geom_text(data=data.frame(qname=c("Rec", "SSB", "F"),
    label=paste0("\"Mohn\"*\"'\"*\"s\"~rho == ", c(
    round(mohn(mohnMatrix(retro_stks, 'rec')), 2),
    round(mohn(mohnMatrix(retro_stks, 'ssb')), 2),
    round(mohn(mohnMatrix(retro_stks, 'fbar')), 2)))),
    aes(x=mean(c(iy, dy)),
      y=Inf, label=label),
    vjust=1.5, hjust=0.5, size=4, parse=TRUE,
    inherit.aes = FALSE)
dev.off()

# PLOT hindcasting indices
taf.png("model_hindcast_indices.png")
plotXval(idx, hind_idx) +
  guides(colour = guide_legend(nrow = 1))
dev.off()

# }}}
