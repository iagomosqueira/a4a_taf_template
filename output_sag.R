# output_sag.R - Prepare and upload stock assessment group (SAG) data
# a4a_taf_template/output_sag.R

# Authors: Ernesto JARDIM <ernesto.jardim@ipma.pt>
#          Colin MILLAR <colin.millar@ices.dk>
#          Iago MOSQUEIRA <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# Before:
# After:

library(icesSAG)
library(FLasher)

# --- SAG

# GET advice forecast
fut <- futs$advice

# GET uncertainty metrics
metsci <- lapply(metrics(run_sim, metrics=list(Rec=rec, SSB=ssb, F=fbar)), 
  FLQuantPoint)

# INFO {{{
info <- stockInfo(StockCode="ple.27.420", AssessmentYear=iy,
  Purpose = "Advice", StockCategory=1, ContactPerson="name.surname@inst.co",
  
  # A: Age based (like VPA, SCA, SAM) 
  # CHECK: a4a code
  ModelType="A", ModelName="a4a",
  
  # rec
  RecruitmentDescription="Recruitment", RecruitmentAge=1,
  # NE3: Number of individuals in thousands (x1000) (fisheries) 
  RecruitmentUnits="NE3",
  
  # ssb
  StockSizeDescription="Spawning Stock Biomass",
  StockSizeUnits="t",
  
  # catch
  CatchesLandingsUnits="t",
  # F
  FishingPressureDescription="F",
  FishingPressureUnits="",
  # CIs
  ConfidenceIntervalDefinition="2*sd",

  # REFPTS

  FMSY=c(refpts$Fmsy),
  MSYBtrigger=c(refpts$Btrigger),
  
  Blim=c(refpts$Blim),
  #Flim=c(refpts$Flim),
  
  Bpa=c(refpts$Bpa),
  Fpa=c(refpts$Fpa),
  
  # MAP
  FMGT=c(refpts$Fmsy),
  FMGT_lower=c(refpts$lFmsy),
  FMGT_upper=c(refpts$uFmsy),
  BMGT=c(refpts$Btrigger)
)
# }}}

# DEBUG
info$StockCategory <- 1

# DATA
fishdata <- stockFishdata(seq(y0, iy))

# LAST year
n <- length(fishdata$Year)
# YEARS 1:n
i <- seq(n - 1)

# Recruitment
fishdata$Recruitment[i] <- c(median(metsci$Rec))
fishdata$Recruitment[n] <- c(iter(rec(fut)[, ac(iy)], 1))
fishdata$Low_Recruitment[i] <- c(lowq(metsci$Rec))
fishdata$High_Recruitment[i] <- c(uppq(metsci$Rec))

# TBiomass
fishdata$TBiomass[i] <- c(stock(run))

# StockSize: ssb(fit)
fishdata$StockSize <- c(mean(metsci$SSB))
fishdata$Low_StockSize <- c(lowq(metsci$SSB))
fishdata$High_StockSize <- c(uppq(metsci$SSB))

# StockSize iy: sagfut
# fishdata$StockSize[n] <- c(ssb(fut)[, ac(iy)])

# Landings (obs)
fishdata$Landings[i] <- c(landings(stk))

# Discards (model + obs)
# Observed discards
fishdata$Discards[i] <- c(discards(stk))

# Catches (obs)
fishdata$Catches[i] <- c(catch(stk))

# FishingPressure
fishdata$FishingPressure[i] <- c(mean(metsci$F)[,-n])
fishdata$Low_FishingPressure[i] <- c(lowq(metsci$F)[,-n])
fishdata$High_FishingPressure[i] <- c(uppq(metsci$F)[,-n])
# TODO: fishdata$FishingPressure_Landings[i] <- c(Fwanted(fit))
# TODO: fishdata$FishingPressure_Discards[i] <- c(Funwanted(fit))

# XML

xmlfile <- createSAGxml(info, fishdata)

# SAVE to file
cat(xmlfile, file="output/ple_27_420.xml")

# UPLOAD 
# options(icesSAG.use_token = TRUE)
# key <- icesSAG::uploadStock(info, fishdata)
# https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx

# CHECK SAG
# findAssessmentKey('ple.27.420', year=iy, published = TRUE)
