# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Mar 22, 2011
# Last modified: Jul 07, 2011
# Purpose:       all steps to manipulate, output, and plot the IUCN marine data
# ====================================================================

source("read.iucn.data.R")
iucn.dat <- read.iucn.dat("data/export-23825-marine-system-species.csv")

source("combine.paleo.iucn.risk.barplots2.R")

# source("plot.freq.assessment.by.year.R")
# source("plot.status.by.class.R")
# 
# source("plot.criteria.R")
