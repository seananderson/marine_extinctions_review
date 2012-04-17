# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Apr 17, 2012
# Last modified: Apr 17, 2012
# Purpose:       bring in historical extinction data from dulvy et al.
# 2003 and 2009
# ====================================================================

localext <- read.table("~/Dropbox/NESCent-extinction/review/iucn/local_marine_extinctions_dulvy_etal_2003.txt", header = TRUE, sep = "\t", na.string = ".")
globalext <- d <- read.table("~/Dropbox/NESCent-extinction/review/iucn/global_marine_extinctions_dulvy_etal_2009.txt", header = TRUE, sep = "\t", na.string = ".")
