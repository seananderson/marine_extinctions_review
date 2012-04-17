# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Apr 17, 2012
# Last modified: Apr 17, 2012
# Purpose:       bring in historical extinction data from dulvy et al.
# 2003 and 2009
# ====================================================================

localext <- read.table("~/Dropbox/NESCent-extinction/review/iucn/local_marine_extinctions_dulvy_etal_2003.txt", header = TRUE, sep = "\t", na.string = ".")
globalext <-  read.table("~/Dropbox/NESCent-extinction/review/iucn/global_marine_extinctions_dulvy_etal_2009.txt", header = TRUE, sep = "\t", na.string = ".")

local_table <- as.data.frame(table(localext$group))
names(local_table) <- c("label", "local_extinctions")
local_table$kingdom <- "Animalia"
local_table$kingdom[local_table$label %in% c("Florideophyceae", "Siphonocladophyceae", "Phaeophyceae")] <- "Plantae" 

global_table <- as.data.frame(table(globalext$group))
names(global_table) <- c("label", "global_extinctions")
global_table$kingdom <- "Animalia"
global_table$kingdom[global_table$label %in% c("Florideophyceae", "Siphonocladophyceae", "Phaeophyceae")] <- "Plantae" 

