# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 07, 2011
# Last modified: Apr 19, 2012
# Purpose:       combine paleo and iucn rates/risk and plot them for
# fig 2 of the review paper
# ====================================================================

rm(list = ls())
source("read.iucn.data.R")
iucn.dat <- read.iucn.dat("data/export-23825-marine-system-species.csv")

rel <- read.csv("paleo-iucn-relation.csv", stringsAsFactors = FALSE, header = TRUE)

out <- list()
for(i in 1:nrow(rel)) {
  if(nrow(subset(iucn.dat, eval(parse(text = rel$iucn.search.string[i])))) > 0) {
  out[[i]] <- subset(iucn.dat, eval(parse(text = rel$iucn.search.string[i])))
  out[[i]]$label <- rel[i,]$id
  }
  #out[[i]]$grouping <- rel[i,]$grouping
}
out <- do.call("rbind", out)

status.names <- data.frame(red.list.status = c("LC", "DD", "VU", "NT", "EN", "CR", "EX", "EW", "LR/nt", "LR/lc"), status.long = c("Least concern", "Data deficient", "Vulnerable", "Near threatened", "Endangered", "Critically endangered", "Extinct", "Extinct in the wild", "Lower risk/near threatened", "Lower risk/least concern"), extinct.scale = c(7, 9, 5, 6, 4, 3, 1, 2, 6, 8))

# for now, make the "lower risk" categories the same as the others:
status.names[status.names$red.list.status == "LR/lc", "status.long"] <- "Least concern"
status.names[status.names$red.list.status == "LR/nt", "status.long"] <- "Near threatened"
status.names[status.names$red.list.status == "LR/lc", "extinct.scale"] <- status.names[status.names$red.list.status == "LC", "extinct.scale"]
status.names[status.names$red.list.status == "LR/nt", "extinct.scale"] <- status.names[status.names$red.list.status == "NT", "extinct.scale"]

iucn.dat2 <- merge(out, status.names)

library(plyr)
iucn.summary <- ddply(iucn.dat2, "label", function(x) {
data.frame(
      extinct = nrow(subset(x, red.list.status %in% c("EX", "EW"))),
      endangered = nrow(subset(x, red.list.status %in% c("CR", "EN"))),
      vulnerable = nrow(subset(x, red.list.status %in% c("VU"))),
      dd = nrow(subset(x, red.list.status %in% c("DD"))),
      n.with.DD = nrow(x),
      n = nrow(subset(x, red.list.status != "DD"))
      #grouping = unique(x$grouping)
)})

# these are now in the historical data:
iucn.summary$extinct <- 0

#### begin substitute sea bird data:
# substitute in the bird data from BirdLife international:
aves<-read.table("Seabirds2011.dat", sep = "\t", header = TRUE, stringsAsFactors = FALSE, comment.char = "#", col.names = c("SciName", "ComName", "IUCNCat", "spID"))
aves.summary<-ddply(aves, "IUCNCat", summarize, freq = length(spID))

iucn.summary[iucn.summary$label == "Aves", "dd"] <- aves.summary[aves.summary$IUCNCat == "DD", "freq"]

iucn.summary[iucn.summary$label == "Aves", "endangered"] <- aves.summary[aves.summary$IUCNCat == "EN", "freq"] + aves.summary[aves.summary$IUCNCat == "CR", "freq"]
iucn.summary[iucn.summary$label == "Aves", "extinct"] <- aves.summary[aves.summary$IUCNCat == "EX", "freq"]
iucn.summary[iucn.summary$label == "Aves", "vulnerable"] <- aves.summary[aves.summary$IUCNCat == "VU", "freq"]

iucn.summary[iucn.summary$label == "Aves", "n"] <- sum(subset(aves.summary, IUCNCat != "DD")$freq)
iucn.summary[iucn.summary$label == "Aves", "n.with.DD"] <- sum(aves.summary$freq)

##### end seabirds

paleo <- read.csv("paleo-extinction-rates.csv", stringsAsFactors = FALSE)
names(paleo) <- tolower(names(paleo))
names(paleo)[1] <- "label"

dat <- merge(iucn.summary, paleo, all = TRUE)
dat[is.na(dat$n), "n"] <- 0

dat <- dat[order(dat$n), ]

dat.scaled <- transform(dat, endangered = endangered/n, dd = dd/n, extinct = extinct/n, vulnerable = vulnerable/n, endangered.plus.vulnerable = endangered / n + vulnerable / n, endangered.plus.extinct = endangered / n + extinct / n, end.plus.ext.l = endangered/n.with.DD + extinct/n.with.DD, end.plus.ext.u = (endangered + dd)/n.with.DD + extinct/n.with.DD)

dat.scaled <- rbind(dat.scaled[nrow(dat.scaled):(nrow(dat.scaled)-1), ], dat.scaled[-((nrow(dat.scaled):(nrow(dat.scaled)-1))), ])

### grouping and ordering:
names(rel)[1] <- "label"
dat.scaled <- merge(dat.scaled, rel[,c("label","kingdom","grouping","plot.order")])
dat.scaled <- transform(dat.scaled, n.prop = log(n / max(n) + 0.01))
dat.scaled$n.prop <- dat.scaled$n.prop/3
dat.scaled$n.prop <- dat.scaled$n.prop - min(dat.scaled$n.prop)
dat.scaled$n.prop <- dat.scaled$n.prop / (max(dat.scaled$n.prop) + 0.5)
dat.scaled$n.prop <- dat.scaled$n.prop + (1 - max(dat.scaled$n.prop))

###
# bring in PBDB rates:
pbdb.rates <- read.csv("data/pbdb.rates.csv")
#pbdb.rates <- subset(pbdb.rates, Class != "All taxa")
pbdb.rates <- subset(pbdb.rates, Class != "Pinnipedia") # now using Carnivora
pbdb.rates <- subset(pbdb.rates, !is.na(BC.Extinction))
pbdb.rates$Class <- factor(pbdb.rates$Class)

pbdb.rates <- ddply(pbdb.rates, "Class", summarize, median.ext = median(BC.Extinction, na.rm = T), ext.0.25 = quantile(BC.Extinction, 0.25, na.rm = TRUE), ext.0.75 = quantile(BC.Extinction, 0.75, na.rm = TRUE))   
pbdb.rates$label <- pbdb.rates$Class
pbdb.rates$Class <- NULL

dat.scaled <- merge(dat.scaled, pbdb.rates, all = TRUE)

##
## bring in historical data:
source("historical.R")
# add Omphalotropis plicosa a globally extinct gastropod from IUCN:
global_table[global_table$label == "Gasteropoda", "global_extinctions"] <- global_table[global_table$label == "Gasteropoda", "global_extinctions"] + 1
# add Prototroctes oxyrhynchus a globally extinct fish:
global_table[global_table$label == "Osteichthyes", "global_extinctions"] <- global_table[global_table$label == "Osteichthyes", "global_extinctions"] + 1

dat.scaled <- merge(dat.scaled, global_table, all = TRUE)
dat.scaled <- merge(dat.scaled, local_table, all = TRUE)
dat.scaled$local_extinctions[is.na(dat.scaled$local_extinctions)] <- 0
dat.scaled$global_extinctions[is.na(dat.scaled$global_extinctions)] <- 0
dat.scaled$extinct[is.na(dat.scaled$extinct)] <- 0
dat.scaled$endangered[is.na(dat.scaled$endangered)] <- 0
dat.scaled$vulnerable[is.na(dat.scaled$vulnerable)] <- 0
dat.scaled$dd[is.na(dat.scaled$dd)] <- 0

# add these by hand since they're only in the historical extinctions:
dat.scaled[dat.scaled$label == "Siphonocladophyceae", "grouping"] <- "Plants"
dat.scaled[dat.scaled$label == "Siphonocladophyceae", "n"] <- 0
dat.scaled[dat.scaled$label == "Siphonocladophyceae", "n.with.DD"] <- 0
dat.scaled[dat.scaled$label == "Bryozoa", "n.with.DD"] <- 0
dat.scaled[dat.scaled$label == "Brachiopoda", "n.with.DD"] <- 0

dat.scaled <- merge(dat.scaled, rel[,c("label","plot.order")], all.x = TRUE)
dat.scaled[dat.scaled$label == "Siphonocladophyceae", "plot.order"] <- 3

# * for those that are highly data deficient:
dat.scaled <- adply(dat.scaled, 1, summarize, dd.high = ifelse(dd > 0.5 & !is.na(dd), "*", ""))

grouping.order <- data.frame(grouping = c("Mammals", "Vertebrates", "Invertebrates", "Plants"), grouping.order = c(4, 3, 2, 1))
dat.scaled <- merge(dat.scaled, grouping.order)
dat.scaled <- transform(dat.scaled, grouping = reorder(grouping, grouping.order))

# order the data:
dat.scaled$endangered.plus.extinct.fake <- dat.scaled$endangered.plus.extinct
dat.scaled[is.na(dat.scaled$endangered.plus.extinct.fake), "endangered.plus.extinct.fake"] <- -99
dat.scaled[dat.scaled$label == "Echinoidea", "endangered.plus.extinct.fake"] <- -98
#dat.scaled <- dat.scaled[order(dat.scaled$grouping, dat.scaled$endangered.plus.extinct.fake), ]
# or by hand:
dat.scaled <- dat.scaled[order(dat.scaled$grouping, -dat.scaled$plot.order), ]


#dat.scaled <- rbind(subset(dat.scaled, label %in% c("Brachiopoda", "Bryozoa")), subset(dat.scaled, !label %in% c("Brachiopoda", "Bryozoa")))
#dat.scaled <- rbind(subset(dat.scaled, label %in% c("Carnivora", "Bryozoa")), subset(dat.scaled, !label %in% c("Brachiopoda", "Bryozoa")))


library(RColorBrewer)
pal = brewer.pal(3, "Set1")
pal[1] <- "blue"
pal[1] <- "#317cff"
pal[2] <- "grey50"
pal[3] <- "red"

pal[3] <- "#ff4c1a"
pal[3] <- "#000000"

bg.plot <- function(colour = "#00000010") rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = colour, border = FALSE)

# remove ones with no data:
dat.scaled <- subset(dat.scaled, !label %in% c("Polypodiopsida", "Merostomata"))

row.names(dat.scaled) <- NULL
### HARDCODED ORDERING OF THE ECHINODERM AND BIVALVE ROWS!!!!!!!
#biv.temp <- dat.scaled[3, ]
#echin.temp <- dat.scaled[4, ]
#dat.scaled[4, ] <- biv.temp
#dat.scaled[3, ] <- echin.temp

pdf("fig2_apr19.pdf", width = 5.20, height = 4.0)
par(mfrow = c(1, 3))
par(mar = c(0,0,0,0))
par(cex = 0.7)
par(mgp = c(2, 0.40, 0)) # title and axis label distances - make them closer
par(oma = c(2.6, 9, 2, 4))
par(tck = -0.02) # shorten the tick length

dat.scaled$y.pos <- 1:nrow(dat.scaled)
#dat.scaled$y.pos[20:23] <- dat.scaled$y.pos[20:24] + 0.5
dat.scaled[dat.scaled$grouping.order == 4, "y.pos"] <- dat.scaled[dat.scaled$grouping.order == 4, "y.pos"] + 1.5
dat.scaled[dat.scaled$grouping.order == 3, "y.pos"] <- dat.scaled[dat.scaled$grouping.order == 3, "y.pos"] + 1
dat.scaled[dat.scaled$grouping.order == 2, "y.pos"] <- dat.scaled[dat.scaled$grouping.order == 2, "y.pos"] + 0.5

## the fossil panel:
plot(1, 1, xlim = log(c(0.004, max(dat.scaled$ext.0.75, na.rm = TRUE)* 1.94)), ylim = range(dat.scaled$y.pos), type = "n", axes = F, xlab = "", ylab = "", xaxs = "i")
bg.plot(colour = "grey97")
box(bty = "o", col = "grey70")
par(xpd = FALSE)
abline(h = dat.scaled$y.pos, col = "grey90")
par(xpd = NA)
par(las = 2)
axis(2, col = "grey70", at = dat.scaled$y.pos, labels = paste(dat.scaled$label, sep = ""), col.axis = "grey45")

par(las = 0)
par(xpd = NA)
axis.pos <- c(0.004, 0.02, 0.1, 0.5)
axis(1, col = "grey70", at = log(c(0.004, axis.pos)), labels = c(0, axis.pos), col.axis = "grey45", cex.axis = 1)
# fake the addition of a 0 on the x-axis

with(dat.scaled, segments(log(ext.0.25 + 0.004), y.pos, log(ext.0.75), y.pos, col = "black"))
with(dat.scaled, points(log(median.ext+ 0.004), y.pos, pch = 19, col = "grey30", cex = 0.9))

par(xpd = NA)
mtext("Median extinction rate", side = 1, line =1.6, cex = 0.7, col = "grey30")

par(las = 2)
axis(2, at = max(dat.scaled$y.pos) + 1.28, label = "Taxon", lwd = 0, col.axis = "grey30")
par(las = 0)

# labels at top:
par(xpd = NA)
#mtext("(a) Fossil", side = 3, line = 0.1, cex = 0.7, adj = 0.05, col = "grey30")
mtext(expression(paste(bold("(a)"), " Fossil")), side = 3, line = 0.1, cex = 0.7, adj = 0.05, col = "grey30")


## the historical panel:
plot(1, 1, xlim = c(0, 38), ylim = range(dat.scaled$y.pos), type = "n", axes = F, xlab = "", ylab = "", xaxs = "i")
bg.plot(colour = "grey97")
box(bty = "o", col = "grey70")
par(xpd = FALSE)
abline(h = dat.scaled$y.pos, col = "grey90")
par(xpd = NA)
par(las = 2)
#axis(2, col = "grey70", at = 1:nrow(dat.scaled), labels = paste(dat.scaled$label, sep = ""), col.axis = "grey35")

par(las = 0)
par(xpd = NA)
axis.pos <- c(0, 10, 20, 30)
axis(1, col = "grey70", at = axis.pos, labels = axis.pos, col.axis = "grey35", cex.axis = 1)

# the actual data plotting:
with(dat.scaled, rect(0, y.pos- 0.3, global_extinctions, y.pos + 0.3, border = FALSE, col = "grey20"))
with(dat.scaled, rect(global_extinctions, y.pos- 0.3, global_extinctions + local_extinctions, y.pos + 0.3, border = FALSE, col = "grey60"))

par(xpd = NA)
mtext("Extinctions", side = 1, line =1.6, cex = 0.7, col = "grey30")

# labels at top:
par(xpd = NA)
#mtext("(b) Historical", side = 3, line = 0.1, cex = 0.7, adj = 0.05, col = "grey30")
mtext(expression(paste(bold("(b)"), " Historical")), side = 3, line = 0.1, cex = 0.7, adj = 0.05, col = "grey30")



## legend:
leg.element <- function(x = 22, y , label, col) {
  rect(x, y-0.12, x +  2, y + 0.28, col = col, border = NA)
  text(x + 1.03, y, label, pos = 4, col = "grey35", cex = 0.8)
}

rect(20, 0.225, 38, 2.0,  border= "grey75", col = "grey95")
leg.element(y =1.4, label = "Global", col = "grey25")
leg.element(y = 0.7, label = "Local", col = "grey60")
col.ext <- "grey35" # for the modern iucn panel


## the iucn panel:
plot(1, 1, xlim = c(0, 1.0), ylim = range(dat.scaled$y.pos), type = "n", axes = F, xlab = "", ylab = "", xaxs = "i")
bg.plot(colour = "grey97")
par(xpd = FALSE)
abline(h = dat.scaled$y.pos, col = "grey90")
par(xpd = NA)
#col.end <- paste("#FF0000", round(dat.scaled$n.prop, 2)* 100 - 1, sep = "")
#col.vul <- as.character(paste("#D0BD00", round(dat.scaled$n.prop, 2)* 100 - 1, sep = ""))
col <- brewer.pal(6, "Set1")
col.end <- col[1]
col.vul <- col[5]
col.end <- "#E73123"
col.vul <- "#E7C823"
box(bty = "o", col = "grey70")
par(xpd = FALSE)
par(xpd = NA)
axis(1, col = "grey70", at = seq(0, 1.0, 0.25), labels = seq(0, 1.0, 0.25), col.axis = "grey45")

par(xpd = FALSE)
with(dat.scaled, rect(rep(0, nrow(dat.scaled)), y.pos- 0.3, extinct, y.pos + 0.3, border = FALSE, col =col.ext))

with(dat.scaled, rect(extinct, y.pos - 0.3, extinct + endangered, y.pos + 0.3, border = FALSE, col = col.end))

with(dat.scaled, rect(endangered + extinct, y.pos- 0.3, extinct + endangered + vulnerable, y.pos + 0.3, border = FALSE, col =col.vul))

## iucn numbers on the right

#box(col = "grey70")
# add confidence intervals with and without DD
#with(dat.scaled, segments(end.plus.ext.l, y.pos, end.plus.ext.u, y.pos, col = "grey25"))
#with(dat.scaled, segments(end.plus.ext.l, y.pos, endangered+extinct, y.pos, col = "grey90", lwd = 1.8))
#with(dat.scaled, points(endangered + extinct, y.pos, col = col.end, pch = 20, cex = 0.6))
par(xpd = FALSE)

par(xpd = NA)
par(las = 0)
mtext("Fraction of species", side = 1, line =1.6, cex = 0.7, col = "grey30")

par(las = 1)
axis(4, col = "grey70", at = dat.scaled$y.pos, labels = paste(dat.scaled$n.with.DD, dat.scaled$dd.high, sep = ""), col.axis = "grey45")
axis(4, at = max(dat.scaled$y.pos) + 1.4, label = "Species\nassessed", lwd = 0, col.axis = "grey30")

with(subset(dat.scaled, endangered.plus.vulnerable != 0 & !is.na(endangered.plus.vulnerable)), segments(end.plus.ext.l, y.pos, end.plus.ext.u, y.pos, col = "grey25", lwd = 0.9))
par(xpd = NA)
with(subset(dat.scaled, endangered.plus.vulnerable != 0 & !is.na(endangered.plus.vulnerable)), points(endangered + extinct, y.pos, col = "grey30", pch = 20, cex = 0.7))


# labels at top:
par(xpd = NA)
mtext(expression(paste(bold("(c)"), " Modern")), side = 3, line = 0.1, cex = 0.7, adj = 0.05, col = "grey30")

## legend:
leg.element <- function(x = 0.35, y , label, col) {
  rect(x, y-0.12, x +  0.055, y + 0.28, col = col, border = NA)
  text(x + 0.03, y, label, pos = 4, col = "grey35", cex = 0.8)
}

rect(0.3, 0.225, 1.0, 2.00,  border= "grey75", col = "grey95")
#leg.element(y =2.1, label = "Extinct", col =col.ext)
leg.element(y =1.4, label = "Endangered", col = col.end)
leg.element(y = 0.7, label = "Vulnerable", col = col.vul)

#segments(-2, 19.75, 1, 19.75, col = "white", lwd = 3)
#segments(-2, 19.65, 1, 19.65, col = "grey80", lwd = 1)
#segments(-2, 19.85, 1, 19.85, col = "grey80", lwd = 1)

add_splitting_line <- function(ypos) {
### The white splitting line:
par(xpd = NA)
segments(-2, ypos-0.25, 1, ypos-0.25, col = "white", lwd = 3)
segments(-2, ypos-0.35, 1, ypos-0.35, col = "grey80", lwd = 1)
segments(-2, ypos-0.15, 1, ypos-0.15, col = "grey80", lwd = 1)
par(xpd = FALSE)
}

for(i in (c(dat.scaled$y.pos)[diff(as.numeric(dat.scaled$grouping))==1])+1) add_splitting_line(i)

dev.off()

