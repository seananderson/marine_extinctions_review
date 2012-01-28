# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 07, 2011
# Last modified: Jan 28, 2012
# Purpose:       combine paleo and iucn rates/risk and plot them for
# fig 2 of the review paper
# ====================================================================

source("read.iucn.data.R")
iucn.dat <- read.iucn.dat("data/export-23825-marine-system-species.csv")

rel <- read.csv("paleo-iucn-relation.csv", stringsAsFactors = FALSE, header = TRUE)

out <- list()
for(i in 3:nrow(rel)) {
  out[[i]] <- subset(iucn.dat, eval(parse(text = rel$iucn.search.string[i])))
  out[[i]]$label <- rel[i,]$id
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
)})

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
dat.scaled <- merge(dat.scaled, rel[,c("label","kingdom")])
dat.scaled <- transform(dat.scaled, n.prop = log(n / max(n) + 0.01))
dat.scaled$n.prop <- dat.scaled$n.prop/3
dat.scaled$n.prop <- dat.scaled$n.prop - min(dat.scaled$n.prop)
dat.scaled$n.prop <- dat.scaled$n.prop / (max(dat.scaled$n.prop) + 0.5)
dat.scaled$n.prop <- dat.scaled$n.prop + (1 - max(dat.scaled$n.prop))

###
# bring in PBDB rates:
pbdb.rates <- read.csv("data/pbdb.rates.csv", sep = " ")
pbdb.rates <- subset(pbdb.rates, Class != "All taxa")
pbdb.rates$Class <- factor(pbdb.rates$Class)

pbdb.rates <- ddply(pbdb.rates, "Class", summarize, median.ext = median(BC.Extinction, na.rm = T), ext.0.25 = quantile(BC.Extinction, 0.25, na.rm = TRUE), ext.0.75 = quantile(BC.Extinction, 0.75, na.rm = TRUE))   
pbdb.rates$label <- pbdb.rates$Class

dat.scaled <- merge(dat.scaled, pbdb.rates, all = TRUE)

###

# * for those that are highly data deficient:
dat.scaled <- adply(dat.scaled, 1, summarize, dd.high = ifelse(dd > 0.5 & !is.na(dd), "*", ""))

# order the data:
dat.scaled <- dat.scaled[order(dat.scaled$kingdom, dat.scaled$endangered.plus.extinct), ]
dat.scaled <- rbind(subset(dat.scaled, label %in% c("Brachiopoda", "Bryozoa")), subset(dat.scaled, !label %in% c("Brachiopoda", "Bryozoa")))

library(RColorBrewer)
pal = brewer.pal(3, "Set1")
pal[1] <- "blue"
pal[1] <- "#317cff"
pal[2] <- "grey50"
pal[3] <- "red"

pal[3] <- "#ff4c1a"
pal[3] <- "#000000"

bg.plot <- function(colour = "#00000010") rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = colour, border = FALSE)

# remove ones with no IUCN or paleo:
dat.scaled <- subset(dat.scaled, !label %in% c("Polypodiopsida", "Merostomata"))

row.names(dat.scaled) <- NULL
### HARDCODED ORDERING OF THE ECHINODERM AND BIVALVE ROWS!!!!!!!
biv.temp <- dat.scaled[3, ]
echin.temp <- dat.scaled[4, ]
dat.scaled[4, ] <- biv.temp
dat.scaled[3, ] <- echin.temp


pdf("iucn-paleo-rates-bar-jan28-DD.pdf", width = 4.15, height = 3.7)
par(mfrow = c(1, 2))
par(mar = c(0,0,0,0))
par(cex = 0.7)
par(mgp = c(2, 0.40, 0)) # title and axis label distances - make them closer
par(oma = c(2.6, 9, 2, 4))
par(tck = -0.02) # shorten the tick length

dat.scaled$y.pos <- 1:nrow(dat.scaled)
dat.scaled$y.pos[17:20] <- dat.scaled$y.pos[17:20] + 0.5

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
with(dat.scaled, points(log(median.ext+ 0.004), y.pos, pch = 19, col = "grey30"))

par(xpd = NA)
mtext("Median extinction rate", side = 1, line =1.6, cex = 0.7, col = "grey30")

par(las = 2)
axis(2, at = max(dat.scaled$y.pos) + 1.28, label = "Taxon", lwd = 0, col.axis = "grey30")
par(las = 0)

# labels at top:
par(xpd = NA)
mtext("(a) Fossil", side = 3, line = 0.1, cex = 0.7, adj = 0.05, col = "grey30")

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
with(dat.scaled, rect(rep(0, nrow(dat.scaled)), y.pos- 0.3, extinct, y.pos + 0.3, border = FALSE, col = "#5E5E5E"))

with(dat.scaled, rect(extinct, y.pos - 0.3, extinct + endangered, y.pos + 0.3, border = FALSE, col = col.end))

with(dat.scaled, rect(endangered + extinct, y.pos- 0.3, extinct + endangered + vulnerable, y.pos + 0.3, border = FALSE, col =col.vul))

box(col = "grey70")
# add confidence intervals with and without DD
#with(dat.scaled, segments(end.plus.ext.l, y.pos, end.plus.ext.u, y.pos, col = "grey25"))
with(dat.scaled, segments(end.plus.ext.l, y.pos, endangered+extinct, y.pos, col = "grey90", lwd = 1.8))
with(dat.scaled, segments(end.plus.ext.l, y.pos, end.plus.ext.u, y.pos, col = col.end, lwd = 0.9))
par(xpd = NA)
with(dat.scaled, points(endangered + extinct, y.pos, col = "grey30", pch = 20, cex = 0.7))
#with(dat.scaled, points(endangered + extinct, y.pos, col = col.end, pch = 20, cex = 0.6))
par(xpd = FALSE)

par(xpd = NA)
par(las = 0)
mtext("Fraction of species", side = 1, line =1.6, cex = 0.7, col = "grey30")

## iucn numbers on the right
par(las = 1)
axis(4, col = "grey70", at = dat.scaled$y.pos, labels = paste(dat.scaled$n, dat.scaled$dd.high, sep = ""), col.axis = "grey45")
axis(4, at = max(dat.scaled$y.pos) + 1.4, label = "Species\nassessed", lwd = 0, col.axis = "grey30")

# labels at top:
par(xpd = NA)
mtext("(b) Modern", side = 3, line = 0.1, cex = 0.7, adj = 0.05, col = "grey30")

## legend:
leg.element <- function(x = 0.35, y , label, col) {
  rect(x, y-0.12, x +  0.055, y + 0.28, col = col, border = NA)
  text(x + 0.03, y, label, pos = 4, col = "grey35", cex = 0.8)
}

rect(0.3, 0.225, 1.0, 2.70,  border= "grey75", col = "grey95")
leg.element(y =2.1, label = "Extinct", col = "#5E5E5E")
leg.element(y =1.4, label = "Endangered", col = col.end)
leg.element(y = 0.7, label = "Vulnerable", col = col.vul)

par(xpd = NA)
segments(-1, 16.75, 1, 16.75, col = "white", lwd = 3)
segments(-1, 16.65, 1, 16.65, col = "grey80", lwd = 1)
segments(-1, 16.85, 1, 16.85, col = "grey80", lwd = 1)

dev.off()

