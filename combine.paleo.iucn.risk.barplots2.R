# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 07, 2011
# Last modified: Apr 17, 2012
# Purpose:       combine paleo and iucn rates/risk and plot them for
# fig 2 of the review paper
# ====================================================================

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
                      #browser()
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

iucn.summary[iucn.summary$label == "Aves", "n"] <- sum(aves.summary$freq)

# TODO what about the extinct ones? there are 5 EX here.
##### end seabirds

paleo <- read.csv("paleo-extinction-rates.csv", stringsAsFactors = FALSE)
names(paleo) <- tolower(names(paleo))
names(paleo)[1] <- "label"

dat <- merge(iucn.summary, paleo, all = TRUE)
dat[is.na(dat$n), "n"] <- 0

dat <- dat[order(dat$n), ]

dat.scaled <- transform(dat, endangered = endangered/n, dd = dd/n, extinct = extinct/n, vulnerable = vulnerable/n, endangered.plus.vulnerable = endangered / n + vulnerable / n, endangered.plus.extinct = endangered / n + extinct / n)
#dat.scaled <- dat.scaled[order(dat.scaled$endangered), ]   

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

#ggplot(pbdb.rates, aes(BC.Extinction, Class))+ geom_point()
#junk <- ddply(pbdb.rates, "Class", summarize, median.ext = median(BC.Extinction, na.rm = T), ext.0.25 = quantile(BC.Extinction, 0.25, na.rm = TRUE), ext.0.75 = quantile(BC.Extinction, 0.75, na.rm = TRUE));ggplot(melt(junk), aes(value, Class, colour = variable)) + geom_point()

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

pdf("iucn-paleo-rates-bar-without-DD.pdf", width = 4.5, height = 4.4)
par(mfrow = c(1, 2))
par(mar = c(0,0,0,0))
par(cex = 0.7)
par(mgp = c(2, 0.4, 0)) # title and axis label distances - make them closer
par(oma = c(3.6, 9, 2, 4))
par(tck = -0.012) # shorten the tick length

## the fossil panel:
plot(1, 1, xlim = log(c(0.004, max(dat.scaled$ext.0.75, na.rm = TRUE)* 1.71)), ylim = c(1, nrow(dat.scaled)), type = "n", axes = F, xlab = "", ylab = "", xaxs = "i")
bg.plot(colour = "grey97")
box(bty = "o", col = "grey70")
par(xpd = FALSE)
abline(h = 1:nrow(dat.scaled), col = "grey90")
par(xpd = NA)
par(las = 2)
axis(2, col = "grey70", at = 1:nrow(dat.scaled), labels = paste(dat.scaled$label, sep = ""), col.axis = "grey35")

par(las = 0)
par(xpd = NA)
axis.pos <- c(0.005, 0.02, 0.1, 0.5)
axis(1, col = "grey70", at = log(axis.pos), labels = axis.pos, col.axis = "grey35", cex.axis = 1)

with(dat.scaled, segments(log(ext.0.25 + 0.004), 1:nrow(dat.scaled), log(ext.0.75), 1:nrow(dat.scaled), col = "black"))
with(dat.scaled, points(log(median.ext+ 0.004), 1:nrow(dat.scaled), pch = 19, col = "grey30"))

par(xpd = NA)
mtext("Median extinction rate\n(fossil)", side = 1, line =2.5, cex = 0.7, col = "grey30")

par(las = 2)
axis(2, at = nrow(dat.scaled) + 1, label = "Taxon", lwd = 0, col.axis = "grey30")
par(las = 0)

## the iucn panel:
plot(1, 1, xlim = c(0, 0.5), ylim = c(1, nrow(dat.scaled)), type = "n", axes = F, xlab = "", ylab = "", xaxs = "i")
bg.plot(colour = "grey97")
par(xpd = FALSE)
abline(h = 1:nrow(dat.scaled), col = "grey90")
par(xpd = NA)
col.end <- paste("#FF0000", round(dat.scaled$n.prop, 2)* 100 - 1, sep = "")
col.vul <- as.character(paste("#D0BD00", round(dat.scaled$n.prop, 2)* 100 - 1, sep = ""))
box(bty = "o", col = "grey70")
par(xpd = FALSE)
par(xpd = NA)
axis(1, col = "grey70", at = seq(0, 0.5, 0.1), labels = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), col.axis = "grey35")


#bar.cols <- brewer.pal(3, "Set1")
par(xpd = FALSE)
with(dat.scaled, rect(rep(0, nrow(dat.scaled)), 1:nrow(dat.scaled)- 0.3, extinct, 1:nrow(dat.scaled) + 0.3, border = FALSE, col = "#00000090"))

# duplicated for darkness: endangered
with(dat.scaled, rect(extinct, 1:nrow(dat.scaled)- 0.3, extinct + endangered, 1:nrow(dat.scaled) + 0.3, border = FALSE, col = col.end))
with(dat.scaled, rect(extinct, 1:nrow(dat.scaled)- 0.3, extinct + endangered, 1:nrow(dat.scaled) + 0.3, border = FALSE, col = col.end))

# duplicated for darkness: vulnerable
with(dat.scaled, rect(endangered + extinct, 1:nrow(dat.scaled)- 0.3, extinct + endangered + vulnerable, 1:nrow(dat.scaled) + 0.3, border = FALSE, col =col.vul))
with(dat.scaled, rect(endangered + extinct, 1:nrow(dat.scaled)- 0.3, extinct + endangered + vulnerable, 1:nrow(dat.scaled) + 0.3, border = FALSE, col =col.vul))


par(xpd = NA)
par(las = 0)
mtext("Fraction of species\n(modern)", side = 1, line =2.5, cex = 0.7, col = "grey30")

## iucn numbers on the right
par(las = 1)
axis(4, col = "grey70", at = 1:nrow(dat.scaled), labels = paste(dat.scaled$n, dat.scaled$dd.high, sep = ""), col.axis = "grey45")
axis(4, at = nrow(dat.scaled) + 1.4, label = "Species\nassessed", lwd = 0, col.axis = "grey30")

## colour legend:
cols.df <- data.frame(value = seq(25, 100, 5), col = c(paste("#FF0000", seq(25, 95, 5), sep = ""), "#FF000099"));cols.df$col <- as.character(cols.df$col)
cols.df.vul <- data.frame(value = seq(25, 100, 5), col = c(paste("#D0BD00", seq(25, 95, 5), sep = ""), "#D0BD0099"));cols.df.vul$col <- as.character(cols.df.vul$col)
leg.pos <- 0.25
rect(leg.pos - 0.018, 0.6, leg.pos + 0.24, 5.5,  border= "grey75", col = "grey95")

cols.df$rect.left <- seq(1, 4, length.out = nrow(cols.df)+ 1)[1:(nrow(cols.df) )] 
cols.df$rect.right <- seq(1, 4, length.out = nrow(cols.df) + 1)[2:(nrow(cols.df) + 1)] 

cols.df.vul$rect.left <- seq(1, 4, length.out = nrow(cols.df.vul)+ 1)[1:(nrow(cols.df.vul) )]
cols.df.vul$rect.right <- seq(1, 4, length.out = nrow(cols.df.vul) + 1)[2:(nrow(cols.df.vul) + 1)] 

# duplicated to match darkness
for(i in 1:nrow(cols.df)) with(cols.df, rect(leg.pos, rect.left[i], leg.pos + 0.02, rect.right[i], col = col[i], border = NA))
for(i in 1:nrow(cols.df)) with(cols.df.vul, rect(leg.pos + 0.02, rect.left[i], leg.pos + 0.02 + 0.02, rect.right[i], col = col[i], border = NA))

for(i in 1:nrow(cols.df)) with(cols.df, rect(leg.pos, rect.left[i], leg.pos + 0.02, rect.right[i], col = col[i], border = NA))
for(i in 1:nrow(cols.df)) with(cols.df.vul, rect(leg.pos + 0.02, rect.left[i], leg.pos + 0.02 + 0.02, rect.right[i], col = col[i], border = NA))

segments(leg.pos + 0.06, 1, leg.pos + 0.06, 4, col = "grey35")
segments(leg.pos + 0.06, 4, leg.pos + 0.07, 4, col = "grey35")
segments(leg.pos + 0.06, 1, leg.pos + 0.07, 1, col = "grey35")
text(leg.pos + 0.07, 1, "25%", pos = 4, col = "grey45")
text(leg.pos + 0.07, 4, "100%", pos = 4, col = "grey45")
text(leg.pos - 0.03, 4.9, "% assessed", pos = 4, col = "grey45")

dev.off()
