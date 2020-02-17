### Figure 1a in Response to Beheim et al.
par(mfrow=c(1,2))
{  ###     The data step
  AggrDat <- read.table('MSP new data.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
  AggrDat <- AggrDat[AggrDat$Dupl == "n",]

  ### Construct SPC1 (scaled to [0, 1]) 
  reslt <- prcomp( subset(AggrDat, select = c(Pop, Terr, Cap, Hier, Gov, Infra, Info, Money)), scale=TRUE)
  AggrDat$SPC1 <- predict(reslt)[,1]
  AggrDat$SPC1 <- (AggrDat$SPC1 - min(AggrDat$SPC1))/(max(AggrDat$SPC1) - min(AggrDat$SPC1))

  ### Plot data in SPC1-MSP phase 
  gdat <- subset(AggrDat, select = c(SPC1,MSP))
  gdat <- gdat[complete.cases(gdat),]
  smoothScatter(gdat, nbin = 128, bandwidth = c(0.05,0.1), nrpoints = 0,
                colramp = colorRampPalette(c("blue","green","yellow","orange", "red")),
                xlab=colnames(gdat)[1], ylab=colnames(gdat)[2], main = "(a)")
  abline(v=seq(0,1, by=0.1), h=seq(0,1, by=0.1), col="grey")
  points(gdat, pch=16, col="brown")
  reslt <- loess(MSP ~ SPC1, data = gdat, span = 0.75)
  Smooth <- data.frame(SPC1 = seq(0, 1, by=0.01))
  Smooth$MSP <- predict(reslt, Smooth$SPC1)
  lines(Smooth, lwd=3, col="brown", lty=1)
}  # end

{  ###     Figure 1b
  dat <- read.table('MG Corrected Data.csv', sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  gdat <- subset(dat, select = c(SPC1,MG_corr))
  colnames(gdat)[2] <- "MG"
  gdat <- gdat[complete.cases(gdat),]
  smoothScatter(gdat, nbin = 128, bandwidth = c(0.05,0.15), nrpoints = 0,
                colramp = colorRampPalette(c("blue","green","yellow","orange", "red")),
                xlab=colnames(gdat)[1], ylab=colnames(gdat)[2], main = "(b)")
  abline(v=seq(0,1, by=0.1), h=seq(0,1, by=0.1), col="grey")
  points(gdat, pch=16, col="brown")
  reslt <- loess(MG ~ SPC1, data = gdat, span = 0.75)
  Smooth <- data.frame(SPC1 = seq(0, 1, by=0.01))
  Smooth$MHG <- predict(reslt, Smooth$SPC1)
  lines(Smooth, lwd=3, col="brown", lty=1)
}  # end
# hist(gdat$SPC1)
par(mfrow=c(1,1))
