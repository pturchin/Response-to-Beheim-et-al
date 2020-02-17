#### fRegrDat constructs RegrDat for analysis of MG
#####     Assumes columns: NGA, PolID, Time; RespVar; Predictors
load("PolsVars.Rdata")

### Add lags
RegrDat <- TableDat
dat <- cbind(RegrDat,matrix(NA,nrow(RegrDat),2))
colnames(dat)[c((ncol(dat)-1),ncol(dat))] <- c("Lag1","Lag2")
for(i in 2:nrow(dat)){
  if(dat$Time[i] == dat$Time[i-1]+100){dat$Lag1[i] <- dat$MG[i-1]}
}
for(i in 3:nrow(dat)){
  if(dat$Time[i] == dat$Time[i-2]+200){dat$Lag2[i] <- dat$MG[i-2]}
}
RegrDat <- dat

#### Calculate Space using the estimated d = 1000 km (suggested by Peter Turchin as default because this is approximate average distance between NGAs)
Space <- RegrDat[,1:4]
Space[,4] <- NA
colnames(Space) <- c("NGA","PolID","Time","Space")

colMat <- colnames(DistMatrix)
rowMat <- rownames(DistMatrix)
for(i in 1:nrow(RegrDat)){
  t1 <- RegrDat$Time[i] - 100
  dat <- RegrDat[RegrDat$Time==t1,1:4] 
  if(nrow(dat) > 1){
    delta <- vector(length=nrow(dat))
    for(j in 1:nrow(dat)){
      dt <- DistMatrix[colMat==dat$NGA[j],]
      delta[j] <- dt[rowMat==RegrDat$NGA[i]]
    }
    s <- exp(-delta/dpar)*dat$MG
    s <- s[delta != 0]  ### Exclude i=j
    Space$Space[i] <- mean(s)
  }
}
Space$Space[is.na(Space$Space)] <- mean(Space$Space, na.rm = TRUE) # Substitute NAs with the mean
Space$Space <- Space$Space/max(Space$Space)   ##### Scale to max = 1
RegrDat <- cbind(RegrDat,Space$Space)
colnames(RegrDat)[ncol(RegrDat)] <- "Space"

#### Calculate Language = matrix of linguistic distances
Phylogeny <- RegrDat[,1:4]
Phylogeny[,4] <- NA
colnames(Phylogeny) <- c("NGA","PolID","Time","Phylogeny")

for(i in 1:nrow(RegrDat)){
  t1 <- RegrDat$Time[i] - 100
  dat <- RegrDat[RegrDat$Time==t1,1:4]
  dat <- dat[dat$NGA != RegrDat$NGA[i],]   ### Exclude i = j
  PolID <- RegrDat$PolID[i]
  PolLang <- polities[polities$PolID==PolID,9:11]
  if(nrow(dat) > 1){
    weight <- vector(length=nrow(dat)) * 0
    for(j in 1:nrow(dat)){
      dt <- dat[j,]
      PolLang2 <- polities[polities$PolID==dt$PolID,9:11]
      if(PolLang[1,3]==PolLang2[1,3]){weight[j] <- 0.25}
      if(PolLang[1,2]==PolLang2[1,2]){weight[j] <- 0.5}
      if(PolLang[1,1]==PolLang2[1,1]){weight[j] <- 1}
    }
    s <- weight*dat$MG
    Phylogeny$Phylogeny[i] <- mean(s)
  }
}
Phylogeny$Phylogeny[is.na(Phylogeny$Phylogeny)] <- 0 # Substitute NAs with 0
RegrDat <- cbind(RegrDat,Phylogeny$Phylogeny)
colnames(RegrDat)[ncol(RegrDat)] <- "Phylogeny"


