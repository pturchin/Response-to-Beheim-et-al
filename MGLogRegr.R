####   Logistic regression with MG as response variable
#####  Construct RegrDat
AggrDat <- read.table('MG Corrected Data.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, MG_corr, SPC1))
colnames(TableDat) <- c("NGA", "PolID", "Time", "MG", "SC")
dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
source("fRegrDatMG.R")  
NGARegrDat <- RegrDat
rm( list = setdiff(ls(),c("NGARegrDat") ) )

#############################################
###  Logistic regression:
RD <- NGARegrDat[complete.cases(NGARegrDat),4:9]
Y <- RD[,1]
X <- RD[,2:length(RD[1,])]
print(summary(reslt <- glm(Y ~ ., data = X, family=binomial(link='logit'))))
