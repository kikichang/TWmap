#villageMapVars.R
#160816
#Kiki Chang(ychang64@jhu.edu)
#This file contains data and variables used in graphing village map.
#The map is provided by Ministry of Transportation (src: http://data.gov.tw/node/7438 )
#NOTE: workspace must be cleared [ -> rm(list=ls()) on the console]  before running this program.
#----------------------------------------------------------------------------#
dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
library(maptools)
library(rgeos)

myMap = readShapePoly("Village_NLSC_1050219")
myMap$V_Name <- iconv(myMap$V_Name, "big5", "utf8")  #村
myMap$T_Name <- iconv(myMap$T_Name, "big5", "utf8")  #區
myMap$C_Name <- iconv(myMap$C_Name, "big5", "utf8")  #縣市
myMap$Substitute <- iconv(myMap$Substitute, "big5", "utf8")

allNames <- data.frame(myMap$T_Name, myMap$C_Name, stringsAsFactors = FALSE)
allDstQ <- c()

#all counties
ctyNames <- unique(myMap$C_Name)
ctyNamesLen <- c(1: length(ctyNames))
df <- data.frame(ctyNames, ctyNamesLen, stringsAsFactors = FALSE)
df$combined <- paste(df$ctyNamesLen, df$ctyNames)
ctyQ <- paste (df$combined, collapse = " ")

#all districts 
for(i in 1:length(ctyNames)) {
  dst <- subset(allNames, (allNames$myMap.C_Name == df$ctyNames[i]))
  dstNames <- unique(dst$myMap.T_Name)
  dstNamesLen <- c(1:length(dstNames))
  dstDf <- data.frame(dstNamesLen, dstNames, stringsAsFactors = FALSE)
  dstDf$combined <- paste(dstDf$dstNamesLen, dstDf$dstNames)
  dstQ <- paste(dstDf$combined, collapse = " ")
  allDstQ[i] <- dstQ
}

save.image("villageMapVars.RData")
message("end of file")