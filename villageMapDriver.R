#villageMapDriver.R
#160823
#Kiki Chang(ychang64@jhu.edu)
#This driver creates a chorepleth map of Taiwan at village level using functions and variables in villageMap.R villageMapVars.R.
#----------------------------------------------------------------------------#
dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
source('map.R')
load("villageMapVars.RData", .GlobalEnv)

#select county and district
ctyNum <- ctyMenu()
dst <- dstMenu(ctyNum)

#subset myMap data for the selected district
myMap <- subset(myMap, C_Name == ctyNames[ctyNum] & T_Name == dst)

#choose data from file
message("This program creates a chorepleth map of ", ctyNames[ctyNum], dst,".", " Please choose data from file(csv/xlsx) for graphing: ")
dataFile <- file.choose() 
chooseFile(dataFile)

#store data
storeVillageData() 

#set graph color(s), starting value, number of intervals, and width
cNum <- chooseColor()
origin <- setStartVal()
nI <- setNumInterval()
w <- setIntervalWidth()

#set range and label (setLabel default argument: numDigit = 4, isPercent = TRUE)
ifelse((length(cNum) == 1), setSgColRange(nI, w, origin), setDbColRange(nI, w, origin))
setLabel(cNum, nI, w, origin)

#graph (default arguments: legendName = "得票率", isVillage = FALSE, textSize = 3, ext = ".svg", showName = TRUE)
graphArea(cNum, nI, isVillage = TRUE)

message("end of file")