#townMapDriver.R
#160830
#Kiki Chang(ychang64@jhu.edu)
#This program creates a chorepleth map of all towns in the selected city/county
#using functions & vars from map.R and townMapVars.RData
#
#Note: town_test.xlsx are provided as sample files for creating township map.
#The map is provided by Ministry of Transportation (src: https://gist-map.motc.gov.tw/Complex/MapTopic )
#TODO: might need to manually run gpclibPermit()  on the console to start the program
#----------------------------------------------------------------------------#
dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
source('map.R')
load("townMapVars.RData", .GlobalEnv)

#select county
ctyNum <- ctyMenu()

#subset myMap data for the selected county & flag allTown
if (ctyNum != length(ctyNames)) {
  myMap <- subset(myMap, countyname == ctyNames[ctyNum])
  allTown <<- FALSE
} else {
  allTown <<- TRUE
}

#choose data from file
message("This program creates a chorepleth map of ", ctyNames[ctyNum], ".", " Please choose data from file(csv/xlsx) for graphing: ")
dataFile <- file.choose() 
chooseFile(dataFile)

#store data
storeTownData() 

#set graph color(s), starting value, number of intervals, and width
cNum <- chooseColor()
origin <- setStartVal()
nI <- setNumInterval()
w <- setIntervalWidth()

#set range and label (setLabel() default argument: numDigit = 4, isPercent = TRUE)
ifelse((length(cNum) == 1), setSgColRange(nI, w, origin), setDbColRange(nI, w, origin))
setLabel(cNum, nI, w, origin)

#graph (default arguments: legendName = "得票率", isVillage = FALSE, textSize = 3, ext = ".svg", showName = TRUE, isTown = FALSE) 
if (ctyNum != length(ctyNames)) {
  graphArea(cNum, nI, isTown = TRUE)
} else {
  graphArea(cNum, nI, showName = FALSE, isTown = TRUE)
}
message("end of file")