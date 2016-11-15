#townMapGenerator.R
#160830
#Kiki Chang(ychang64@jhu.edu)
#This program generates a chorepleth map of the selected city/county
#using functions & vars from map.R and townMapVars.RData.
#
#Note: town_test.xlsx are provided as sample files for creating township map.
#The map is provided by Ministry of Transportation (src: https://gist-map.motc.gov.tw/Complex/MapTopic )
#TODO: might need to manually run gpclibPermit()  on the console to start the program
#e.g.
#townMapGenerator(filePath, "台北市", "3 4", 6, 30, 60, FALSE, "指數")
#townMapGenerator(filePath, "全台", "3 4", 6, 180, 150, FALSE,"指數")
#----------------------------------------------------------------------------#

dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
source('map.R')
load("townMapVars.RData", .GlobalEnv)

#filePath is the full path name of the data file
#color (integer 1-6 in string form) is a string of length 1 or 2  (e.g. "5", "3 6") 
#number of intervals (nI) is an integer between 1-8; width(w) is a positive number (>0)
#color numbers can be selected from (1)red (2)orange (3)green (4)blue (5)purple (6)grey
#default settings:  starting value (origin) = 0, county names are shown, data are labeled w/ percent sign(%), legend is "得票率",
# text size is 3, file extension is .svg
townMapGenerator <- function(filePath, cityName, color,nI, w, origin = 0, percent = TRUE, legend = "得票率", text = 3, extension = ".svg") {
  
  #check if all required arguments are given
  args <- c("filePath", "cityName", "color", "nI", "w")
  for (i in 1:length(args)) {
    x=as.name(args[i])
    print(ifelse(missing(x), stop(paste(args[i], "not specified.")), paste(args[i], "is specified.") ))
  }
  
  #get color number(s)
  cNum <- as.numeric(strsplit(color, split = " ")[[1]])
  
  #subset myMap data for the selected county & flag allTown
  if (cityName != "全台") {
    cityName <- sub("台", "臺", cityName)
    myMap <<- subset(myMap, countyname == cityName) #update myMap (global var)
    allTown <<- FALSE
  } else {
    allTown <<- TRUE
  }

  #choose data from file
  chooseFile(filePath)
  
  #store data
  storeTownData()
  
  #set range and label (setLabel default argument: numDigit = 4, isPercent = TRUE)
  ifelse((length(cNum) == 1), setSgColRange(nI, w, origin), setDbColRange(nI, w, origin))
  setLabel(cNum, nI, w, origin, isPercent = percent)
  
  #graph (default arguments: legendName = "得票率", isVillage = FALSE, textSize = 3, ext = ".svg", showName = TRUE, isTown = FALSE)
  if (cityName != "全台") {
    graphArea(cNum, nI, isTown = TRUE, legendName = legend) # all towns in the selected city/county
  } else {
    graphArea(cNum, nI, showName = FALSE, isTown = TRUE, legendName = legend)  #all ~300 towns in Taiwan
  }
  
}