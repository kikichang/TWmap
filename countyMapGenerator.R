#countyMapGenerator.R
#160830
#Kiki Chang(ychang64@jhu.edu)
#This function creates a chorepleth map of Taiwan at county level.
#using functions and variables in map.R countyMapVars.R.
#
#Note: county_test.xlsx are provided as sample files for creating township map and county map, respectively.
#The map is provided by Ministry of Transportation (src: https://gist-map.motc.gov.tw/Complex/MapTopic )
#TODO: might need to manually run gpclibPermit()  on the console to start the program
#----------------------------------------------------------------------------#

dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
source('map.R')
load("countyMapVars.RData", .GlobalEnv)

#filePath is the full path name of the data file
#color (integer 1-6 in string form) is a string of length 1 or 2  (e.g. "5", "3 6")
#color numbers can be selected from (1)red (2)orange (3)green (4)blue (5)purple (6)grey
#number of intervals (nI) is an integer between 1-8; width(w) is a positive number (>0) 
#default settings:  starting value (origin) = 0, county names are shown, data are labeled w/ percent sign(%), legend is "得票率",
# text size is 3, file extension is .svg
countyMapGenerator <- function(filePath, color,nI, w, origin = 0, percent = TRUE, showCtyName = TRUE, legend = "得票率", 
                               text = 3, extension = ".svg") {
  args <- c("filePath", "color", "nI", "w")
  for (i in 1:length(args)) {
    x=as.name(args[i])
    print(ifelse(missing(x), stop(paste(args[i], "not specified.")), paste(args[i], "is specified.") ))
  }
  
  cNum <- as.numeric(strsplit(color, split = " ")[[1]])  #color number(s)
  
  #choose data from file
  chooseFile(filePath)
  
  #store data
  storeCountyData() 

  #set range and label (setLabel default argument: numDigit = 4, isPercent = TRUE)
  ifelse((length(cNum) == 1), setSgColRange(nI, w, origin), setDbColRange(nI, w, origin))
  setLabel(cNum, nI, w, origin, isPercent = percent)
  
  #graph (default arguments: legendName = "得票率", isVillage = FALSE, textSize = 3, ext = ".svg", showName = TRUE)
  graphArea(cNum, nI, showName = showCtyName, legendName = legend, textSize = text, ext = extension)

}