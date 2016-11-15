#countyMapDriver.R
#160825
#Kiki Chang(ychang64@jhu.edu)
#This program creates a chorepleth map of Taiwan counties (以2015.10行政區製圖)
#using functions & vars from map.R and countyMapVars.RData
#Data are loaded from a csv/xlxs file. The format is demonstrated in the example below.
#
#縣市級圖
#         col1      col2         
# row 1   縣市名    得票率     #must have column names (can be anything)
# row 2   臺北市    0.3
#   ...
# row n   臺南市    0.7
#
#Note: county_test.xlsx are provided as sample files for creating township map and county map, respectively.
#Note:For xlxs file, data must be stored in the first sheet (default).
#Note:桃園縣市、高雄縣市、台南縣市、台中縣市合併為桃園市、高雄市、台南市、台中市; 台北縣更名為新北市
#The map is provided by Ministry of Transportation (src: https://gist-map.motc.gov.tw/Complex/MapTopic )

#TODO: might need to manually run gpclibPermit()  on the console to start the program
#----------------------------------------------------------------------------#
dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
source('map.R')
load("countyMapVars.RData", .GlobalEnv)

#choose data from file
message("This program creates a chorepleth map of Taiwan counties. Please choose data from file(csv/xlsx) for graphing: ")
dataFile <- file.choose() 
chooseFile(dataFile)

#store data
storeCountyData()

#set graph color(s), starting value, number of intervals, and width
cNum <- chooseColor()
origin <- setStartVal()
nI <- setNumInterval()
w <- setIntervalWidth()

#set range and label (setLabel() default arguments: numDigit = 4, isPercent = TRUE)
ifelse((length(cNum) == 1), setSgColRange(nI, w, origin), setDbColRange(nI, w, origin))
setLabel(cNum, nI, w, origin, FALSE)

#graph (optional arguments: legendName, isVillage, textSize, ext, showName)
graphArea(cNum, nI)

message("end of file")