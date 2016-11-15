#ctyMapVars.R
#160810
#Kiki Chang(ychang64@jhu.edu)
#This file contains data and variables used in graphing TW county map.
#The map is provided by Ministry of Transportation (src: https://gist-map.motc.gov.tw/Complex/MapTopic )
#----------------------------------------------------------------------------#
dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
library(maptools)
library(rgeos)

myMap = readShapePoly("twn_county")
myMap$countyid <- levels(droplevels(myMap$countyid))
myMap$countyname <- iconv(myMap$countyname, "big5", "utf8") #change encodings

#get county names
myArea <- as.data.frame(coordinates(myMap))
ind <- (grep("(海)", myMap$countyname))
myArea <- myArea[-ind,]
temp <- unique(myMap$countyname)
temp <- temp[!(grepl("(海)", temp))]
myArea$names <- temp
rm(temp)

save.image("countyMapVars.RData")
message("end of file")

