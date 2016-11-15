#townMapVars.R
#160810
#Kiki Chang(ychang64@jhu.edu)
#This file contains data and variables used in graphing TW town map.
#The map is provided by Ministry of Transportation (src: https://gist-map.motc.gov.tw/Complex/MapTopic )
#----------------------------------------------------------------------------#
dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
library(maptools)
library(rgeos)

myMap = readShapePoly("twn_town")
myMap$countyid <- levels(droplevels(myMap$townid)) #drop levels
myMap$countyname <- iconv(myMap$countyname, "big5", "utf8") #change encodings
myMap$townname <- iconv(myMap$townname, "big5", "utf8") 

#for displaying ctyMenu()
ctyNames <- unique(myMap$countyname)
ind <- (grep("(海)", ctyNames))
ctyNames <- ctyNames[-ind]
ctyNames[length(ctyNames)+1] <- "全台"   #不含外島
ctyNamesLen <- c(1: length(ctyNames))
df <- data.frame(ctyNames, ctyNamesLen, stringsAsFactors = FALSE)
df$combined <- paste(df$ctyNamesLen, df$ctyNames)
ctyQ <- paste (df$combined, collapse = " ")

save.image("townMapVars.RData")
message("end of file")