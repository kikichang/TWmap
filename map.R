#map.R
#160825
#Kiki Chang (ychang64@jhu.edu)
#This file contains functions for creating village,township, and county map.
#global vars: myDat, myMap, myLab, myVal, myColRange
#local vars (part): filePath, colorNum, startVal, numInterval, width
#----------------------------------------------------------------------------#

#setwd("/Users/Kiki/Documents/縣市鄉鎮市區/") #TODO: change the file path to this file's current location
#import packages (If package is not installed, type install.packages("package name"))
library(ggplot2)
library(gpclib)
library(tools)
library(xlsx)
library(xlsxjars)
library(rJava)
library(xlsxjars)
library(showtext)
library(RColorBrewer)

#----------set graph color(s), startingValue, numInterval, and intervalWidth--------------#

#[helper method] for user input choice [integer] (chooseColor(),setNumInterval());return TRUE if valid; FALSE otherwise.
#max, min are inclusive (valid choice)
userChoice <- function(str, min, max) {
  if(is.na(as.numeric(str))) {
    message(paste(str, "is not a numerical value."))
    return(FALSE)
  } else if (as.numeric(str)%%1 == 0) {
    if(as.numeric(str) > max || as.numeric(str) < min) {
      message(paste(str, " is not in between ",min, " and ", max, ".", sep = "" ))
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    message(paste(str, "is not an integer."))
    return(FALSE)
  }
}
#ask user for starting value; return starting value[numeric].
setStartVal <- function() {
  start <- readline(prompt = "What is the start value for this graph? Enter a number or type quit() to exit (default = 0): ")
  if(is.na(as.numeric(start))) {
    message(start, " is not a numerical value.")
    return(setStartVal())
  } else {
    return(as.numeric(start))
  }
}
#ask user for number of intervals(1-8); return number of intervals[numeric].
setNumInterval <- function() {
  numIvl <- readline(prompt = "How many intervals are in this graph? Enter an integer between 1-7(8 for two colors) or type quit() to exit: ")
  if(userChoice(numIvl,1,8)) {
    return(as.numeric(numIvl))
  } else {
    return(setNumInterval())
  }
}
#ask user for interval width; return interval width[numeric].
setIntervalWidth <- function() {
  ivlWidth <- readline(prompt = "What is the interval width for this graph? Enter a positive number or type quit() to exit: ")
  if(is.na(as.numeric(ivlWidth))) {
    message(ivlWidth, " is not a numerical value.")
    return(setIntervalWidth())
  } else if (as.numeric(ivlWidth) < 0) {
    message(ivlWidth, " is not a positive number.")
    return(setIntervalWidth())
  } else {
    return(as.numeric(ivlWidth))
  }
}
#ask user for graph color;return the corresponding number(s)[numeric] of the color(s) for the graph.
chooseColor <- function() {
  message("choose 1 or 2 colors for the graph: (1)紅 (2)橘 (3)綠 (4)藍 (5)紫 (6)灰\n (if choose 2 colors, numbers should be seperated by space)")
  color <- readline(prompt = "enter the number or type quit() to exit: ")
  color <- strsplit(color, split = " ")[[1]]
  if (length(color) == 1) {
    if(userChoice(color, 1,7)) {
      return(as.numeric(color))
    }
  } else if (length(color) == 2) {
    if(userChoice(color[1],1,7) && userChoice(color[2],1,7)) {
      return(as.numeric(color))
    }
  } else {
    return(chooseColor())
  }
}

#---------------------------set data range and graph label--------------------------------#

#sort data for single hue using defined starting value, width, and number of intervals; return myColRange.
#no range is assigned if data == starting value.
setSgColRange <- function(numIvl, width, startVal=0) {
  colRange <- c(NA)
  for(i in 1:length(myVal)) {
    if(!is.na(myVal[i])) { #if village myVal[i]'s data is not empty
      for(j in (0:(numIvl-1))) { #every range besides the last one, which is inclusive
        if(myVal[i] < width*j+ startVal) {
          colRange[i] <- (j+1)
          break
        }
      }
      if(myVal[i] > (width*(numIvl-1)+ startVal) && myVal[i] <= (width*numIvl + startVal)) {  # smaller or equal to max AND greater than the previous range 
        colRange[i] <- (numIvl+1)
      } else if (myVal[i] > (width*numIvl + startVal)) { #greater than max
        colRange[i] <- (numIvl+2)
      }
    }
  }
  assign("myColRange", colRange, envir = .GlobalEnv)
}
#sort data for two colors using defined starting value, width, and number of intervals; return myColRange.
#number of interval is defined for each color; no range is assigned if data == starting value.
setDbColRange <- function(numIvl, width, startVal=0) {
  colRange <- c(NA)
  numColRange <- ((numIvl+1)*2)
  for(i in 1:length(myVal)) {
    f <- 0
    k <- 0
    if(!is.na(myVal[i])) { #not empty
      if (myVal[i] > startVal) {  #  > startVal
        if (myVal[i] > (width*numIvl + startVal)) { #greater than max
          colRange[i] <- 1
        } else if (myVal[i] > (width*(numIvl-1)+ startVal) && myVal[i] <= (width*numIvl + startVal)) {  #smaller or equal to max AND greater than the previous range 
          colRange[i] <- 2
        } else if (numIvl > 1) {  #for loops
          for(j in (1:(numIvl-1))) { #every range besides the last two edge cases
            if(myVal[i] < (width*j+ startVal)) {
              colRange[i] <- ((numIvl+1) - f)
              break
            }
            f <- (f+1)
          }
        }
      } else if (myVal[i] < startVal) {  # < startVal
        if (myVal[i] < ((-width)*(numIvl) + startVal)) { #smaller than min
          colRange[i] <- numColRange
        } else if (myVal[i] < ((-width)*(numIvl-1)+ startVal) && myVal[i] >= ((-width)*numIvl + startVal)) {
          colRange[i] <- (numColRange-1)
        } else if (numIvl > 1) {
          for(j in ((numIvl-1):1)) { #every range besides the last one, which is inclusive
            if(myVal[i] > ((-width)*j+ startVal)) {
              colRange[i] <- (numIvl*2 - k)
              break
            }
            k <- (k+1) 
          }
        }
      }
    }
  }
  assign("myColRange", colRange, envir = .GlobalEnv)
}
#create the label for legends; @param numDigit max. number of digits (default = 4); 
#@param isPercent(bool) data is in percent form (default = TRUE); return myLab.
setLabel <- function(colorNum, numIvl,width, startVal = 0, isPercent = TRUE, numDigit = 4) {
  #lab <- NA
  if (length(colorNum) == 1) { #single color
    sequence = seq(from = startVal, to = numIvl*width + startVal, by = width)
    lab2 <- as.character(signif(sequence, digits = numDigit))
    lab1 <- as.character(signif((sequence-width), digits = numDigit))
    if (isPercent) {
      lab <- paste(lab1, "% ~ ",lab2, "%", sep = "")
      lab[1] <- paste("< ", lab2[1], "%", sep = "")
      lab[length(lab)+1] <- paste("> ", lab2[length(lab2)], "%", sep = "")
    } else {
      lab <- paste(lab1, " ~ ",lab2, sep = "")
      lab[1] <- paste("< ", lab2[1], sep = "")
      lab[length(lab)+1] <- paste("> ", lab2[length(lab2)], sep = "")
    }
  } else { #two colors 
    sequence = seq(from = startVal, to = numIvl*width + startVal, by = width)
    lab2 = sequence[2:length(sequence)]
    lab2[length(lab2)+1] = lab2[length(lab2)]
    lab2s = c(rev(lab2), startVal, -lab2)
    lab2s = signif(lab2s, digits = numDigit)  #-lab2s is the same as rev(lab2s)
    
    lab1 = sequence[1:length(sequence)-1]
    lab1[length(lab1)+1] = lab1[length(lab1)]
    lab1s = c(rev(lab1), startVal, -lab1) 
    lab1s = signif(lab1s, digits = numDigit)
    if(isPercent) {
      lab <- paste(lab1s, "% ~ ", lab2s, "%", sep = "")
      lab[1] <- paste("> ", lab2s[1], "%", sep = "")
      lab[length(lab)] <- paste("< ", lab2s[length(lab2s)], "%", sep = "")
      lab[numIvl+2] <- paste(lab2s[numIvl+2], "%", sep = "")
    } else {
      lab <- paste(lab1s, " ~ ", lab2s, sep = "")
      lab[1] <- paste("> ", lab2s[1], sep = "")
      lab[length(lab)] <- paste("< ", lab2s[length(lab2s)], sep = "")
      lab[numIvl+2] <- paste(lab2s[numIvl+2], sep = "")
    }
  }
  assign("myLab", lab, envir = .GlobalEnv)
}

#--------------------------select data file and graph map----------------------------------#

#ask user for data; eliminate rows with NAs; create a list (myDat) containing graph title and data.
chooseFile <- function(filePath) {
  fp <- strsplit(basename(filePath), "[.]")[[1]]
  if (fp[2] == "csv") {
    dat <- read.csv(filePath, header = TRUE, stringsAsFactors=FALSE)
    dat <- dat[complete.cases(dat),]
    dat <- list(fp[1], dat) 
  } else if (fp[2] == "xlsx") {
    dat <- read.xlsx(filePath,1, header = TRUE, stringsAsFactors=FALSE)
    dat <- dat[complete.cases(dat),]
    dat <- list(fp[1], dat) #remove rows w/ NAs 
  } else {
    warning("please import a csv/xlsx file")
  }
  assign("myDat", dat, envir = .GlobalEnv)
}
#graph data to the corresponding area; area without data are graphed using grey40 (dark grey).
#@param showName(bool) determines if area names are shown on graph; @param legendName(string) legend shown on graph; @param ext(string) file extension.
#@param textSize(numeric) size of the area name; @param isVillage(bool) set TRUE if graphing village map (default = FALSE); @param isTown(bool) set TRUE 
#if graphing townMap(default = FAULSE)
#file extensions can be: eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
graphArea <- function(colorNum, numInterval,legendName = "得票率", isVillage = FALSE, textSize = 3, ext = ".svg", showName = TRUE, isTown = FALSE) {
  myFrame <- fortify(myMap)
  names(myFrame)[1:2] = c("x","y")
  
  ids <- as.numeric(unique(myFrame$id))
  myGraph <- data.frame(id = ids)  #hard code would be myGraph <- data.frame(id = c(0:22))
  myGraph$val <- myVal
  myGraph$range <-myColRange
  
#   #area names
#   area <- as.data.frame(coordinates(myMap))
#   area$names <- unique(myMap$countyname)
  
  showtext.auto(enable = TRUE)
  font.add("HuaWen","华文细黑.ttf")
  
  #graph
  img = ggplot(myGraph) +
    geom_map(aes(map_id = id, fill = factor(range)), color = "white", map = myFrame) + coord_map() + 
    labs(x= NULL, y = NULL, title = myDat[[1]]) + guides(fill = guide_legend(nrow = length(myLab))) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(), text = element_text(family = 'HuaWen'))
  
  #if(!exists("myArea")) {
  if(isVillage) {
    myArea <- as.data.frame(coordinates(myMap))
    myArea$names <- unique(myMap$V_Name)  
  }
  
  if(isTown) {
    myArea <- as.data.frame(coordinates(myMap))
    ind <- (grep("(海)", myMap$townname))
    if(length(ind) != 0) {
      myArea <- myArea[-ind,]
    }
    temp <- (myMap$townname)
    temp <- temp[!(grepl("(海)", temp))]
    myArea$names <- temp
    rm(temp)
   # myAreas$names <- unique(myMap$townname)
  }
  
  
  #display area names on graphg
  if (showName) {
    img = img + geom_text(size = textSize , aes(x=V1, y= V2, label = names), data = myArea)
  }
  
  #set colors
  if (length(colorNum) == 1) {
    pal <- switch(colorNum, "Reds", "Oranges", "Greens", "Blues", "Purples","Greys")
    img = img + scale_fill_brewer(palette = pal, drop = FALSE, name = legendName,  na.value = "grey40", limits = c(as.character(1:length(myLab))), labels = myLab)
  } else {
    pal1 <- switch(colorNum[1], "Reds", "Oranges", "Greens", "Blues", "Purples","Greys")
    pal2 <- switch(colorNum[2], "Reds", "Oranges", "Greens", "Blues", "Purples","Greys")
    twoPal <-c(rev(brewer.pal(numInterval+1, pal1)), "grey90", brewer.pal(numInterval+1, pal2))
    img = img + scale_fill_manual(values = twoPal, drop = FALSE, name = legendName, na.value = "grey40", limits = c(as.character(1:length(myLab))), labels = myLab)
  }
  
  #set width & length of the graph
  if (isVillage || (!allTown)) {
    maxX <- as.numeric(max(myFrame$x)) 
    minX <- as.numeric(min(myFrame$x))
    maxY <- as.numeric(max(myFrame$y))
    minY <- as.numeric(min(myFrame$y))
    img = img + expand_limits(x=c(minX,maxX), y=c(minY,maxY))
  } else {
    img = img + expand_limits(x=c(119.75,122.25), y=c(22,25.5))
  }
  
  #set file name and save image to file
  imgName <-paste(myDat[[1]], ext, sep= "")
  ggsave(filename = imgName, plot=img)
}

#------------------store village, township, county data from file-------------------------#
#store data to the corresponding village; invalid area color would be dark grey (grey40); return myVal.
storeVillageData <- function() {
  vals <- c()
  for (i in 1:length(myMap$VILLAGE_ID)) {
    n <- grep(myMap$V_Name[i], myDat[[2]][,1])
    if (length(n) != 0) {
      if (length(n) == 1) {
        vals[i] <- myDat[[2]][n,2]
      } else if (length(n) > 1) {  #multiple values found for one village in the data
        message("multiple values for ", myMap$V_Name[i], " (stored the first found)")
        vals[i] <- myDat[[2]][n[1],2]
      }
    } else if ((grepl("[1-9]", myMap$V_Name[i]))) { #if V_Name contains numerical values (e.g. 黃官嶼2)
      vlg <- strsplit(myMap$V_name[i], "[1-9]")[[1]]
      n <- grep(vlg, myDat[[2]][,1])
      if (length(n) == 1) {
        vals[i] <- myDat[[2]][n,2]
      } else if (length(n) > 1) {  #multiple values found for one village in the data
        message("multiple values for ", myMap$V_Name[i], " (stored the first found)")
        vals[i] <- myDat[[2]][n[1],2]
      }
    } else { #look for alternative village name
      n <- grep(myMap$Substitute[i], myDat[[2]][,1])
      if (length(n) == 1) {
        vals[i] <- myDat[[2]][n,2]
      } else if (length(n) > 1) {  #multiple values found for one village in the data
        message("multiple values for ", myMap$V_Name[i], " (stored the first found)")
        vals[i] <- myDat[[2]][n[1],2]
      } else {
        message("data not found for ", myMap$V_Name[i])
      }
    }
  }
  assign("myVal", vals, envir = .GlobalEnv)
}
#store data to the corresponding town; invalid area color would be dark grey (grey40); return myVal.
storeTownData <- function() {
  vals <- c()
  sub("台", "臺", myDat[[2]][,1])
  for(i in 1:length(myMap$townid)) {
    n <- intersect(grep(myMap$townname[i], myDat[[2]][,2]), grep(myMap$countyname[i], myDat[[2]][,1]))
    if (length(n) == 1) {
      vals[i] <- myDat[[2]][n,3]
    } else if(length(n) > 1) {
      message("multiple values for ", myMap$countyname[i], myMap$townname[i], "(stored the first found)")
      vals[i] <- myDat[[2]][n[1],3]
    } else if (grepl("(海)", myMap$townname[i])) {  #TODO: need to check for if myMap$countyname[i] exists as well? (prob not)
      ctyName <- strsplit(myMap$countyname[i], '[(海)]')[[1]]
      twnName <- strsplit(myMap$townname[i], '[(海)]')[[1]]
      n <- intersect(grep(twnName[1], myDat[[2]][,2]), grep(ctyName[1], myDat[[2]][,1]))
      if(length(n) == 1) {
        vals[i] <- myDat[[2]][n,3]
      } else if (length(n) > 1) {
        message("multiple values for ", ctyName[1], twnName[1], "(stored the first found)", sep = "")
        vals[i] <- myDat[[2]][n[1],3]
      } else {
        message("data not found for ", ctyName[1], twnName[1])
      }
    } else {
      message("data not found for ", myMap$countyname[i], myMap$townname[i])
    }
  }
  assign("myVal", vals, envir = .GlobalEnv)
}
#store data to the corresponding county; invalid area color would be dark grey (grey40); return myVal.
storeCountyData <- function() {
  vals <- c()
  sub("臺", "台", myDat[[2]][,1])
  for (i in 1:length(myMap$countyid)) {
    n <- grep(myMap$countyname[i], myDat[[2]][,1])
    if (length(n) == 1) {
      vals[i] <- myDat[[2]][n,2]
    } else if (length(n) > 1) {
      message("multiple values for ", myMap$countyname[i], " (stored the first found)")
      vals[i] <- myDat[[2]][n[1],2]
    } else if (grepl("(海)", myMap$countyname[i])) { #基隆市(海),台中市(海),台南市(海), 高雄市(海)
      vals[i] <- vals[i-1]
    } else {
      message("data not found for ", myMap$countyname[i])
    }
  }
  assign("myVal",vals, envir = .GlobalEnv)
}

#------------------menu for villageMapDriver-------------------------#
#ask user for county; return county number(numeric)
ctyMenu <- function() {
  message(paste("Which city/county do you want to graph? \n", ctyQ, sep=""))
  cityNumber <- readline(prompt = "enter the number or type quit() to exit: ")
  if(userChoice(cityNumber,1,length(ctyNamesLen))) {
    return(as.numeric(cityNumber))
  } else {
    return(ctyMenu())
  }
}
#ask user for district; param: county number; return district name(string)
dstMenu <- function(ctyNum) {
  message(paste("Which district do you want to graph? \n", allDstQ[ctyNum], sep=""))
  dstNum <- readline(prompt = "enter the number type quit() to exit: ")
  max <- subset(allNames, allNames$myMap.C_Name == ctyNames[ctyNum])
  if(userChoice(dstNum,1,length(unique(max$myMap.T_Name)))) {
    dstName <- (strsplit(allDstQ[ctyNum], " ")[[1]])[as.numeric(dstNum)*2]
    return(dstName) 
  } else {
    return(dstMenu(ctyNum))
  }
}