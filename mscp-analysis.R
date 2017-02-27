########################################## GLOBAL PARAMETERS #########################################
# file system
BASE_DIR <- paste(dirname(sys.frame(1)$ofile), "/simulations/", sep = "")
BASE_FILENAME <- "sim-"
# log level
LOG_LEVEL <- "debug"    # possible: "all", debug", "none"
# VOD types
VOD_TYPES <- c("sym", "asym1", "asym2")


#----------------------------------------------------------------------------------------------------#
# function: importVodSimData
#     Imports VOD simulation data. 
#     Data is stored as: "BASE_DIR/modelType/Date-dateCount/sim-simCount.Rdata"
#     Available for modelType: "default", "reinf", "decl-mem", "mel-vs-max"
#     Date format: yyyymmdd
#     param:  modelType
#         the model type used by the simulation
#         possible: "default", "reinf", "decl-mem", "mel-vs-max"
#     param:  date
#         the date of the simulation 
#         (if undefined: latest available date)
#     param:  dateCount
#         defines the round of simulation of the given date
#         (if undefined: latest available round of simulations)
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: "sym", "asym1", "asym2"
#----------------------------------------------------------------------------------------------------#
importVodSimData <- function(modelType = "default",
                             date = "latest",
                             dateCount = "latest", 
                             vodType = "sym") {
  
  modelDir <- paste(BASE_DIR, modelType, sep = "")
  
  if (date == "latest") {
    dateDirs <- list.dirs(modelDir, recursive = FALSE)
    dates <- gsub(paste(modelDir, "/", sep = ""), "", dateDirs, fixed = TRUE)
    date <- max(dates)
  }
  dateDir <- paste(modelDir, "/", date, sep = "")
  
  if (dateCount == "latest") {
    dateCountDirs <- list.dirs(dateDir, recursive = FALSE)
    dateCounts <- gsub(paste(dateDir, "/", sep = ""), "", dateCountDirs, fixed = TRUE)
    dateCount <- max(dateCounts)
  }
  dateCountDir <- paste(dateDir, "/", dateCount, sep = "")
  
  vodTypeDir <- paste(dateCountDir, "/", vodType, sep = "")
  
  vodSimData = list()
  simCountFiles <- list.files(vodTypeDir, recursive = FALSE)
  for (i in 1:length(simCountFiles)) {
    filename <- paste(vodTypeDir, "/", BASE_FILENAME, i, ".Rdata", sep = "")
    vodSimData[[i]] <- get(load(filename))
  }
  return(vodSimData)
}

#----------------------------------------------------------------------------------------------------#
# function: extractLNISequence
#   Extracts the interaction sequence required to compute the LNI. For details, see Diekmann & 
#   Przepiorka (2016), p.1318.
#    "1" - player 1 coordinates, others deviate
#    "2" - player 2 coordinates, others deviate
#    "3" - player 3 coordinates, others deviate
#   "-1" - either more than one player coordinates, or all players deviate
#   param:  vodData
#       the VOD data to extract the LNI sequence from
#----------------------------------------------------------------------------------------------------#
extractLNISequence <- function(vodData) {
  moves <- vodData[vodData$round >= 1,2:4]
  
  moves$lniSequence <- -1
  moves[moves$player1 == "c" & moves$player2 == "d" & moves$player3 == "d", "lniSequence"] <- 1
  moves[moves$player1 == "d" & moves$player2 == "c" & moves$player3 == "d", "lniSequence"] <- 2
  moves[moves$player1 == "d" & moves$player2 == "d" & moves$player3 == "c", "lniSequence"] <- 3
  
  return(moves$lniSequence)
}


#----------------------------------------------------------------------------------------------------#
# function: computeLNIs
#   Computation of the Latent Norm Index (LNI) for the given VOD data.
#   TODOs: 
#       - generalize code for different sequence lengths (quite dodgy the way it is)
#   param:  lniSequence
#       the LNI sequence 
#----------------------------------------------------------------------------------------------------#
computeLNIs <- function(lniSequence) {
  
  # 1-sequences
  oneSequences <- c()
  i <- 1
  while (i < length(lniSequence)) {
    currInteraction <- lniSequence[i]
    
    if (currInteraction[1] == -1) {
      i <- i+1
      next
    }
    
    # compare current interaction with next interactions, 
    # as long as we haven't reached the end of the sequence
    # and as long as they are the same
    seqLength <- 1
    j <- i+1
    while (j <= length(lniSequence) 
           & currInteraction == lniSequence[j]) {
      seqLength <- seqLength+1
      j <- j+1
    }
    if (seqLength > 1) {
      oneSequences <- c(oneSequences, seqLength)
    }
    i <- j
  }
  oneSequences <- oneSequences[oneSequences >= 3]
  lni13 <- 100 * sum(oneSequences) / length(lniSequence)
  
  # 2-sequences
  twoSequences <- c()
  i <- 1
  while (i+1 <= length(lniSequence)) {
    j <- i+1
    k <- i+2
    currInteraction <- lniSequence[i:j]
    if (currInteraction[1] == -1
        | currInteraction[2] == -1
        | currInteraction[1] == currInteraction[2]) {
      i <- i+1
      j <- j+1
      k <- k+1
      next
    }
    seqLength <- 2
    alternatingIndex <- 0
    while (k <= length(lniSequence)
           & lniSequence[k] == currInteraction[(alternatingIndex%%2)+1]) {
      seqLength <- seqLength+1
      k <- k+1
      alternatingIndex <- alternatingIndex+1
    }
    twoSequences <- c(twoSequences, seqLength)
    i <- k
  }
  twoSequences <- twoSequences[twoSequences >= 3]
  lni23 <- 100 * sum(twoSequences) / length(lniSequence)
  
  # 3-sequences
  threeSequences <- c()
  i <- 1
  while (i+2 <= length(lniSequence)) {
    j <- i+1
    k <- i+2
    l <- i+3
    currInteraction <- lniSequence[i:k]
    if (currInteraction[1] == -1
        | currInteraction[2] == -1
        | currInteraction[3] == -1
        | currInteraction[1] == currInteraction[2]
        | currInteraction[1] == currInteraction[3]
        | currInteraction[2] == currInteraction[3]) {
      i <- i+1
      j <- j+1
      k <- k+1
      l <- l+1
      next
    }
    seqLength <- 3
    alternatingIndex <- 0
    while (l <= length(lniSequence)
           & lniSequence[l] == currInteraction[(alternatingIndex%%3)+1]) {
      seqLength <- seqLength+1
      l <- l+1
      alternatingIndex <- alternatingIndex+1
    }
    threeSequences <- c(threeSequences, seqLength)
    i <- k
  }
  threeSequences <- threeSequences[threeSequences >= 3]
  lni33 <- 100 * sum(threeSequences) / length(lniSequence)
  
  return(data.frame(lni13, lni23, lni33))
}


plotInteractionPattern <- function(vodData) {
  vodData <- importVodSimData()
  singleVodData <- vodData[[1]]
  singleVodData <- singleVodData[2:length(singleVodData$round),2:4]
  rownames(singleVodData) <- seq(length=nrow(singleVodData))
  
  p1Cooperations <- data.frame(which(singleVodData[,1] == 1), 1)
  p2Cooperations <- data.frame(which(singleVodData[,2] == 1), 2)
  p3Cooperations <- data.frame(which(singleVodData[,3] == 1), 3)

  
  quartz(width=11,height=6)
  par(mfrow=c(3,1))
    
  plot(p1Cooperations, ann=FALSE, xaxt = "n", yaxt = "n", 
       type = 'p', pch = 'x',
       xlim = range(0:nrow(singleVodData)), ylim = range(0.5:3.5))
  
  mtext(side=1,"Period",line=2)       ## x-axis label
  mtext(side=2,"Player",line=2)       ## y-axis label
  # mtext(side=3,"",line=2,cex=1.5)   ## title
  axis(side=1,at=seq(0,nrow(singleVodData),1),labels=seq(0,nrow(singleVodData),1))
  axis(side=2,at=seq(0,3,1),labels=seq(0,3,1))
  
  points(p2Cooperations, type = 'p', pch = 'x')
  points(p3Cooperations, type = 'p', pch = 'x')
  
  stripchart(p1Cooperates)
  plot(p1Cooperates, type = 'o', pch = 'x', ylab = '')
  
  
  
  which(firstData)
  
  
  
  x <- c(2,8,11,19)
  x <- data.frame(x,1) ## 1 is your "height"
  x
  plot(x, type = 'o', pch = '|', ylab = '')
  
  
  
  x <- stats::rnorm(50)
  xr <- round(x, 1)
  
  x
  stripchart(x)
  m <- mean(par("usr")[1:2])
  
}

#----------------------------------------------------------------------------------------------------#
# function: analyzeData
#     Starting point for the data analysis.
#     param:  modelType
#         the model type used by the simulation
#         possible: "default", "reinf", "decl-mem", "mel-vs-max"
#     param:  date
#         the date of the simulation 
#         (if undefined: latest available date)
#     param:  dateCount
#         defines the round of simulations of the given date
#         (if undefined: latest available round of simulations)
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: "all", "sym", "asym1", "asym2"
#----------------------------------------------------------------------------------------------------#
analyzeData <- function(modelType = "default",
                        date = "latest",
                        dateCount = "latest",
                        vodType = "all") {
  
  if (vodType == "all") {
    vodType <- VOD_TYPES
  }
  
  LNIs <- data.frame()
  for (i in 1:length(vodType)) {
    currVodType <- vodType[i]
    
    if (LOG_LEVEL == "all") {
      cat(paste("Analyzing data for:
                model type:\t", modelType, "
                date:\t\t", date, "
                date count:\t", dateCount, "
                vod type:\t", currVodType,"
                ########################\n"))
    }
    
    vodSimData <- importVodSimData(modelType = modelType, date = date, 
                                   dateCount = dateCount, vodType = currVodType)
    vodTypeLNIs <- data.frame()
    
    # row binding of all simulations per VOD type
    for (i in 1:length(vodSimData)) {
      lniSequence <- extractLNISequence(vodSimData[[i]])
      vodTypeLNIs <- rbind(vodTypeLNIs, computeLNIs(lniSequence))
    }
    colnames(vodTypeLNIs) <- c((paste(currVodType, "_h1", sep = "")),
                               (paste(currVodType, "_h2", sep = "")),
                               (paste(currVodType, "_h3", sep = "")))
    
    # column binding of different VOD types
    if (nrow(LNIs) == 0) {
      LNIs <- vodTypeLNIs
    } else {
      LNIs <- cbind(LNIs, vodTypeLNIs)
    }
  }
  
  if (LOG_LEVEL == "all") {
    print(LNIs)
  }
  
  meanLNIs <- colMeans(LNIs)
  
  if (LOG_LEVEL == "debug" || LOG_LEVEL == "all") {
    print(meanLNIs)
  }
}


############################################# TEST CASES #############################################
#----------------------------------------------------------------------------------------------------#
# function: createVodTestData1
#     Creates the first LNI example data, as in Diekmann & Przepiorka (2016), p.1318
#----------------------------------------------------------------------------------------------------#
createVodTestData1 <- function() {
  round <- c(0,1,2,3,4,5,6,7,8,9,10)
  player1 <- c(NA,"d","c","c","d","d","c","d","c","d","d")
  player2 <- c(NA,"c","d","d","d","c","d","d","d","c","d")
  player3 <- c(NA,"d","d","d","c","d","d","d","d","d","c")
  util1 <- c(0,100,60,60,100,100,60,0,60,100,100)
  util2 <- c(0,60,100,100,100,60,100,0,100,60,100)
  util3 <- c(0,100,100,100,60,100,100,0,100,100,60)
  return(data.frame(round,player1,player2,player3,util1,util2,util3))
}

#----------------------------------------------------------------------------------------------------#
# function: createVodTestData2
#     Creates the second LNI example data, as in Diekmann & Przepiorka (2016), p.1318
#----------------------------------------------------------------------------------------------------#
createVodTestData2 <- function() {
  round <- c(0,1,2,3,4,5,6,7,8,9,10)
  player1 <- c(NA,"c","d","c","d","c","d","d","d","c","d")
  player2 <- c(NA,"c","c","d","c","d","c","c","c","d","d")
  player3 <- c(NA,"d","d","d","d","d","d","d","d","d","c")
  util1 <- c(0,60,100,60,100,60,100,100,100,60,100)
  util2 <- c(0,60,60,100,60,100,60,60,60,100,100)
  util3 <- c(0,100,100,100,100,100,100,100,100,100,60)
  return(data.frame(round,player1,player2,player3,util1,util2,util3))
}

#----------------------------------------------------------------------------------------------------#
# function: createLNITestSequence1
#     Creates an LNI test sequence
#----------------------------------------------------------------------------------------------------#
createLNITestSequence1 <- function() {
  return(c(1,1,2,2,2,3,3,3,3,1,2,3,2,2,2,2,2,2,2))
}

#----------------------------------------------------------------------------------------------------#
# function: createVodTestData3
#     Creates the second LNI example data, as in Diekmann & Przepiorka (2016), p.1318
#----------------------------------------------------------------------------------------------------#
createVodTestData2 <- function() {
  round <- c(0,1,2,3,4,5,6,7,8,9,10)
  player1 <- c(NA,"c","d","c","d","c","d","d","d","c","d")
  player2 <- c(NA,"c","c","d","c","d","c","c","c","d","d")
  player3 <- c(NA,"d","d","d","d","d","d","d","d","d","c")
  util1 <- c(0,60,100,60,100,60,100,100,100,60,100)
  util2 <- c(0,60,60,100,60,100,60,60,60,100,100)
  util3 <- c(0,100,100,100,100,100,100,100,100,100,60)
  return(data.frame(round,player1,player2,player3,util1,util2,util3))
}

#----------------------------------------------------------------------------------------------------#
# function: createLNITestSequence2
#     Creates an LNI test sequence
#----------------------------------------------------------------------------------------------------#
createLNITestSequence2 <- function() {
  return(c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1))
}

#----------------------------------------------------------------------------------------------------#
# function: testAnalysis
#     Starting point for test analysis.
#----------------------------------------------------------------------------------------------------#
testAnalysis <- function() {
  
  # test case 1
  vodTestData <- createVodTestData1()
  lniSequence <- extractLNISequence(vodTestData)
  lnis <- computeLNIs(lniSequence)
  if (lnis$lni13 != 0) stop(paste("Error during computation of test case 1:\n",
                                  "\texpected value for lni13: 0\n",
                                  "\tcomputed value for lni13:", lnis$lni13))
  if (lnis$lni23 != 0) stop(paste("Error during computation of test case 1:\n",
                                  "\texpected value for lni23: 0\n",
                                  "\tcomputed value for lni23:", lnis$lni23))
  if (lnis$lni33 != 70) stop(paste("Error during computation of test case 1:\n",
                                   "\texpected value for lni33: 70\n",
                                   "\tcomputed value for lni33:", lnis$lni33))
  
  # test case 2
  vodTestData <- createVodTestData2()
  lniSequence <- extractLNISequence(vodTestData)
  lnis <- computeLNIs(lniSequence)
  if (lnis$lni13 != 30) stop(paste("Error during computation of test case 2:\n",
                                   "\texpected value for lni13: 30\n",
                                   "\tcomputed value for lni13:", lnis$lni13))
  if (lnis$lni23 != 50) stop(paste("Error during computation of test case 2:\n",
                                   "\texpected value for lni23: 50\n",
                                   "\tcomputed value for lni23:", lnis$lni23))
  if (lnis$lni33 != 30) stop(paste("Error during computation of test case 2:\n",
                                   "\texpected value for lni33: 30\n",
                                   "\tcomputed value for lni33:", lnis$lni33))
  
  # test case 3
  lnis <- computeLNIs(createLNITestSequence1())
  if (round(lnis$lni13, 3) != 73.684) stop(paste("Error during computation of test case 2:\n",
                                                 "\texpected value for lni13: 73.684\n",
                                                 "\tcomputed value for lni13:", round(lnis$lni13, 3)))
  if (round(lnis$lni23, 3) != 15.789) stop(paste("Error during computation of test case 2:\n",
                                                 "\texpected value for lni23: 15.789\n",
                                                 "\tcomputed value for lni23:", round(lnis$lni23, 3)))
  if (round(lnis$lni33, 3) != 21.053) stop(paste("Error during computation of test case 2:\n",
                                                 "\texpected value for lni33: 21.053\n",
                                                 "\tcomputed value for lni33:", round(lnis$lni33, 3)))
  
  # test case 4  
  lnis <- computeLNIs(createLNITestSequence2())
  if (lnis$lni13 != 0) stop(paste("Error during computation of test case 2:\n",
                                  "\texpected value for lni13: 0\n",
                                  "\tcomputed value for lni13:", round(lnis$lni13, 3)))
  if (lnis$lni23 != 0) stop(paste("Error during computation of test case 2:\n",
                                  "\texpected value for lni23: 0\n",
                                  "\tcomputed value for lni23:", round(lnis$lni23, 3)))
  if (lnis$lni33 != 0) stop(paste("Error during computation of test case 2:\n",
                                  "\texpected value for lni33: 0\n",
                                  "\tcomputed value for lni33:", round(lnis$lni33, 3)))
  
  # success message at the end of the test analysis
  print("Success: Test analysis completed.")
}





