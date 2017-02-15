########################################## GLOBAL PARAMETERS #########################################
# file system
BASE_DIR <- paste(dirname(sys.frame(1)$ofile), "/simulations/", sep = "")
BASE_FILENAME <- "sim-"
# log level
LOG_LEVEL <- "none"    # possible: "debug", "none"


#----------------------------------------------------------------------------------------------------#
# function: importVodSimData
#     Imports VOD simulation data. 
#     Data is stored as: "BASE_DIR/modelType/Date-dateCount/sim-simCount.Rdata"
#     Available for modelType: "default", "reinf", "decl-mem", "mel-vs-max"
#     Date format: yyyymmdd
#     param:  modelType
#         the model type used by the simulation
#         possible: "default", "reinf", "decl-mem", "mel-vs-max"
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: "sym", "asym1", "asym2"
#     param:  date
#         the date of the simulation 
#         (if undefined: latest available date)
#     param:  dateCount
#         defines the round of simulation of the given date
#         (if undefined: latest available round of simulations)
#----------------------------------------------------------------------------------------------------#
importVodSimData <- function(modelType = "default", 
                             vodType = "sym",
                             date = "latest",
                             dateCount = "latest") {
  
  modelDir <- paste(BASE_DIR, modelType, sep = "")
  vodTypeDir <- paste(modelDir, "/", vodType, sep = "")
  
  if (date == "latest") {
    dateDirs <- list.dirs(vodTypeDir, recursive = FALSE)
    dates <- gsub(paste(vodTypeDir, "/", sep = ""), "", dateDirs, fixed = TRUE)
    date <- max(dates)
  }
  dateDir <- paste(vodTypeDir, "/", date, sep = "")
  
  
  if (dateCount == "latest") {
    dateCountDirs <- list.dirs(dateDir, recursive = FALSE)
    dateCounts <- gsub(paste(dateDir, "/", sep = ""), "", dateCountDirs, fixed = TRUE)
    dateCount <- max(dateCounts)
  }
  dateCountDir <- paste(dateDir, "/", dateCount, sep = "")
  
  vodSimData = list()
  simCountFiles <- list.files(dateCountDir, recursive = FALSE)
  for (i in 1:length(simCountFiles)) {
    filename <- paste(dateCountDir, "/", BASE_FILENAME, i, ".Rdata", sep = "")
    vodSimData[[i]] <- get(load(filename))
    if (LOG_LEVEL == "debug") {
      print(paste("Success: Data import from:", filename))
    }
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
  moves[moves$player1 == "c" & moves$player2 == "d" & moves$player3 == "d", ]$lniSequence <- 1
  moves[moves$player1 == "d" & moves$player2 == "c" & moves$player3 == "d", ]$lniSequence <- 2
  moves[moves$player1 == "d" & moves$player2 == "d" & moves$player3 == "c", ]$lniSequence <- 3
  return(moves$lniSequence)
}


#----------------------------------------------------------------------------------------------------#
# function: computeLNIs
#   Computation of the Latent Norm Index (LNI) for the given VOD data.
#   param:  vodData
#       the VOD data 
#----------------------------------------------------------------------------------------------------#
computeLNIs <- function(vodData) {
  lniSequence <- extractLNISequence(vodData)
  
  ### TEST ###
  #lniSequence <- createLNITestSequence1()
  #lniSequence <- extractLNISequence(createVodTestData2())
  ### TEST ###
  
  # 1-sequences
  oneSequences <- c()
  i <- 1
  while (i < length(lniSequence)) {
    currInteraction <- lniSequence[i]
    
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

#----------------------------------------------------------------------------------------------------#
# function: analyzeData
#     Starting point for the data analysis.
#     param:  modelType
#         the model type used by the simulation
#         possible: "default", "reinf", "decl-mem", "mel-vs-max"
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: "sym", "asym1", "asym2"
#     param:  date
#         the date of the simulation 
#         (if undefined: latest available date)
#     param:  dateCount
#         defines the round of simulations of the given date
#         (if undefined: latest available round of simulations)
#----------------------------------------------------------------------------------------------------#
analyzeData <- function(modelType = "default",
                        vodType = "sym",
                        date = "latest",
                        dateCount = "latest") {
  
  vodSimData <- importVodSimData(modelType = modelType, vodType = vodType, 
                                 date = date, dateCount = dateCount)
  lnis <- data.frame()
  for (i in 1:length(vodSimData)) {
    lnis <- rbind(lnis, computeLNIs(vodSimData[[i]]))
  } 
  lnis
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


createLNITestSequence1 <- function() {
  return(c(1,1,2,2,2,3,3,3,3,1,2,3,2,2,2,2,2,2,2))
}

#----------------------------------------------------------------------------------------------------#
# function: testAnalysis
#     Starting point for test analysis.
#----------------------------------------------------------------------------------------------------#
testAnalysis <- function() {
  vodTestData <- createVodTestData1()
  computeLNIs(vodTestData)
  
  vodTestData <- createVodTestData2()
  computeLNIs(vodTestData)
}





