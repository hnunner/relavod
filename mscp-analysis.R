########################################## GLOBAL PARAMETERS #########################################
# file system
BASE_DIR <- paste(dirname(sys.frame(1)$ofile), "/simulations/", sep = "")
BASE_FILENAME <- "vod-simulation-"


#----------------------------------------------------------------------------------------------------#
# function: importVodSimData
#     Imports VOD simulation data. Data is stored as: "vod-simulation-MODELTYPE-TIMESTAMP.Rdata"
#     Available MODELTYPEs: "default", "reinf", "decl-mem", "mel-vs-max"
#     TIMESTAMP format: yyyymmdd-hhmmss
#     param:  modelType
#         the model type used by the simulation
#     param:  timestamp
#         the timestamp of the simulation (if undefined: latest available timestamp)
#----------------------------------------------------------------------------------------------------#
importVodSimData <- function(modelType = "default", timestamp = "latest") {
  filename <- paste(BASE_DIR, BASE_FILENAME, modelType, "-", timestamp, ".Rdata", sep = "")
  # if timestamp is undefined, take latest available timestamp
  if (timestamp == "latest") {
    files <- list.files(BASE_DIR)
    fileTimestamps <- gsub(".Rdata", "", 
                           gsub(paste(BASE_FILENAME, modelType, "-", sep = ""), "", 
                                files[grepl(modelType, files)], fixed = TRUE), fixed = TRUE)
    maxTimestamp <- NA
    for (i in 1:length(fileTimestamps)) {
      if (is.na(maxTimestamp)) {
        maxTimestamp <- fileTimestamps[i]
      } else if (fileTimestamps[i] > maxTimestamp) {
        maxTimestamp <- fileTimestamps[i]
      }
    }
    filename <- paste(BASE_DIR, BASE_FILENAME, modelType, "-", maxTimestamp, ".Rdata", sep = "")
  }
  vodSimData <- get(load(filename))
  print(paste("Success: Data import from:", filename))
  return(vodSimData)
}


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
  print(lniSequence)
}

#----------------------------------------------------------------------------------------------------#
# function: analyzeData
#     Starting point for the data analysis.
#----------------------------------------------------------------------------------------------------#
analyzeData <- function() {
  vodSimData <- importVodSimData(modelType = "reinf")
  vodSimData <- importVodSimData(modelType = "reinf", timestamp = "20170208-115642")
  vodSimData <- importVodSimData()
  computeLNIs(vodSimData)
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
# function: testAnalysis
#     Starting point for test analysis.
#----------------------------------------------------------------------------------------------------#
testAnalysis <- function() {
  vodTestData <- createVodTestData1()
  computeLNIs(vodTestData)
  
  vodTestData <- createVodTestData2()
  computeLNIs(vodTestData)
}





