########################################## GLOBAL PARAMETERS #########################################
if (!exists("BASE_DIR")) BASE_DIR <<- paste(dirname(sys.frame(1)$ofile), "/", sep = "")

############################################# FUNCTIONS ##############################################
#----------------------------------------------------------------------------------------------------#
#   function: initSimulation
#     Sources required files and classes.
#     param:  modelType
#         the type of model used for the simulation
#----------------------------------------------------------------------------------------------------#
initSimulation <- function(modelType) {
  
  # constants
  if (!exists("MODEL_TYPES")) source(paste(BASE_DIR, "constants.R", sep = ""))
  
  # source VOD class
  if (!exists("Vod", mode="function")) source(paste(BASE_DIR, "vod.R", sep = ""))
  
  modelTypeFound <- FALSE
  # if default model: cource symmetric + asymmetric players (one-shot VOD mixed strategy equilibria)
  if (modelType == MODEL_TYPES[1]) {
    if(!exists("SymmetricPlayer", mode="function")) source(paste(PLAYERS_DIR,
                                                                 "playerSymmetric.R", sep = ""))
    if(!exists("AsymmetricPlayer", mode="function")) source(paste(PLAYERS_DIR,
                                                                  "playerAsymmetric.R", sep = ""))
    modelTypeFound <- TRUE
    
    # if not: find and source required model
  } else {
    for (i in 2:length(MODEL_TYPES)) {
      if (modelType == MODEL_TYPES[i]) {
        playerType <- paste("player", MODEL_TYPES[i], sep = "")
        playerFile <- paste(playerType, ".R", sep = "")
        if(!exists(playerType, mode="function")) source(paste(PLAYERS_DIR, playerFile, sep = ""))
        modelTypeFound <- TRUE
      }
    }
  }
  if (!modelTypeFound) {
    stop(paste("Unknown model type:", modelType))
  }
}

#----------------------------------------------------------------------------------------------------#
#   function: initPlayers
#     Initializes the players.
#     param:  modelType
#         the type of model used for the simulation
#     param:  vodType
#         the type of the VOD
#----------------------------------------------------------------------------------------------------#
initPlayers <- function(modelType, vodType) {
  
  # determining the cooperation costs per player, depending on VOD type
  coopCosts <- c()
  if (vodType == VOD_TYPES[1]) {
    coopCosts <- c(COOP_COST_SYMM, COOP_COST_SYMM, COOP_COST_SYMM)
  } else if (vodType == VOD_TYPES[2]) {
    coopCosts <- c(COOP_COST_ASYMM1, COOP_COST_SYMM, COOP_COST_SYMM)
  } else if (vodType == VOD_TYPES[3]) {
    coopCosts <- c(COOP_COST_ASYMM2, COOP_COST_SYMM, COOP_COST_SYMM)
  } else {
    stop(paste("Unknown VOD type:", vodType))
  }
  
  # initialization of players
  players <- list()
  for (currPlayer in 1:PLAYERS_CNT) {
    currCoopCosts <- coopCosts[currPlayer]
    
    # one-shot mixed strategy equilibria for symmetric / asymmetric VODs
    if (modelType == MODEL_TYPES[1]) {
      if (vodType == VOD_TYPES[1]) {
        players[[currPlayer]] <- SymmetricPlayer$new(currPlayer, currCoopCosts)
      } else {
        players[[currPlayer]] <- AsymmetricPlayer$new(currPlayer, currCoopCosts, coopCosts[-currPlayer])
      }
      
      # coordinate-4
    } else if (modelType == MODEL_TYPES[2]) {
      players[[currPlayer]] <- CoordinateXPlayer$new(currPlayer, currCoopCosts, COORD_X)
      
      # classic Q-Learning
    } else if (modelType == MODEL_TYPES[3]) {
      players[[currPlayer]] <- ClassicQPlayer$new(currPlayer, currCoopCosts)
      
      # random
    } else if (modelType == MODEL_TYPES[4]) {
      players[[currPlayer]] <- RandomPlayer$new(currPlayer, currCoopCosts)
      
      # coordinate-4 with epsilon as noise factor
    } else if (modelType == MODEL_TYPES[5]) {
      players[[currPlayer]] <- CoordinateXEpsilonNoisePlayer$new(currPlayer, currCoopCosts, COORD_X)
      
      # classic Q-Learning with epsilon as noise factor
    } else if (modelType == MODEL_TYPES[6]) {
      players[[currPlayer]] <- ClassicQEpsilonNoisePlayer$new(currPlayer, currCoopCosts)
      
      # win-stay loose-shift (Helbing, 2008)
    } else if (modelType == MODEL_TYPES[7]) {
      players[[currPlayer]] <- WinStayLooseShiftPlayer$new(currPlayer, currCoopCosts)
      
      # unknown
    } else {
      stop(paste("Unknown model type:", modelType))
    }
  }
  
  return(players)
}

#----------------------------------------------------------------------------------------------------#
#   function: createSimulationsBaseDirectory
#     Creates the base directory to store VOD simulation data in.
#     param:  modelType
#         the type of model to create the directory for
#----------------------------------------------------------------------------------------------------#
createSimulationsBaseDirectory <- function(modelType) {
  
  # creation of base directory for the model type
  modelTypeDir <- paste(SIM_DIR, modelType, "/", sep = "")
  if (!file.exists(modelTypeDir)) {
    dir.create(modelTypeDir)
  }
  
  # creation of base directory for the date
  dateDir <- paste(modelTypeDir, gsub("-", "", Sys.Date(), fixed = TRUE), "/", sep = "")
  if (!file.exists(dateDir)) {
    dir.create(dateDir)
  }
  
  # creation of base directory for the simulations
  simDir <- NA
  dirCnt <- 0
  while (is.na(simDir) || file.exists(simDir)) {
    dirCnt <- dirCnt+1
    simDir <- paste(dateDir, dirCnt, "/", sep = "")
  }
  dir.create(simDir)
  
  return(simDir)
}

#----------------------------------------------------------------------------------------------------#
#   function: createVodDirectory
#     Creates a directory to store the simulation data for a specific type of VOD in.
#     param:  baseDir
#         the base directory
#     param:  vodType
#         the type of VOD to create the directory for
#----------------------------------------------------------------------------------------------------#
createVodDirectory <- function(baseDir, vodType) {
  
  # creation of base directory for the VOD type
  vodTypeDir <- paste(baseDir, vodType, "/", sep = "")
  if (!file.exists(vodTypeDir)) {
    dir.create(vodTypeDir)
  }
  
  return(vodTypeDir)
}

#----------------------------------------------------------------------------------------------------#
#   function: getModelParameters
#     Retrieves the comprehensive model parameters in a data frame.
#     param:  vod
#         the VOD to retrieve parameters from
#     param:  modelType
#         the type of model used (e.g., default, CoordinateX, ... - for details, see 'constants.R')
#     param:  vodType
#         the type of VOD used (sym, asym1, asym2)
#     param:  vodCount
#         the number of VODs to be performed during one simulation
#     param:  roundsPerVod
#         the amount of rounds played in one VOD instance
#----------------------------------------------------------------------------------------------------#
getModelParameters <- function(vod, modelType, vodType, vodCount, roundsPerVod) {
  
  players <- vod$players
  playersCount <- length(players)
  
  # initialization of general parameters
  modelParams <- data.frame("model_type" = modelType, 
                            "vod_type" = vodType, 
                            "vod_count" = vodCount, 
                            "rounds_per_vod" = roundsPerVod,
                            "players_count" = playersCount,
                            "util_max" = UTIL_MAX,
                            "util_none" = UTIL_NONE,
                            stringsAsFactors=FALSE)
  
  # player specific parameters / model specific parameters
  playerParameters <- c()
  for (i in 1:playersCount) {
    playerParameters <- c(playerParameters, players[[i]]$getModelParameters())
  } 
  j <- 1
  while (j < length(playerParameters)) {
    modelParams[playerParameters[j]] <- playerParameters[j+1]
    j <- j+2
  }
  
  return(modelParams)
}

#----------------------------------------------------------------------------------------------------#
#   function: storeModelParameters
#     Retrieves the comprehensive model parameters in a data frame.
#     param:  modelParams
#         the model parameters to be stored
#     param:  directory
#         the directory to store the parameters in
#----------------------------------------------------------------------------------------------------#
storeModelParameters <- function(modelParams, directory) {
  
  # row binding of parameters to have a single all-inclusive table for all VODs and models 
  # (if possible)
  parameterRows <- modelParams[[1]]
  fileCnt <- 1
  
  for (i in 2:length(modelParams)) {
    currentParameters <- modelParams[[i]]
    if (ncol(parameterRows) == ncol(currentParameters)) {
      parameterRows <- rbind(parameterRows, currentParameters)
    } else {
      write.csv(parameterRows, file = paste(directory, "model-params-", fileCnt, ".csv", sep = ""))
      parameterRows <- currentParameters
      fileCnt <- fileCnt+1
    }
  }
  
  if (fileCnt == 1) {
    write.csv(parameterRows, file = paste(directory, "model-params.csv", sep = ""))
  } else {
    write.csv(parameterRows, file = paste(directory, "model-params-", fileCnt, ".csv", sep = ""))
  }
}

#----------------------------------------------------------------------------------------------------#
#   function: storeData
#     Stores the given data.
#     param:  data
#         the data to be stored
#     param:  directory
#         the directory to store the data in
#     param:  vodCount
#         the counter of the vod to be stored
#----------------------------------------------------------------------------------------------------#
storeData <- function(data, directory, vodCount) {
  filename <- paste(directory, BASE_FILENAME, vodCount, ".Rdata", sep = "")
  save(data, file=filename)
}

#----------------------------------------------------------------------------------------------------#
#   function: computeSimulation
#     Start and central organizational point of the simulation. Within this function the whole logic
#     is composed together to completely simulate the VOD and store the results.
#     param:  modelType
#         the type of model used for the simulation - for details, see constants.R
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: "all", "VOD_TYPES[x]"
#     param:  vodCount
#         the amount of VODs
#     param:  roundsPerVod
#         the amount of interaction rounds per VOD
#----------------------------------------------------------------------------------------------------#
computeSimulation <- function(modelType = MODEL_TYPES[7],
                              vodType = "all",
                              vodCount = 10,              
                              roundsPerVod = 100) {       
  
  # initializations
  initSimulation(modelType)
  baseDirectory <- createSimulationsBaseDirectory(modelType)
  if (vodType == "all") {
    vodType <- VOD_TYPES
  }
  modelParams <- list()
  
  # looping over VOD types (e.g., sym, asym1, asym2)
  for (currVodType in 1:length(vodType)) {
    
    currVodType <- vodType[currVodType]
    directory <- createVodDirectory(baseDirectory, currVodType)
    
    # looping over the amount of VOD instancess played
    for (currVod in 1:vodCount) {
      
      # initializing the players
      players <- initPlayers(modelType, currVodType)
      
      # creating a new VOD for the players
      vod <- Vod$new(players)
      
      # actual VOD simulation
      for (currRound in 1:roundsPerVod) {
        vod$computeRound()
      }
      
      # data storage
      storeData(vod$history, directory, currVod)
      
      # caching of current model's parameters
      modelParams[[currVodType]] <- getModelParameters(vod, modelType, currVodType, 
                                                       vodCount, roundsPerVod)
    }
  }
  # joint storage of all model parameters
  storeModelParameters(modelParams, baseDirectory)
}
