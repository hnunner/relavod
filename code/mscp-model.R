########################################## GLOBAL PARAMETERS #########################################
BASE_DIR <<- paste(dirname(sys.frame(1)$ofile), "/", sep = "")

############################################# FUNCTIONS ##############################################
#----------------------------------------------------------------------------------------------------#
#   function: initSimulation
#     Sources required files and classes.
#     param:  modelType
#         the type of model used for the simulation
#----------------------------------------------------------------------------------------------------#
initSimulation <- function(modelType) {
  
  # constants
  source(paste(BASE_DIR, "constants.R", sep = ""))
  
  # source VOD class
  if(!exists("Vod", mode="function")) source(paste(BASE_DIR, "vod.R", sep = ""))
  
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
    
    # default = one-shot mixed strategy equilibria for symmetric / asymmetric VODs
    if (modelType == MODEL_TYPES[1]) {
      if (vodType == VOD_TYPES[1]) {
        players[[currPlayer]] <- SymmetricPlayer$new(currPlayer, currCoopCosts)
      } else {
        players[[currPlayer]] <- AsymmetricPlayer$new(currPlayer, currCoopCosts, coopCosts[-currPlayer])
      }
      
      # coordinate-4
    } else if (modelType == MODEL_TYPES[2]) {
      players[[currPlayer]] <- Coordinate4Player$new(currPlayer, currCoopCosts)
      
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
#   function: storeData
#     Stores the given data.
#     param:  data
#         the data to be stored
#     param:  directory
#         the directory to store the data in
#     param:  simulationCnt
#         the counter of the simulation to be stored
#----------------------------------------------------------------------------------------------------#
storeData <- function(data, directory, simulationCnt) {
  filename <- paste(directory, BASE_FILENAME, simulationCnt, ".Rdata", sep = "")
  save(data, file=filename)
}

#----------------------------------------------------------------------------------------------------#
#   function: computeSimulation
#     Start and central organizational point of the simulation. Within this function the whole logic
#     is composed together to completely simulate the VOD and store the results.
#   param:  modelType
#       the type of model used for the simulation
#       possible: "default", "reinf", "decl-mem", "mel-vs-max"
#   param:  vodType
#       the type of VOD used for the simulation
#       possible: "all", "sym", "asym1", "asym2"
#   param:  simulationCount
#       the amount of overall simulations
#   param:  interactionRounds
#       the amount of interaction rounds per simulation
#----------------------------------------------------------------------------------------------------#
computeSimulation <- function(modelType = "default",
                              vodType = "all",
                              simulationCount = 30,                  # 120 (subjects) / 4 (conditions)
                              interactionRounds = 56) {
  
  # initializations
  initSimulation(modelType)
  baseDirectory <- createSimulationsBaseDirectory(modelType)
  if (vodType == "all") {
    vodType <- VOD_TYPES
  }
  
  for (currVodType in 1:length(vodType)) {
    
    currVodType <- vodType[currVodType]
    directory <- createVodDirectory(baseDirectory, currVodType)
    
    # simulation rounds resembling the amount of VOD games played in groups of players
    for (currSim in 1:simulationCount) {
      
      # initializing the players
      players <- initPlayers(modelType, currVodType)
      
      # creating a new VOD for the players
      vod <- Vod$new(players)
      
      # actual VOD simulation
      for (currInteractionRound in 1:interactionRounds) {
        vod$computeRound()
      }
      
      # data storage
      storeData(vod$history, directory, currSim)
    }
  }
}
