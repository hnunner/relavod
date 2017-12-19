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
  
  # source appropriate model class
  modelTypeFound <- FALSE
  for (i in 1:length(MODEL_TYPES)) {
    if (modelType == MODEL_TYPES[i]) {
      playerType <- paste("player", MODEL_TYPES[i], sep = "")
      playerFile <- paste(playerType, ".R", sep = "")
      if(!exists(playerType, mode="function")) source(paste(PLAYERS_DIR, playerFile, sep = ""))
      modelTypeFound <- TRUE
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
initPlayers <- function(modelType, vodType, 
                        
                        randomCoopRatio,
                        
                        balancingType, 
                        socialBehavior,
                        propStart, 
                        epsilonStart, 
                        epsilonDecay, 
                        alpha, 
                        gamma,
                        
                        classicX, 
                        classicPlayersPerState, 
                        
                        coordX,
                        
                        seqX) {
  
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
    
    # Random
    if (modelType == MODEL_TYPES[1]) {
      players[[currPlayer]] <- RandomPlayer$new(currPlayer, coopCosts,
                                                randomCoopRatio)
      
    # ClassicQ
    } else if (modelType == MODEL_TYPES[2]) {
      players[[currPlayer]] <- ClassicQPlayer$new(currPlayer, coopCosts, 
                                                  classicX, classicPlayersPerState, balancingType,
                                                  propStart, epsilonStart, epsilonDecay, alpha, 
                                                  gamma, socialBehavior)
      
    # CoordinateX
    } else if (modelType == MODEL_TYPES[3]) {
      players[[currPlayer]] <- CoordinateXPlayer$new(currPlayer, coopCosts, 
                                                     coordX, balancingType,propStart, epsilonStart, 
                                                     epsilonDecay, alpha, gamma, socialBehavior)
      
    # SequenceX
    } else if (modelType == MODEL_TYPES[4]) {
      if (LOG_LEVEL == "all") {
        print(paste("Starting to initialize SequenceX player ", currPlayer))
      }
      players[[currPlayer]] <- SequenceXPlayer$new(currPlayer, coopCosts,
                                                   seqX, balancingType,propStart, epsilonStart,
                                                   epsilonDecay, alpha, gamma, socialBehavior)
      
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
  if (!dir.exists(modelTypeDir)) {
    dir.create(modelTypeDir)
  }
  
  # creation of base directory for the simulations
  simDir <- NA
  dirCnt <- 0
  while (is.na(simDir) || dir.exists(simDir)) {
    dirCnt <- dirCnt+1
    simDir <- paste(modelTypeDir, dirCnt, "/", sep = "")
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
  if (!dir.exists(vodTypeDir)) {
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
computeSimulation <- function(modelType = MODEL_TYPES[2],
                              vodType = "all",
                              vodCount = 10,              
                              roundsPerVod = 100,
                              
                              randomCoopRatio = RANDOM_COOP_RATIO,
                              
                              balancingType = BALANCING_TYPE, 
                              socialBehavior = SOCIAL_BEHAVIOR,
                              propStart = PROP_START, 
                              epsilonStart = EPSILON_START, 
                              epsilonDecay = EPSILON_DECAY, 
                              alpha = ALPHA, 
                              gamma = GAMMA,
                              
                              classicX = CLASSIC_X, 
                              cQPlayersPerState = CLASSIC_PLAYERS_PER_STATE, 
                              
                              coordX = COORD_X,
                              
                              seqX = SEQ_X) {       
  
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
      players <- initPlayers(modelType, currVodType, randomCoopRatio, balancingType,
                             socialBehavior, propStart, epsilonStart, epsilonDecay,
                             alpha, gamma, classicX, classicPlayersPerState, coordX, seqX)
      
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



#----------------------------------------------------------------------------------------------------#
#   function: computeRandomSimulation
#----------------------------------------------------------------------------------------------------#
computeRandomSimulation <- function(vodCount, roundsPerVod, randomCoopRatio) {       
  computeSimulation(modelType = "Random", 
                    vodType = "all", 
                    vodCount = vodCount, 
                    roundsPerVod = roundsPerVod,
                    randomCoopRatio = randomCoopRatio)
}

#----------------------------------------------------------------------------------------------------#
#   function: computeClassicQSimulation
#----------------------------------------------------------------------------------------------------#
computeClassicQSimulation <- function(vodCount, roundsPerVod, balancingType, socialBehavior,
                                      propStart, epsilonStart, epsilonDecay, alpha, gamma,
                                      classicX, cQPlayersPerState) {       
  computeSimulation(modelType = "ClassicQ", 
                    vodType = "all", 
                    vodCount = vodCount, 
                    roundsPerVod = roundsPerVod,
                    
                    balancingType = balancingType,
                    socialBehavior = socialBehavior,
                    propStart = propStart,
                    epsilonStart = epsilonStart,
                    epsilonDecay = epsilonDecay,
                    alpha = alpha,
                    gamma = gamma,
                    
                    classicX = classicX,
                    cQPlayersPerState = cQPlayersPerState)
}

#----------------------------------------------------------------------------------------------------#
#   function: computeCoordinateXSimulation
#----------------------------------------------------------------------------------------------------#
computeCoordinateXSimulation <- function(vodCount, roundsPerVod, balancingType, socialBehavior,
                                         propStart, epsilonStart, epsilonDecay, alpha, gamma, 
                                         coordX) {       
  computeSimulation(modelType = "CoordinateX", 
                    vodType = "all", 
                    vodCount = vodCount, 
                    roundsPerVod = roundsPerVod,
                    
                    balancingType = balancingType,
                    socialBehavior = socialBehavior,
                    propStart = propStart,
                    epsilonStart = epsilonStart,
                    epsilonDecay = epsilonDecay,
                    alpha = alpha,
                    gamma = gamma,
                    
                    coordX = coordX)
}

#----------------------------------------------------------------------------------------------------#
#   function: computeSequenceXSimulation
#----------------------------------------------------------------------------------------------------#
computeSequenceXSimulation <- function(vodCount, roundsPerVod, balancingType, socialBehavior,
                                       propStart, epsilonStart, epsilonDecay, alpha, gamma, seqX) {       
  computeSimulation(modelType = "SequenceX", 
                    vodType = "all", 
                    vodCount = vodCount, 
                    roundsPerVod = roundsPerVod,
                    
                    balancingType = balancingType,
                    socialBehavior = socialBehavior,
                    propStart = propStart,
                    epsilonStart = epsilonStart,
                    epsilonDecay = epsilonDecay,
                    alpha = alpha,
                    gamma = gamma,
                    
                    seqX = seqX)
}
