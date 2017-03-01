########################################## GLOBAL PARAMETERS #########################################
BASE_DIR <- dirname(sys.frame(1)$ofile)

############################################## CLASSES ###############################################


#####------------------------------------------ Player ------------------------------------------#####
# class: Player
#     Root class representing the basic player. This class holds all the parameters and functions 
#     shared by all implementations of different players. The main goal when extending this class
#     is to override the "computeAction" function, which holds the actual behavioral strategies by 
#     deciding which action to take.
#----------------------------------------------------------------------------------------------------#
Player <- setRefClass("Player",
                      
                      #------------------------------------------------------------------------------# 
                      #   class parameters (public by default)
                      #     param:  ID 
                      #         the player's identifier
                      #     param:  coopCost
                      #         the player's cost to cooperate
                      #     param:  history
                      #         the player's game history, consisting of round, all player actions,
                      #         player's own utility
                      #------------------------------------------------------------------------------#
                      fields = c("ID", "coopCost", "history"),
                      
                      #------------------------------------------------------------------------------# 
                      #   class methods (public by defualt)
                      #------------------------------------------------------------------------------#
                      methods = list(
                        
                        #----------------------------------------------------------------------------# 
                        #   function: initialize
                        #     Initializes the Player: ID is set, history is initialized with an empty
                        #     data frame and player is validated
                        #     param:  ID
                        #         the player's ID
                        #     param:  coopCost
                        #         the player's cost to cooperate
                        #----------------------------------------------------------------------------#
                        initialize = function(ID, coopCost) {
                          ID <<- ID
                          coopCost <<- coopCost
                          history <<- data.frame("round" = numeric(1), "player1" = character(1), 
                                                 "player2" = character(1), "player3" = character(1),
                                                 "util" = numeric(1), stringsAsFactors=FALSE)
                          validate()
                          if (LOG_LEVEL == "debug") {
                            print(paste("Player", ID, "successfully created!"))
                          }
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: validate
                        #     Integrity check for player: ID must be numeric.
                        #----------------------------------------------------------------------------#
                        validate = function() {
                          if (!is.numeric(ID)) {
                            stop("Error during player validation: 'ID' must be numeric!")
                          }
                          if (!is.numeric(coopCost)) {
                            stop("Error during player validation: 'coopCost' must be numeric!")
                          }
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: assessAction
                        #     Assesses the player's action.
                        #     param:  round
                        #         the round
                        #     param:  actions
                        #         actions played in the corresponding round by all players
                        #     param:  util
                        #         utility earned in the corresponding round, based on the action taken
                        #----------------------------------------------------------------------------#
                        assessAction = function(round, actions, util) {
                          history <<- rbind(history, c(round, actions, util))
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: computeAction
                        #     Computes which action to take. This implementation is just a 
                        #     placeholder for more elaborate implementations, such as reinforcement 
                        #     declarative memory. This method must be overwritten by inheriting 
                        #     classes. The current implementation corresponds to the mixed-strategy
                        #     equilibrium as described in Diekmann & Przepiorka (2016), p.1316.
                        #----------------------------------------------------------------------------#
                        computeAction = function() {
                          coopProb <- 1 - sqrt(coopCost/UTIL_MAX)
                          if (runif(1, 0, 1) < coopProb) {
                            return(COOPERATE)
                          } else {
                            return(DEVIATE)
                          }
                        }
                      )
)

#####------------------------------------- AsymmetricPlayer -------------------------------------#####
# class: AsymmetricPlayer
#     Class extending the basic Player class. This class represents a player using reinforcement 
#     learning for decision making.
#----------------------------------------------------------------------------------------------------#
AsymmetricPlayer <- setRefClass("AsymmetricPlayer",
                                
                                #--------------------------------------------------------------------# 
                                #  class inheritance
                                #--------------------------------------------------------------------#
                                contains = "Player",
                                
                                #--------------------------------------------------------------------# 
                                #   class parameters (public by default)
                                #     param:  othersCoopCosts
                                #         the other player's costs to cooperate
                                #--------------------------------------------------------------------#
                                fields = c("othersCoopCosts"),
                                
                                #--------------------------------------------------------------------# 
                                #   class methods (public by defualt)
                                #--------------------------------------------------------------------#
                                methods = list(
                                  
                                  #------------------------------------------------------------------# 
                                  #   function: initialize
                                  #     Initializes the Player: ID is set, history is initialized with 
                                  #     an empty data frame and player is validated
                                  #     param:  ID
                                  #         the player's ID
                                  #     param:  coopCost
                                  #         the player's cost to cooperate
                                  #------------------------------------------------------------------#
                                  initialize = function(ID, coopCost, othersCoopCosts) {
                                    othersCoopCosts <<- othersCoopCosts
                                    callSuper(ID, coopCost)
                                    if (LOG_LEVEL == "debug") {
                                      print(paste("Asymmetric Player", ID, "successfully created!"))
                                    }
                                  },
                                  
                                  #------------------------------------------------------------------# 
                                  #   function: validate
                                  #     Integrity check for player: othersCoopCosts must be a vector 
                                  #     with two elements.
                                  #------------------------------------------------------------------#
                                  validate = function() {
                                    if (!length(othersCoopCosts == 2)) {
                                      stop(paste("Error during player validation: 'othersCoopCosts' 
                                                 must be a vector with two elements, but is:", 
                                                 othersCoopCosts))
                                    }
                                    callSuper()
                                  },
                                  
                                  #------------------------------------------------------------------# 
                                  #   function: computeAction
                                  #     Computes which action to take. The current implementation 
                                  #     corresponds to the mixed-strategy equilibrium as described 
                                  #     in Diekmann & Przepiorka (2016), p.1317.
                                  #------------------------------------------------------------------#
                                  computeAction = function() {
                                    othersCostUtilRatio <- 1
                                    for (i in 1:length(othersCoopCosts)) {
                                      othersCostUtilRatio <- othersCostUtilRatio *
                                        (othersCoopCosts[i] / UTIL_MAX)
                                    }
                                    othersCostUtilRatio <- othersCostUtilRatio^(1/2)
                                    coopProb <- 1 - (UTIL_MAX/coopCost) * othersCostUtilRatio
                                    if (runif(1, 0, 1) < coopProb) {
                                      return(COOPERATE)
                                    } else {
                                      return(DEVIATE)
                                    }
                                  }
                                )
)

#####----------------------------------- ReinforcementPlayer ------------------------------------#####
# class: ReinforcementPlayer
#     Class extending the basic Player class. This class represents a player using reinforcement 
#     learning for decision making.
#----------------------------------------------------------------------------------------------------#
ReinforcementPlayer <- setRefClass("ReinforcementPlayer",
                                   
                                   #-----------------------------------------------------------------# 
                                   #  class inheritance
                                   #-----------------------------------------------------------------#
                                   contains = "Player",
                                   
                                   #-----------------------------------------------------------------# 
                                   #  class methods (public by defualt)
                                   #-----------------------------------------------------------------#
                                   methods = list(
                                     
                                     #---------------------------------------------------------------# 
                                     #  function: computeAction
                                     #    TODO
                                     #---------------------------------------------------------------#
                                     computeAction = function() {
                                       return(COOPERATE)
                                     }
                                   )
)


############################################# FUNCTIONS ##############################################
#----------------------------------------------------------------------------------------------------#
#   function: initSimulation
#     Imports required classes.
#     param:  modelType
#         the type of model used for the simulation
#----------------------------------------------------------------------------------------------------#
initSimulation <- function(modelType) {
  
  # constants
  source(paste(BASE_DIR, "constants.R", sep = ""))
  
  # VOD class
  if(!exists("Vod", mode="function")) source(paste(BASE_DIR, "vod.R", sep = ""))
  
  modelTypeFound <- FALSE
  # if default model: symmetric + asymmetric (one-shot VOD mixed strategy equilibria)
  if (modelType == MODEL_TYPES[1]) {
    if(!exists("SymmetricPlayer", mode="function")) source(paste(BASE_DIR,
                                                                 "symmetric-player.R", sep = ""))
    if(!exists("AsymmetricPlayer", mode="function")) source(paste(BASE_DIR,
                                                                  "asymmetric-player.R", sep = ""))
    modelTypeFound <- TRUE
    
    # if not: find required model
  } else {
    for (i in 2:length(MODEL_TYPES)) {
      if (modelType == MODEL_TYPES[i]) {
        playerType <- paste(MODEL_TYPES[i], "Player", sep = "")
        playerFile <- paste(playerType, ".R", sep = "")
        if(!exists(playerType, mode="function")) source(paste(BASE_DIR, playerFile, sep = ""))
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
  } else if (currVodType == VOD_TYPES[2]) {
    coopCosts <- c(COOP_COST_ASYMM1, COOP_COST_SYMM, COOP_COST_SYMM)
  } else if (currVodType == VOD_TYPES[3]) {
    coopCosts <- c(COOP_COST_ASYMM2, COOP_COST_SYMM, COOP_COST_SYMM)
  } else {
    stop(paste("Unknown VOD type:", currVodType))
  }

  # initialization of players
  players <- list()
  for (currPlayer in 1:PLAYERS_CNT) {
    currCoopCosts <- coopCosts[currPlayer]
    
    # default
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
  modelTypeDir <- paste(BASE_DIR, "/simulations/", modelType, "/", sep = "")
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
