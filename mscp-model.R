########################################## GLOBAL PARAMETERS #########################################
# game design
PLAYERS_CNT <- 3
UTIL_MAX <- 100
COOP_COST <- 40
UTIL_MIN <- UTIL_MAX - COOP_COST
UTIL_NONE <- 0
# actions
COOPERATE <- "c"
DEVIATE <- "d"
# log level
LOG_LEVEL <- "debug"    # possible: "debug", "none"
# file system
BASE_DIR <- paste(dirname(sys.frame(1)$ofile), "/simulations/", sep = "")
BASE_FILENAME <- "sim-"


############################################## CLASSES ###############################################
#####------------------------------------------- VOD --------------------------------------------#####
# class: Vod
#     Class representing the Volunteer's Dilemma game. It holds the basic game logic (function 
#     "computeRound"), the players, the game history and a counter for the rounds played.
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
Vod <- setRefClass("Vod",
                   
                   #---------------------------------------------------------------------------------# 
                   #  class parameters (public by default)
                   #    param:  players 
                   #        list of players involved in the VOD
                   #    param:  history
                   #        game history, consisting of round, player actions, player utilities
                   #    param:  roundsPlayed
                   #        counter for played rounds of the VOD
                   #---------------------------------------------------------------------------------# 
                   fields = c("players", "history", "roundsPlayed"),
                   
                   #---------------------------------------------------------------------------------# 
                   #  class methods (public by defualt)
                   #---------------------------------------------------------------------------------# 
                   methods = list(
                     
                     #-------------------------------------------------------------------------------# 
                     #  function: initialize
                     #    Initializes the VOD: players are set, history is initialized with an empty
                     #    data frame, round counter is initialized with 0 and VOD is validated
                     #    param:  players
                     #        the players involved in the VOD
                     #-------------------------------------------------------------------------------# 
                     initialize = function(players) {
                       players <<- players
                       history <<- data.frame("round" = numeric(1), "player1" = character(1), 
                                              "player2" = character(1), "player3" = character(1),
                                              "util1" = numeric(1), "util2" = numeric(1), 
                                              "util3" = numeric(1), stringsAsFactors=FALSE)
                       roundsPlayed <<- 0
                       validate()
                       if (LOG_LEVEL == "debug") {
                         print("VOD successfully created!")
                       }
                     },
                     
                     #-------------------------------------------------------------------------------# 
                     #  function: validate
                     #    Integrity check of the VOD: checking the right amount of players.
                     #-------------------------------------------------------------------------------# 
                     validate = function() {
                       if (length(players) != PLAYERS_CNT) {
                         stop(paste("Invalid amount of players. ", 
                                    "Required: ", PLAYERS_CNT, "; ",
                                    "Received: ", length(players)))
                       } 
                     },
                     
                     #-------------------------------------------------------------------------------# 
                     #  function: computeRound
                     #    Funciton holding the actual game logic of a single round of the VOD.
                     #-------------------------------------------------------------------------------# 
                     computeRound = function() {
                       
                       # 1. simulation of actions for each player
                       actions <- c()
                       coop <- FALSE
                       for (i in 1:length(players)) {
                         # retrieval of list objects with [[]], instead of just reference pointer 
                         # with [] --- for further information, see: 
                         # https://rforpublichealth.blogspot.nl/2015/03/basics-of-lists.html
                         action <- players[[i]]$computeAction()
                         if (action == COOPERATE) {
                           coop <- TRUE 
                         }
                         actions <- c(actions, action)
                       }
                       
                       # 2. calculation of utilities, based on the player's actions
                       utils <- c()
                       for (i in 1:length(players)) {
                         if (!coop) {
                           utils <- c(utils, UTIL_NONE)
                         } else {
                           if (actions[i] == COOPERATE) {
                             utils <- c(utils, UTIL_MIN)
                           } 
                           if (actions[i] == DEVIATE) {
                             utils <- c(utils, UTIL_MAX)
                           }
                         }
                       }
                       
                       # round simulation completed
                       roundsPlayed <<- roundsPlayed + 1
                       
                       # updating the player's personal game histories
                       for (i in 1:length(players)) {
                         action <- players[[i]]$updateHistory(roundsPlayed, actions, utils[i])
                       }
                       
                       # updating the VOD's game history
                       history <<- rbind(history, c(roundsPlayed, actions, utils))
                       
                       if (LOG_LEVEL == "debug") {
                         print(paste("Round", roundsPlayed, "successfully computed!"))
                       }
                     }
                   )
)

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
                      #     param:  history
                      #         the player's game history, consisting of round, all player actions,
                      #         player's own utility
                      #------------------------------------------------------------------------------#
                      fields = c("ID", "history"),
                      
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
                        #----------------------------------------------------------------------------#
                        initialize = function(ID) {
                          ID <<- ID
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
                            stop("Error during player validation: ID must be numeric!")
                          }
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: updateHistory
                        #     Updates the player's personal game history with the given parameters.
                        #     param:  round
                        #         the round
                        #     param:  actions
                        #         actions played in the corresponding round by all players
                        #     param:  util
                        #         utility earned in the corresponding round, based on the action taken
                        #----------------------------------------------------------------------------#
                        updateHistory = function(round, actions, util) {
                          history <<- rbind(history, c(round, actions, util))
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: computeAction
                        #     Computes which action to take. This implementation is just a 
                        #     placeholder for more elaborate implementations, such as reinforcement 
                        #     declarative memory. This method must be overwritten by inheriting 
                        #     classes.
                        #----------------------------------------------------------------------------#
                        computeAction = function() {
                          if (runif(1, 0, 1) < 1/PLAYERS_CNT) {
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
#   function: createDirectory
#     Creates a directory to store simulation data in.
#     param:  modelType
#         the type of model to create the directory for
#----------------------------------------------------------------------------------------------------#
createDirectory <- function(modelType) {
  # creation of base directory for the model type
  modelDir <- paste(BASE_DIR, modelType, "/", sep = "")
  if (!file.exists(modelDir)) {
    dir.create(modelDir)
  }
  
  # creation of base directory for the date
  dateDir <- paste(modelDir, gsub("-", "", Sys.Date(), fixed = TRUE), "/", sep = "")
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
#   param:  simulationCount
#       the amount of overall simulations
#   param:  roundsPerSimulation
#       the amount of rounds per simulation
#----------------------------------------------------------------------------------------------------#
computeSimulation <- function(modelType = "default",
                              simulationCount = 5, 
                              roundsPerSimulation = 50) {
  
  directory <- createDirectory(modelType)
  
  for (currSim in 1:simulationCount) {
    
    players <- list()
    for (i in 1:PLAYERS_CNT) {
      if (modelType == "default") {
        players[[i]] <- Player$new(i)
      } else if (modelType == "reinf") {
        players[[i]] <- ReinforcementPlayer$new(i)
      } else if (modelType == "decl-mem") {
        stop("Declarative memory not implemented yet!")
      } else if (modelType == "mel-vs-max") {
        stop("Melioration vs. maximization not implemented yet!")
      } else {
        stop(paste("Unknown model type:", modelType))
      }
    }
    vod <- Vod$new(players)
    for (currRound in 1:roundsPerSimulation) {
      vod$computeRound()
    }
    
    storeData(vod$history, directory, currSim)
  }
}
