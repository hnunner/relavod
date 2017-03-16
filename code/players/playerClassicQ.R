######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
ROUNDS_PER_STATE <<- 3    # the amount of rounds representing a state; e.g., 3 means that a player
                          # looks at all actions taken by the player herself in the previous 3 rounds 
                          # and takes the action that produced the highest utility
PROP_START <<- 100        # initial propensity for each strategy
EPSILON_START <<- 0.1     # initial balance between exploration (epsilon) and exploration (1-epsilon)
EPSILON_DECAY <<- 0.995   # rate at which epsilon is decreasing after each completion of a strategy
ALPHA <<- 0.4             # RL learning rate, the higher the more important recently learned 
                          # information; 0 < ALPHA <= 1
GAMMA <<- 0.6             # RL discount factor, the higher the more important the previous rewards;
                          # 0 <= GAMMA <= 1

#####-------------------------------------- ClassicQPlayer --------------------------------------#####
# class: ClassicQPlayer
#     Class extending the basic Player class. This class represents a player using a classical 
#     Q-Learning approach, where action-state-pairs are being reinforced. A state is represented
#     by actions of all players in the previous rounds.
#
#     Free parameters:
#       - ROUNDS_PER_STATE
#       - PROP_START
#       - EPSILON_START
#       - EPSILON_DECAY
#       - ALPHA
#       - GAMMA
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
ClassicQPlayer <- setRefClass("ClassicQPlayer",
                              
                              #----------------------------------------------------------------------# 
                              #  class inheritance
                              #----------------------------------------------------------------------#
                              contains = "Player",
                              
                              #----------------------------------------------------------------------#
                              #   class parameters (public by default)
                              #      param:  epsilon
                              #          balance between exploration and exploitation
                              #----------------------------------------------------------------------#
                              fields = c("qTable", "currState", "epsilon", "optimalExpectedUtility"),
                              
                              #----------------------------------------------------------------------#
                              #  class methods (public by defualt)
                              #----------------------------------------------------------------------#
                              methods = list(
                                
                                #--------------------------------------------------------------------#
                                #   function: initialize
                                #     Initializes the Player.
                                #     param:  ID
                                #         the player's ID
                                #     param:  coopCost
                                #         the player's cost to cooperate
                                #--------------------------------------------------------------------#
                                initialize = function(ID, coopCost) {
                                  
                                  # initialization of the q-table
                                  # TODO: generalize initialization of q-table
                                  state <- c("000", "001", "010", "011", "100", "101", "110", "111")
                                  c_prop <- c(rep(PROP_START, 8))
                                  d_prop <- c(rep(PROP_START, 8))
                                  qTable <<- data.frame(state, c_prop, d_prop)
                                  
                                  # initialization of current state
                                  currState <<- NA
                                  
                                  # initialization of epsilon
                                  epsilon <<- EPSILON_START
                                  
                                  # initialization of the optimal utility estimate
                                  optimalExpectedUtility <<- UTIL_NONE
                                  
                                  # initializations of super class
                                  callSuper(ID, coopCost)
                                  
                                  if (LOG_LEVEL == "all") {
                                    print(paste("Classic-Q Player", ID, "successfully created!"))
                                  }
                                },
                                
                                #--------------------------------------------------------------------#
                                #   function: assessAction
                                #     Assesses the player's action.
                                #     param:  round
                                #          the round
                                #     param:  allPlayersActions
                                #          actions played in the corresponding round by all players
                                #     param:  util
                                #          utility earned in the corresponding round, based on the 
                                #          action taken
                                #--------------------------------------------------------------------#
                                assessAction = function(round, allPlayersActions, util) {
                                  
                                  if (is.na(currState)) {
                                    callSuper(round, allPlayersActions, util)
                                    return()
                                  }
                                  
                                  ownAction <- allPlayersActions[ID]
                                  qRow <- qTable[qTable$state == currState, ]
                                  
                                  oldProp <- NA
                                  if (ownAction == COOPERATE) {
                                    oldProp <- qRow$c_prop
                                  } else if (ownAction == DEVIATE) {
                                    oldProp <- qRow$d_prop
                                  } else {
                                    stop(paste("Unknown action: ", ownAction))
                                  }
                                  
                                  # calculate new propensity based on update function by 
                                  # Sutton & Barto (1998), p.148
                                  newProp <- oldProp + ALPHA *
                                    (util + GAMMA * optimalExpectedUtility - oldProp)
                                  
                                  if (ownAction == COOPERATE) {
                                    qTable[qTable$state == currState, ]$c_prop <<- newProp
                                  } else if (ownAction == DEVIATE) {
                                    qTable[qTable$state == currState, ]$d_prop <<- newProp
                                  } else {
                                    stop(paste("Unknown action: ", ownAction))
                                  }
                                  
                                  # Decay of epsilon: after a strategy has been fully
                                  # performed. Not after each action, to ensure that each
                                  # player has an equal probability to explore the same
                                  # amount of strategies.
                                  epsilon <<- epsilon * EPSILON_DECAY
                                  
                                  callSuper(round, allPlayersActions, util)
                                },
                                
                                #--------------------------------------------------------------------#
                                #  function: computeAction
                                #    Checks the state the player is currently in and selects the
                                #    best response. A state is represented by the actions a player
                                #    took in the previous rounds.
                                #--------------------------------------------------------------------#
                                computeAction = function() {
                                  
                                  action <- NA
                                  
                                  # get all previous actions by player
                                  prevActions <- history[history$round >= 1, ID + 1]
                                  
                                  # if not enough actions performed yet, choose random action
                                  if (length(prevActions) < ROUNDS_PER_STATE) {
                                    action <- if(runif(1) > 0.5) COOPERATE else DEVIATE
                                    
                                    # if enough actions performed, choose accordingly
                                  } else {
                                    
                                    # get the row from the q-table that corresponds to the actions from the
                                    # previous rounds
                                    currState <<- paste(tail(prevActions, ROUNDS_PER_STATE), collapse = "")
                                    qRow <- qTable[qTable$state == currState, ]
                                    
                                    # choose random action, if none is preferred
                                    if (qRow$c_prop == qRow$d_prop) {
                                      action <- if(runif(1) > 0.5) COOPERATE else DEVIATE
                                      
                                    # else use epsilon-greedy balancing between explore and exploit
                                    } else {
                                      if (runif(1) <= epsilon) {     # explore (epsilon %)
                                        if (qRow$c_prop < qRow$d_prop) {
                                          action <- COOPERATE
                                        } else {
                                          action <- DEVIATE
                                        }
                                      } else {                       # exploit (1-epsilon %)
                                        if (qRow$c_prop < qRow$d_prop) {
                                          action <- DEVIATE
                                        } else {
                                          action <- COOPERATE
                                        }
                                      }
                                    }
                                  }
                                  setOEU(action)
                                  return(action)
                                },
                                
                                #--------------------------------------------------------------------#
                                #   function: setOEU
                                #     Sets the overall expected utility based on the prospective 
                                #     action.
                                #--------------------------------------------------------------------#
                                setOEU = function(action) {
                                  if (action == COOPERATE) {
                                    optimalExpectedUtility <<- UTIL_MAX - coopCost
                                  } else if (action == DEVIATE) {
                                    optimalExpectedUtility <<- UTIL_MAX
                                  } else {
                                    stop(paste("Unknown action: ", action))
                                  }
                                },
                                
                                #--------------------------------------------------------------------#
                                #   function: getModelParameters
                                #     Returns the player's parametrical settings.
                                #--------------------------------------------------------------------#
                                getModelParameters = function() {
                                  return(c(callSuper(), 
                                           paste("p", ID, "_rounds_per_state", sep = ""), ROUNDS_PER_STATE, 
                                           paste("p", ID, "_prop_start", sep = ""), PROP_START, 
                                           paste("p", ID, "_epsilon_start", sep = ""), EPSILON_START,
                                           paste("p", ID, "_epsilon_decay", sep = ""), EPSILON_DECAY,
                                           paste("p", ID, "_alpha", sep = ""), ALPHA, 
                                           paste("p", ID, "_gamma", sep = ""), GAMMA))
                                },
                                
                                #--------------------------------------------------------------------#
                                #   function: getPersonalDetailColumns
                                #     Returns the player's columns for personal details.
                                #--------------------------------------------------------------------#
                                getPersonalDetailColumns = function() {
                                  columns <- c(paste("p", ID, "_epsilon", sep = ""),
                                               paste("p", ID, "_currstate", sep = ""),
                                               paste("p", ID, "_oeu", sep = ""),
                                               paste("p", ID, "_c_000", sep = ""),
                                               paste("p", ID, "_d_000", sep = ""),
                                               paste("p", ID, "_c_001", sep = ""),
                                               paste("p", ID, "_d_001", sep = ""),
                                               paste("p", ID, "_c_010", sep = ""),
                                               paste("p", ID, "_d_010", sep = ""),
                                               paste("p", ID, "_c_011", sep = ""),
                                               paste("p", ID, "_d_011", sep = ""),
                                               paste("p", ID, "_c_100", sep = ""),
                                               paste("p", ID, "_d_100", sep = ""),
                                               paste("p", ID, "_c_101", sep = ""),
                                               paste("p", ID, "_d_101", sep = ""),
                                               paste("p", ID, "_c_110", sep = ""),
                                               paste("p", ID, "_d_110", sep = ""),
                                               paste("p", ID, "_c_111", sep = ""),
                                               paste("p", ID, "_d_111", sep = ""))
                                  return(columns)
                                },
                                
                                #--------------------------------------------------------------------#
                                #   function: getCurrentPersonalDetails
                                #     Returns the player's current personal details.
                                #--------------------------------------------------------------------#
                                getCurrentPersonalDetails = function() {
                                  details <- c(round(epsilon, digits = 5),
                                               currState,
                                               optimalExpectedUtility,
                                               qTable[qTable$state == "000", ]$c_prop,
                                               qTable[qTable$state == "000", ]$d_prop,
                                               qTable[qTable$state == "001", ]$c_prop,
                                               qTable[qTable$state == "001", ]$d_prop,
                                               qTable[qTable$state == "010", ]$c_prop,
                                               qTable[qTable$state == "010", ]$d_prop,
                                               qTable[qTable$state == "011", ]$c_prop,
                                               qTable[qTable$state == "011", ]$d_prop,
                                               qTable[qTable$state == "100", ]$c_prop,
                                               qTable[qTable$state == "100", ]$d_prop,
                                               qTable[qTable$state == "101", ]$c_prop,
                                               qTable[qTable$state == "101", ]$d_prop,
                                               qTable[qTable$state == "110", ]$c_prop,
                                               qTable[qTable$state == "110", ]$d_prop,
                                               qTable[qTable$state == "111", ]$c_prop,
                                               qTable[qTable$state == "111", ]$d_prop)
                                  return(details)
                                }
                              )
)
