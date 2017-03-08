######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
PROP_START <<- 100        # initial propensity for each strategy
X_MAX <<- 15              # triggers a warning during initialization, when X exceeds X_MAX
EPSILON_START <<- 0.1     # initial balance between exploration (epsilon) and exploration (1-epsilon)
EPSILON_DECAY <<- 0.995   # rate at which epsilon is decreasing over time
ALPHA <<- 0.1             # RL learning rate, the higher the more important recently learned 
# information; 0 < ALPHA <= 1
GAMMA <<- 0.1             # RL discount factor, the higher the more important the future rewards;
# 0 <= GAMMA <= 1

#####------------------------------------ CoordinateXPlayer -------------------------------------#####
# class: CoordinateXPlayer
#     Class extending the basic Player class. This class represents a player using a coordinate-x
#     strategy, with x representing at which position of an action sequence the player cooperates. 
#     E.g., x=4 means that the player will deviate three times and then coordinate. Reinforcement 
#     learning is used to evaluate taken actions and epsilon-decay is used to balance between
#     exploration and exploitation.
#
#     Free parameters:
#       - PROP_START
#       - EPSILON_START
#       - ALPHA
#       - GAMMA
#       - optimalExpectedUtility
#       - X
#
#     TODOs:
#       - epsilon-decay
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
CoordinateXPlayer <- setRefClass("CoordinateXPlayer",
                                 
                                 #-------------------------------------------------------------------# 
                                 #  class inheritance
                                 #-------------------------------------------------------------------#
                                 contains = "Player",
                                 
                                 #-------------------------------------------------------------------#
                                 #   class parameters (public by default)
                                 #      param:  strategies
                                 #          the player's strategies (see initialize())
                                 #      param:  currentStrategy
                                 #          the strategy currently used by the player
                                 #      param:  actions
                                 #          vector of action sequence for the upcoming rounds
                                 #      param:  epsilon
                                 #          balance between exploration and exploitation
                                 #      param:  optimalExpectedUtility
                                 #          the optimal utility estimate
                                 #-------------------------------------------------------------------#
                                 fields = c("X", "strategies", "currentStrategy", "actions", 
                                            "epsilon", "optimalExpectedUtility", "currentUtility"),
                                 
                                 #-------------------------------------------------------------------#
                                 #  class methods (public by defualt)
                                 #-------------------------------------------------------------------#
                                 methods = list(
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: initialize
                                   #     Initializes the Player.
                                   #     param:  ID
                                   #         the player's ID
                                   #     param:  coopCost
                                   #         the player's cost to cooperate
                                   #     param:  X
                                   #         the maximum position to cooperate in a sequence of 
                                   #         actions
                                   #-----------------------------------------------------------------#
                                   initialize = function(ID, coopCost, X) {
                                     
                                     X <<- X
                                     
                                     # initialization of strategies
                                     #    a single strategy is a tuple of:
                                     #      1. after how many deviations a player cooperates
                                     #      2. the player's propensity for the strategy
                                     coord <- c(0:X)
                                     prop <- rep(PROP_START, X+1)
                                     strategies <<- data.frame(coord, prop)
                                     currentStrategy <<- NA
                                     
                                     # initialization of actions
                                     actions <<- c()
                                     
                                     # initialization of epsilon
                                     epsilon <<- EPSILON_START
                                     
                                     # initialization of the optimal utility estimate
                                     optimalExpectedUtility <<- UTIL_NONE
                                     
                                     # initialization of the current utility
                                     currentUtility <<- 0
                                     
                                     # initializations of super class
                                     callSuper(ID, coopCost)
                                     
                                     if (LOG_LEVEL == "debug") {
                                       print(paste("Coordinate-X Player", ID, "whith X =", X,
                                                   "successfully created!"))
                                     }
                                   },
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: validate
                                   #     Integrity check for player: ID must be numeric.
                                   #-----------------------------------------------------------------#
                                   validate = function() {
                                     if (!is.numeric(X)) {
                                       stop("Error during player validation: 'X' must be numeric!")
                                     }
                                     if (X > X_MAX) {
                                       warning(paste("X =", X, "appears to be irrationally high!"))
                                     }
                                     callSuper()
                                   },
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: assessAction
                                   #     Assesses the player's action.
                                   #     param:  round
                                   #          the round
                                   #     param:  allPlayersActions
                                   #          actions played in the corresponding round by all players
                                   #     param:  util
                                   #          utility earned in the corresponding round, based on the 
                                   #          action taken
                                   #-----------------------------------------------------------------#
                                   assessAction = function(round, allPlayersActions, util) {
                                     
                                     # if strategy has not been fully performed yet, add up 
                                     # strategy's utility
                                     if (length(actions) > 0) {
                                       currentUtility <<- currentUtility + util
                                     } else {
                                       # else, calculate new propensity based on update function by 
                                       # Sutton & Barto (1998), p.148
                                       oldProp <- strategies[strategies$coord == currentStrategy,2]
                                       newProp <- oldProp + ALPHA * 
                                         (currentUtility + GAMMA * optimalExpectedUtility - oldProp)
                                       strategies[strategies$coord == currentStrategy,2] <<- newProp
                                       
                                       # decaying of epsilon - after a strategy has been fully
                                       # performed, not after each action, to ensure that each
                                       # player has an equal probability to explore the same
                                       # amount of strategies
                                       epsilon <- epsilon * EPSILON_DECAY
                                     }
                                     
                                     if (LOG_LEVEL == "debug") {
                                       if (ID == 1) {
                                         print(strategies)
                                       }
                                     }
                                     
                                     callSuper(round, allPlayersActions, util)
                                   },
                                   
                                   #-----------------------------------------------------------------#
                                   #  function: computeAction
                                   #    Selects the first action from the actions list. If the 
                                   #    action list is empty, it will be refilled based on the
                                   #    propensities for the different strategies.
                                   #-----------------------------------------------------------------#
                                   computeAction = function() {
                                     
                                     # if no more actions planned ahead, choose a strategy
                                     if (length(actions) <= 0) {
                                       
                                       # either explore other strategies, or exploit best strategy
                                       #    using epsilon-greedy approach, as suggested by
                                       #    Sutton & Barto (1998), p.148f.
                                       if (runif(1) <= epsilon) {     # explore (epsilon %)
                                         # sort by descending propensity and pick any but first 
                                         # coord value
                                         currentStrategy <<- strategies[
                                           with(strategies, order(-prop)),1][sample(1:X+1,1)]
                                       } else {                       # exploit (1-epsilon %)
                                         # sort by descending propensity and pick first coord value
                                         currentStrategy <<- strategies[
                                           with(strategies, order(-prop)),1][1]
                                       }
                                       
                                       # choose action sequence and corresponding optimal expected 
                                       # utility based on strategy
                                       optimalExpectedUtility <<- UTIL_NONE
                                       if (currentStrategy == 0) {
                                         actions <<- c(DEVIATE)
                                         optimalExpectedUtility <<- UTIL_MAX
                                       } else {
                                         actions <<- c()
                                         i <- 1
                                         while (i < currentStrategy) {
                                           actions <<- c(actions, DEVIATE)
                                           optimalExpectedUtility <<- 
                                             optimalExpectedUtility + UTIL_MAX
                                           i <- i+1
                                         }
                                         actions <<- c(actions, COOPERATE)
                                         optimalExpectedUtility <<- 
                                           optimalExpectedUtility + (UTIL_MAX - coopCost)
                                       }
                                     }
                                     
                                     # extract current action
                                     action <- actions[[1]]
                                     actions <<- tail(actions, (length(actions) - 1))
                                     return(action)
                                   },
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: getModelParameters
                                   #     Returns the player's parametrical settings.
                                   #-----------------------------------------------------------------#
                                   getModelParameters = function() {
                                     return(c(callSuper(), 
                                              paste("p", ID, "_X", sep = ""), X, 
                                              paste("p", ID, "_prop_start", sep = ""), PROP_START, 
                                              paste("p", ID, "_epsilon_start", sep = ""), EPSILON_START, 
                                              paste("p", ID, "_alpha", sep = ""), ALPHA, 
                                              paste("p", ID, "_gamma", sep = ""), GAMMA))
                                   },
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: getPersonalDetailColumns
                                   #     Returns the player's columns for personal details.
                                   #-----------------------------------------------------------------#
                                   getPersonalDetailColumns = function() {
                                     columns <- c(paste("p", ID, "_epsilon", sep = ""),
                                                  paste("p", ID, "_currstrat", sep = ""),
                                                  paste("p", ID, "_actions", sep = ""),
                                                  paste("p", ID, "_optutil", sep = ""),
                                                  paste("p", ID, "_currutil", sep = ""))
                                     for (i in 1:length(strategies$coord)) {
                                       columns <- c(columns, paste("p", ID, "_coord", 
                                                                   strategies[i,1], sep = ""))
                                     }
                                     return(columns)
                                   },
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: getCurrentPersonalDetails
                                   #     Returns the player's current personal details. In this case
                                   #     the player's peronal strategy settings.
                                   #     Formatting: c(prop_coord-0, prop_coord-1, ..., prop_coord-x)
                                   #-----------------------------------------------------------------#
                                   getCurrentPersonalDetails = function() {
                                     actionSeq <- ""
                                     for (action in actions) {
                                       actionSeq <- paste(actionSeq, action)
                                     }
                                     details <- c(epsilon, currentStrategy, actionSeq, 
                                                  round(optimalExpectedUtility, digits = 2),
                                                  round(currentUtility, digits = 2))
                                     for (i in 1:length(strategies$coord)) {
                                       details <- c(details, round(strategies[i,2], digits = 2))
                                     }
                                     return(details)
                                   }
                                 )
)
