######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
PROP_START <<- 0.2
X_MAX <<- 15
EPSILON_START <<- 0.1

#####------------------------------------ CoordinateXPlayer -------------------------------------#####
# class: CoordinateXPlayer
#     Class extending the basic Player class. This class represents a player using a coordinate-x
#     strategy, with x representing at which position of an action sequence the player cooperates. 
#     E.g., x=4 means that the player will deviate three times and then coordinate. Reinforcement 
#     learning is used to evaluate taken actions and diminishing epsilon-greedy is used to balance 
#     between exploration and exploitation.
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
                                 #-------------------------------------------------------------------#
                                 fields = c("strategies", "currentStrategy", "actions", "epsilon"),
                                 
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
                                   #-----------------------------------------------------------------#
                                   initialize = function(ID, coopCost, X) {
                                     
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
                                     
                                     # initializations of super class
                                     callSuper(ID, coopCost)
                                     
                                     if (LOG_LEVEL == "debug") {
                                       print(paste("Coordinate-X Player", ID, "whith X =", X,
                                                   "successfully created!"))
                                     }
                                   },
                                   
                                   #----------------------------------------------------------------------------# 
                                   #   function: validate
                                   #     Integrity check for player: ID must be numeric.
                                   #----------------------------------------------------------------------------#
                                   validate = function() {
                                     if (!is.numeric(X)) {
                                       stop("Error during player validation: 'X' must be numeric!")
                                     }
                                     if (X > X_MAX) {
                                       warning(paste("X =", X, "appears to be irrationally high!"))
                                     }
                                     callSuper()
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
                                     
                                     # update the propensity for current strategy
                                     # TODO - implement propper reinforcement learning algorithm
                                     # to update propensities
                                     currentStrategy[[2]] <<- currentStrategy[[2]] + 0.1
                                     
                                     
                                     
                                     
                                     
                                     for (i in 1:length(strategies)) {
                                       if (strategies[[i]][[1]] == currentStrategy[[1]]) {
                                         strategies[[i]][[2]] <<- currentStrategy[[2]]
                                       }
                                     }
                                     callSuper(round, actions, util)
                                   },
                                   
                                   #---------------------------------------------------------------# 
                                   #  function: computeAction
                                   #    Selects the first action from the actions list. If the 
                                   #    action list is empty, it will be refilled based on the
                                   #    propensities for the different strategies.
                                   #---------------------------------------------------------------#
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
                                       
                                       # choose action sequence based on strategy
                                       if (currentStrategy == 0) {
                                         actions <<- c(DEVIATE)
                                       } else {
                                         actions <<- c()
                                         i <- 1
                                         while (i < currentStrategy) {
                                           actions <<- c(actions, DEVIATE)
                                           i <- i+1
                                         }
                                         actions <<- c(actions, COOPERATE)
                                       }
                                     }

                                     # extract current action
                                     action <- actions[[1]]
                                     actions <<- tail(actions, (length(actions) - 1))
                                     return(action)
                                   }
                                 )
)


testFunction <- function() {
  source('~/git/uu/mscp-model/code/simulation.R')
  source('~/git/uu/mscp-model/code/constants.R')
  source('~/git/uu/mscp-model/code/players/playerCoordinateX.R')
  
  player <- CoordinateXPlayer$new(1, 2)
  
  for (i in 1:50) {
    dummyAction <- player$computeAction()
    player$assessAction(1,2,3)
  }
  
  player$strategies[[1]][[1]]
  player$strategies[[1]][[2]]
  player$strategies[[2]][[1]]
  player$strategies[[2]][[2]]
  player$strategies[[3]][[1]]
  player$strategies[[3]][[2]]
  player$strategies[[4]][[1]]
  player$strategies[[4]][[2]]
  player$strategies[[5]][[1]]
  player$strategies[[5]][[2]]
}
