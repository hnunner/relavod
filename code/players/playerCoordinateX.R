######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
PROP_START <<- 100        # initial propensity for each strategy
X_MAX <<- 15              # triggers a warning during initialization, when X exceeds X_MAX
EPSILON_START <<- 0.1     # initial balance between exploration (epsilon) and exploration (1-epsilon)
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
#       - optimalFutureProp
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
                                 #      param:  optimalFutureProp
                                 #          the optimal future propensity estimate
                                 #-------------------------------------------------------------------#
                                 fields = c("X", "strategies", "currentStrategy", "actions", 
                                            "epsilon", "optimalFutureProp"),
                                 
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
                                     
                                     # initialization of the optimal future propensity estimate
                                     optimalFutureProp <<- (2*UTIL_MAX + UTIL_MAX-coopCost) / 3
                                     
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
                                   #     param:  actions
                                   #          actions played in the corresponding round by all players
                                   #     param:  util
                                   #          utility earned in the corresponding round, based on the 
                                   #          action taken
                                   #-----------------------------------------------------------------#
                                   assessAction = function(round, actions, util) {
                                     
                                     # new propensity based on update function by 
                                     # Sutton & Barto (1998), p.148
                                     oldProp <- strategies[strategies$coord == currentStrategy,2]
                                     newProp <- oldProp + ALPHA * (util + GAMMA * 
                                                                     optimalFutureProp - oldProp)
                                     strategies[strategies$coord == currentStrategy,2] <<- newProp
                                     
                                     if (LOG_LEVEL == "debug") {
                                       if (ID == 1) {
                                         print(strategies)
                                       }
                                     }
                                     
                                     callSuper(round, actions, util)
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
