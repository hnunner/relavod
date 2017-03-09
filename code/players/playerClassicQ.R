######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
ROUNDS_PER_STATE <<- 3    # the amount of rounds representing a state; e.g., 3 means that a player
                          # looks at all actions taken by all players in the previous 3 rounds and
                          # takes the action that produced the highest utility
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
                              fields = c("epsilon"),
                              
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
                                  
                                  # initialization of epsilon
                                  epsilon <<- EPSILON_START
                                  
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
                                  
                                  # # if all actions have been performed, calculate new propensity 
                                  # # based on update function by Sutton & Barto (1998), p.148
                                  # oldProp <- strategies[strategies$coord == currentStrategy,2]
                                  # newProp <- oldProp + ALPHA * 
                                  #   (util + GAMMA * optimalExpectedUtility - oldProp)
                                  # strategies[strategies$coord == currentStrategy,2] <<- newProp
                                  
                                  # Decay of epsilon: after a strategy has been fully
                                  # performed. Not after each action, to ensure that each
                                  # player has an equal probability to explore the same
                                  # amount of strategies.
                                  epsilon <<- epsilon * EPSILON_DECAY
                                  
                                  callSuper(round, allPlayersActions, util)
                                },
                                
                                #--------------------------------------------------------------------#
                                #  function: computeAction
                                #    Selects the first action from the actions list. If the 
                                #    action list is empty, it will be refilled based on the
                                #    propensities for the different strategies.
                                #--------------------------------------------------------------------#
                                computeAction = function() {
                                  action <- COOPERATE
                                  return(action)
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
                                  columns <- c(paste("p", ID, "_epsilon", sep = ""))
                                  return(columns)
                                },
                                
                                #--------------------------------------------------------------------#
                                #   function: getCurrentPersonalDetails
                                #     Returns the player's current personal details.
                                #--------------------------------------------------------------------#
                                getCurrentPersonalDetails = function() {
                                  details <- c(round(epsilon, digits = 5))
                                  return(details)
                                }
                              )
)
