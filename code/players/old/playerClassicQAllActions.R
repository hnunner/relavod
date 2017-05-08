######################################## SOURCING MOTHER CLASS #######################################
if(!exists("ClassicQPlayer", mode="function")) source(paste(PLAYERS_DIR, "playerClassicQ.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
PROP_START <<- 100        # initial propensity for each strategy
EPSILON_START <<- 0.1     # initial balance between exploration (epsilon) and exploration (1-epsilon)
EPSILON_DECAY <<- 0.995   # rate at which epsilon is decreasing after each completion of a strategy
ALPHA <<- 0.4             # RL learning rate, the higher the more important recently learned 
                          # information; 0 < ALPHA <= 1
GAMMA <<- 0.6             # RL discount factor, the higher the more important the previous rewards;
                          # 0 <= GAMMA <= 1


PLAYERS_PER_STATE <<- PLAYERS_CNT  # actions of how many players defining a state (here: all players)

#####------------------------------ ClassicQAllActionsPlayer ------------------------------#####
# class: ClassicQAllActionsPlayer
#     Class extending the basic Player class. This class represents a player using a classical 
#     Q-Learning approach, where action-state-pairs are being reinforced. A state is represented
#     by actions of all players in the previous rounds.
#
#     Free parameters:
#       - X
#       - PROP_START
#       - EPSILON_START
#       - EPSILON_DECAY
#       - ALPHA
#       - GAMMA
#       - PLAYERS_PER_STATE
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
ClassicQAllActionsPlayer <- setRefClass("ClassicQAllActionsPlayer",
                              
                              #----------------------------------------------------------------------# 
                              #  class inheritance
                              #----------------------------------------------------------------------#
                              contains = "ClassicQPlayer",
                              
                              #----------------------------------------------------------------------#
                              #  class methods (public by defualt)
                              #----------------------------------------------------------------------#
                              methods = list(
                                
                                #--------------------------------------------------------------------#
                                #   function: getPreviousActions
                                #     Gets all previous actions (here: for all players)
                                #--------------------------------------------------------------------#
                                getPreviousActions = function(action) {
                                  return(history[history$round >= 1, 2:4])
                                },
                                
                                #--------------------------------------------------------------------#
                                #   function: setCurrentState
                                #     Sets the state the player is currently in.
                                #--------------------------------------------------------------------#
                                setCurrentState = function(prevActions) {
                                  lastActions <- tail(prevActions, X)
                                  currState <<- paste(paste(lastActions[1,], collapse = ""), 
                                                      paste(lastActions[2,], collapse = ""),
                                                      paste(lastActions[3,], collapse = ""), sep = "")
                                }
                              )
)
