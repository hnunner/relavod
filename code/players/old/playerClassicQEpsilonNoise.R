######################################## SOURCING MOTHER CLASS #######################################
if(!exists("ClassicQPlayer", mode="function")) source(paste(PLAYERS_DIR, "playerClassicQ.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
PROP_START <<- 100        # initial propensity for each strategy
EPSILON_START <<- 0.1     # initial balance between exploration (epsilon) and exploration (1-epsilon)
EPSILON_DECAY <<- 1       # rate at which epsilon is decreasing after each completion of a strategy
ALPHA <<- 0.4             # RL learning rate, the higher the more important recently learned 
                          # information; 0 < ALPHA <= 1
GAMMA <<- 0.6             # RL discount factor, the higher the more important the previous rewards;
                          # 0 <= GAMMA <= 1

#####-------------------------------- ClassicQEpsilonNoisePlayer --------------------------------#####
# class: ClassicQEpsilonNoisePlayer
#     Class extending the classic Q-Learning player class. This class represents a player using 
#     a classical Q-Learning approach, where action-state-pairs are being reinforced. A state is r
#     epresented by actions of all players in the previous rounds. Instead of using epsilon-greedy
#     for balancing exploitation and exploration, the Q-Table is prior to action selection adjusted 
#     every round by using epsilon as noise factor.
#
#     Free parameters:
#       - X
#       - PROP_START
#       - EPSILON_START
#       - EPSILON_DECAY
#       - ALPHA
#       - GAMMA
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
ClassicQEpsilonNoisePlayer <- setRefClass("ClassicQEpsilonNoisePlayer",
                              
                              #----------------------------------------------------------------------# 
                              #  class inheritance
                              #----------------------------------------------------------------------#
                              contains = "ClassicQPlayer",
                              
                              #----------------------------------------------------------------------#
                              #  class methods (public by defualt)
                              #----------------------------------------------------------------------#
                              methods = list(
                                
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
                                  if (length(prevActions) < X) {
                                    action <- if(runif(1) > 0.5) COOPERATE else DEVIATE
                                    
                                    # if enough actions performed, choose accordingly
                                  } else {
                                    
                                    # get the row from the q-table that corresponds to the actions from the
                                    # previous rounds
                                    currState <<- paste(tail(prevActions, X), collapse = "")
                                    qRow <- qTable[qTable$state == currState, ]
                                    
                                    # add noise to state's propensities
                                    qRow$c_prop <- qRow$c_prop + (qRow$c_prop * runif(1, -epsilon, epsilon))
                                    qRow$d_prop <- qRow$d_prop + (qRow$d_prop * runif(1, -epsilon, epsilon))
                                    # select action 
                                    if (qRow$c_prop > qRow$d_prop) {
                                      action <- COOPERATE
                                    } else if (qRow$d_prop > qRow$c_prop) {
                                      action <- DEVIATE
                                    }
                                  }
                                  setOEU(action)
                                  return(action)
                                }
                              )
)
