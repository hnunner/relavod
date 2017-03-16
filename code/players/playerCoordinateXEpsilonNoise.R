######################################## SOURCING MOTHER CLASS #######################################
if(!exists("CoordinateXPlayer", mode="function")) 
  source(paste(PLAYERS_DIR, "playerCoordinateX.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################

#####------------------------------- CoordinateXEpsilonNoisePlayer ------------------------------#####
# class: CoordinateXEpsilonNoisePlayer
#     Class extending the CoordinateX Player class. Instead of using a classical epsilon-greedy
#     approach to balance between exploitation and exploration, this player uses epsilon as a 
#     noise factor. This noise factor is used to in-/decrease the actual strategy propensities
#     to allow for some randomness in decision making. As in the CoordinateX base player, epsilon
#     decreases over time to achieve less variability of choices over time.
#
#     Free parameters, as in CoordinateX base player:
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
CoordinateXEpsilonNoisePlayer <- setRefClass("CoordinateXEpsilonNoisePlayer",
                                             
                                             #-------------------------------------------------------#
                                             #  class inheritance
                                             #-------------------------------------------------------#
                                             contains = "CoordinateXPlayer",
                                             
                                             #-------------------------------------------------------#
                                             #  class methods (public by defualt)
                                             #-------------------------------------------------------#
                                             methods = list(
                                               
                                               #-------------------------------------------------------#
                                               #  function: computeAction
                                               #    Selects the first action from the actions list. 
                                               #    If the action list is empty, it will be refilled 
                                               #    based on the propensities for the different 
                                               #    strategies.
                                               #-------------------------------------------------------#
                                               computeAction = function() {
                                                 
                                                 # if no more actions planned ahead, choose a strategy
                                                 if (length(actions) <= 0) {
                                                   
                                                   # balancing between exploration and exploitation
                                                   # by using epsilon as noise factor
                                                   noisyStrategies <- strategies
                                                   for (i in 1:length(strategies$prop)) {
                                                     noisyStrategies$prop[i] <- strategies$prop[i] + 
                                                       (strategies$prop[i] * runif(1, -epsilon, epsilon))
                                                   }
                                                   
                                                   currentStrategy <<- noisyStrategies[
                                                     with(noisyStrategies, order(-prop)),1][1]
                                                   
                                                   # set new list of actions and OEU
                                                   setActionsAndOEU()
                                                 }
                                                 
                                                 # extract current action
                                                 action <- actions[[1]]
                                                 actions <<- tail(actions, (length(actions) - 1))
                                                 return(action)
                                               }

                                             )
)
