######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
Q_START <<- 0.2

#####------------------------------------ Coordinate4Player -------------------------------------#####
# class: Coordinate4Player
#     Class extending the basic Player class. This class represents a player using a coordinate-x
#     strategy, with x=4. Reinforcement learning is used to evaluate taken actions and thus as basis
#     for decision making.
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
Coordinate4Player <- setRefClass("Coordinate4Player",
                                 
                                 #-------------------------------------------------------------------# 
                                 #  class inheritance
                                 #-------------------------------------------------------------------#
                                 contains = "Player",
                                 
                                 #-------------------------------------------------------------------#
                                 #   class parameters (public by default)
                                 #     param:  othersCoopCosts
                                 #         the other player's costs to cooperate
                                 #-------------------------------------------------------------------#
                                 fields = c("strategies", "currentStrategy", "actions"),
                                 
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
                                   initialize = function(ID, coopCost) {
                                     
                                     # initialization of strategies
                                     strategies <<- list(list("cooperate-1", Q_START, 
                                                              COOPERATE), 
                                                         list("cooperate-2", Q_START, 
                                                              DEVIATE, COOPERATE),
                                                         list("cooperate-3", Q_START, 
                                                              DEVIATE, DEVIATE, COOPERATE),
                                                         list("cooperate-4", Q_START, 
                                                              DEVIATE, DEVIATE, DEVIATE, COOPERATE),
                                                         list("cooperate-5", Q_START, 
                                                              DEVIATE))
                                     currentStrategy <<- NA
                                     
                                     # initialization of actions
                                     actions <<- c()
                                     
                                     callSuper(ID, coopCost)
                                     if (LOG_LEVEL == "debug") {
                                       print(paste("Coordinate-4 Player", ID, 
                                                   "successfully created!"))
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
                                     
                                     # random number between 0 and sum of all propensities
                                     randMax <- 0
                                     for (i in 1:length(strategies)) {
                                       randMax <- randMax + strategies[[i]][[2]]
                                     }
                                     
                                     # selection of strategy / actions for the upcoming round(s)
                                     if (length(actions) == 0) {
                                       rand <- runif(1, 0, randMax)
                                       addedPropensity <- 0
                                       for (i in 1:length(strategies)) {
                                         addedPropensity <- addedPropensity + strategies[[i]][[2]]
                                         if (rand < addedPropensity) {
                                           currentStrategy <<- strategies[[i]]
                                           actions <<- tail(currentStrategy, 
                                                            (length(currentStrategy) - 2))
                                           if (LOG_LEVEL == "debug") {
                                             print(paste("rand:", rand, 
                                                         " - propensity:", addedPropensity,
                                                         " - strategy:", currentStrategy[1]))
                                           }
                                           break()
                                         }
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
  source('~/git/uu/mscp-model/code/players/playerCoordinate4.R')
  
  player <- Coordinate4Player$new(1, 2)
  
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
