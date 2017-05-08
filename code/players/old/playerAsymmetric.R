######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))

#####------------------------------------- AsymmetricPlayer -------------------------------------#####
# class: AsymmetricPlayer
#     Class extending the basic Player class. This class represents a player using the one-shot
#     mixed strategy equilibrium for asymmetric VODs, as described in Diekmann $ Przepiorka (2016),
#     p. 1317
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
AsymmetricPlayer <- setRefClass("AsymmetricPlayer",
                                
                                #--------------------------------------------------------------------# 
                                #  class inheritance
                                #--------------------------------------------------------------------#
                                contains = "Player",
                                
                                #--------------------------------------------------------------------# 
                                #   class parameters (public by default)
                                #     param:  othersCoopCosts
                                #         the other player's costs to cooperate
                                #--------------------------------------------------------------------#
                                fields = c("othersCoopCosts"),
                                
                                #--------------------------------------------------------------------# 
                                #   class methods (public by defualt)
                                #--------------------------------------------------------------------#
                                methods = list(
                                  
                                  #------------------------------------------------------------------# 
                                  #   function: initialize
                                  #     Initializes the Player: ID is set, history is initialized with 
                                  #     an empty data frame and player is validated
                                  #     param:  ID
                                  #         the player's ID
                                  #     param:  coopCost
                                  #         the player's cost to cooperate
                                  #------------------------------------------------------------------#
                                  initialize = function(ID, coopCost, othersCoopCosts) {
                                    othersCoopCosts <<- othersCoopCosts
                                    callSuper(ID, coopCost)
                                    if (LOG_LEVEL == "all") {
                                      print(paste("Asymmetric Player", ID, "successfully created!"))
                                    }
                                  },
                                  
                                  #------------------------------------------------------------------# 
                                  #   function: validate
                                  #     Integrity check for player: othersCoopCosts must be a vector 
                                  #     with two elements.
                                  #------------------------------------------------------------------#
                                  validate = function() {
                                    if (!length(othersCoopCosts == 2)) {
                                      stop(paste("Error during player validation: 'othersCoopCosts' 
                                                 must be a vector with two elements, but is:", 
                                                 othersCoopCosts))
                                    }
                                    callSuper()
                                  },
                                  
                                  #------------------------------------------------------------------# 
                                  #   function: computeAction
                                  #     Computes which action to take. The current implementation 
                                  #     corresponds to the mixed-strategy equilibrium for asymmetric
                                  #     games, as described in Diekmann & Przepiorka (2016), p.1317.
                                  #------------------------------------------------------------------#
                                  computeAction = function() {
                                    
                                    # othersCoopCosts <- c(30, 50)
                                    # coopCost <- 50
                                    # UTIL_MAX <- 80
                                    
                                    othersCostUtilRatio <- 1
                                    for (i in 1:length(othersCoopCosts)) {
                                      othersCostUtilRatio <- othersCostUtilRatio *
                                        (othersCoopCosts[i] / UTIL_MAX)
                                    }
                                    othersCostUtilRatio <- othersCostUtilRatio^(1/length(othersCoopCosts))
                                    coopProb <- 1 - (UTIL_MAX/coopCost) * othersCostUtilRatio
                                    if (runif(1, 0, 1) < coopProb) {
                                      return(COOPERATE)
                                    } else {
                                      return(DEVIATE)
                                    }
                                  },
                                  
                                  #----------------------------------------------------------------------------# 
                                  #   function: getModelParameters
                                  #     Returns the player's parametrical settings.
                                  #----------------------------------------------------------------------------#
                                  getModelParameters = function() {
                                    othersCoopCostsSeq <- ""
                                    for (othersCoopCost in othersCoopCosts) {
                                      othersCoopCostsSeq <- paste(othersCoopCostsSeq, othersCoopCost)
                                    }
                                    return(c(callSuper(), 
                                             paste("p", ID, "_others_coop_cost", sep = ""), othersCoopCostsSeq))
                                  }
                                )
)