if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))

#####------------------------------------- SymmetricPlayer --------------------------------------#####
# class: AsymmetricPlayer
#     Class extending the basic Player class. This class represents a player using the one-shot
#     mixed strategy equilibrium for symmetric VODs, as described in Diekmann $ Przepiorka (2016),
#     p. 1316
#----------------------------------------------------------------------------------------------------#
SymmetricPlayer <- setRefClass("SymmetricPlayer",
                               
                               #---------------------------------------------------------------------# 
                               #  class inheritance
                               #---------------------------------------------------------------------#
                               contains = "Player",
                               
                               #---------------------------------------------------------------------# 
                               #  class methods (public by defualt)
                               #---------------------------------------------------------------------#
                               methods = list(
                                 
                                 #-------------------------------------------------------------------# 
                                 #   function: computeAction
                                 #      Computes which action to take. The implementation corresponds 
                                 #      to the mixed-strategy equilibrium for symmetric games, as 
                                 #      described in Diekmann & Przepiorka (2016), p.1316.
                                 #-------------------------------------------------------------------#
                                 computeAction = function() {
                                   coopProb <- 1 - sqrt(coopCost/UTIL_MAX)
                                   if (runif(1, 0, 1) < coopProb) {
                                     return(COOPERATE)
                                   } else {
                                     return(DEVIATE)
                                   }
                                 }
                               )
)
