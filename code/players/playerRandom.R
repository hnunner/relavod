######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))

#####--------------------------------------- RandomPlayer ---------------------------------------#####
# class: RandomPlayer
#     Class extending the basic Player class. This class represents a player playing fully at random.
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
RandomPlayer <- setRefClass("RandomPlayer",
                            
                            #------------------------------------------------------------------------# 
                            #  class inheritance
                            #------------------------------------------------------------------------#
                            contains = "Player",
                            
                            #------------------------------------------------------------------------#
                            #  class methods (public by defualt)
                            #------------------------------------------------------------------------#
                            methods = list(
                              
                              
                              #----------------------------------------------------------------------#
                              #  function: computeAction
                              #    Selects the first action from the actions list. If the 
                              #    action list is empty, it will be refilled based on the
                              #    propensities for the different strategies.
                              #----------------------------------------------------------------------#
                              computeAction = function() {
                                action <- if(runif(1) > 0.5) COOPERATE else DEVIATE
                                return(action)
                              }
                            )
)
