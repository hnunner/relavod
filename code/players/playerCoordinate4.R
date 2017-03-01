if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))

#####------------------------------------ Coordinate4Player -------------------------------------#####
# class: Coordinate4Player
#     Class extending the basic Player class. This class represents a player using a coordinate-x
#     strategy, with x=4. Reinforcement learning is used to evaluate taken actions and thus as basis
#     for decision making.
#----------------------------------------------------------------------------------------------------#
Coordinate4Player <- setRefClass("Coordinate4Player",
                                   
                                   #-----------------------------------------------------------------# 
                                   #  class inheritance
                                   #-----------------------------------------------------------------#
                                   contains = "Player",
                                   
                                   #-----------------------------------------------------------------# 
                                   #  class methods (public by defualt)
                                   #-----------------------------------------------------------------#
                                   methods = list(
                                     
                                     #---------------------------------------------------------------# 
                                     #   function: initialize
                                     #     Initializes the Player.
                                     #     param:  ID
                                     #         the player's ID
                                     #     param:  coopCost
                                     #         the player's cost to cooperate
                                     #---------------------------------------------------------------#
                                     initialize = function(ID, coopCost) {
                                       callSuper(ID, coopCost)
                                       if (LOG_LEVEL == "debug") {
                                         print(paste("Coordinate-4 Player", ID, 
                                                     "successfully created!"))
                                       }
                                     },
                                     
                                     #---------------------------------------------------------------# 
                                     #  function: computeAction
                                     #    TODO
                                     #---------------------------------------------------------------#
                                     computeAction = function() {
                                       return(COOPERATE)
                                     }
                                   )
)