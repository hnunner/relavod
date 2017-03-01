#####------------------------------------------ Player ------------------------------------------#####
# class: Player
#     Root class representing the basic player. This class holds all the parameters and functions 
#     shared by all implementations of different players. The main goal when extending this class
#     is to override the "computeAction" function, which holds the actual behavioral strategies by 
#     deciding which action to take.
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
Player <- setRefClass("Player",
                      
                      #------------------------------------------------------------------------------# 
                      #   class parameters (public by default)
                      #     param:  ID 
                      #         the player's identifier
                      #     param:  coopCost
                      #         the player's cost to cooperate
                      #     param:  history
                      #         the player's game history, consisting of round, all player actions,
                      #         player's own utility
                      #------------------------------------------------------------------------------#
                      fields = c("ID", "coopCost", "history"),
                      
                      #------------------------------------------------------------------------------# 
                      #   class methods (public by defualt)
                      #------------------------------------------------------------------------------#
                      methods = list(
                        
                        #----------------------------------------------------------------------------# 
                        #   function: initialize
                        #     Initializes the Player: ID is set, history is initialized with an empty
                        #     data frame and player is validated
                        #     param:  ID
                        #         the player's ID
                        #     param:  coopCost
                        #         the player's cost to cooperate
                        #----------------------------------------------------------------------------#
                        initialize = function(ID, coopCost) {
                          ID <<- ID
                          coopCost <<- coopCost
                          history <<- data.frame("round" = numeric(1), "player1" = character(1), 
                                                 "player2" = character(1), "player3" = character(1),
                                                 "util" = numeric(1), stringsAsFactors=FALSE)
                          validate()
                          if (LOG_LEVEL == "debug") {
                            print(paste("Player", ID, "successfully created!"))
                          }
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: validate
                        #     Integrity check for player: ID must be numeric.
                        #----------------------------------------------------------------------------#
                        validate = function() {
                          if (!is.numeric(ID)) {
                            stop("Error during player validation: 'ID' must be numeric!")
                          }
                          if (!is.numeric(coopCost)) {
                            stop("Error during player validation: 'coopCost' must be numeric!")
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
                          history <<- rbind(history, c(round, actions, util))
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: computeAction
                        #     Computes which action to take. This implementation is just a 
                        #     placeholder for more elaborate implementations. This method must be 
                        #     overwritten by inheriting classes.
                        #----------------------------------------------------------------------------#
                        computeAction = function() {
                          return(COOPERATE)
                        }
                      )
)