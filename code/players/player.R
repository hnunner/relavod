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
                      #     param:  coopCosts
                      #         all players' cost to cooperate
                      #     param:  ownCoopCosts
                      #         player's own cost to cooperate
                      #     param:  lowestCoopCosts
                      #         lowest cost to cooperate
                      #     param:  history
                      #         the player's game history, consisting of round, all player actions,
                      #         player's own utility
                      #------------------------------------------------------------------------------#
                      fields = c("ID", "coopCosts", "ownCoopCosts", "lowestCoopCosts", "history"),
                      
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
                        #     param:  coopCosts
                        #         all players' cost to cooperate
                        #----------------------------------------------------------------------------#
                        initialize = function(ID, coopCosts) {
                          ID <<- ID
                          coopCosts <<- coopCosts
                          ownCoopCosts <<- coopCosts[ID]
                         
                          lowestCoopCosts <<- COOP_COST_SYMM
                          for (coopCost in coopCosts) {
                            if (coopCost < lowestCoopCosts) {
                              lowestCoopCosts <<- coopCost
                            }
                          }
                          
                          history <<- data.frame("round" = numeric(1), 
                                                 "player1" = character(1), 
                                                 "player2" = character(1), 
                                                 "player3" = character(1),
                                                 "util_player1" = numeric(1), 
                                                 "util_player2" = numeric(1), 
                                                 "util_player3" = numeric(1), 
                                                 stringsAsFactors=FALSE)
                          validate()
                          if (LOG_LEVEL == "all") {
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
                          for (coopCost in coopCosts) {
                            if (!is.numeric(coopCost)) {
                              stop("Error during player validation: 'coopCost' must be numeric!")
                            }
                          }
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: assessAction
                        #     Assesses the player's action.
                        #     param:  round
                        #         the round
                        #     param:  allPlayersActions
                        #         actions played in the corresponding round by all players
                        #     param:  allPlayersUtils
                        #         utilities earned in the corresponding round for all players, 
                        #         based on the action taken
                        #----------------------------------------------------------------------------#
                        assessAction = function(round, allPlayersActions, allPlayersUtils) {
                          history <<- rbind(history, c(round, allPlayersActions, allPlayersUtils))
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: computeAction
                        #     Computes which action to take. This implementation is just a 
                        #     placeholder for more elaborate implementations. This method must be 
                        #     overwritten by inheriting classes.
                        #----------------------------------------------------------------------------#
                        computeAction = function() {
                          return(COOPERATE)
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: getModelParameters
                        #     Returns the player's parametrical settings.
                        #----------------------------------------------------------------------------#
                        getModelParameters = function() {
                          return(c(paste("p1_coop_cost", sep = ""), coopCosts[1],
                                   paste("p2_coop_cost", sep = ""), coopCosts[2],
                                   paste("p3_coop_cost", sep = ""), coopCosts[3]))
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: getPersonalDetailColumns
                        #     Returns the player's columns for personal details, such as 
                        #     propensities. This can be used to extend the VOD's game history data 
                        #     frame.
                        #----------------------------------------------------------------------------#
                        getPersonalDetailColumns = function() {
                          return(NA)
                        },
                        
                        #----------------------------------------------------------------------------# 
                        #   function: getCurrentPersonalDetails
                        #     Returns the player's current personal details, such as propensities.
                        #     This can be used to extend the VOD's game history for each round.
                        #----------------------------------------------------------------------------#
                        getCurrentPersonalDetails = function() {
                          return(NA)
                        }
                        
                      )
)