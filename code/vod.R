#####------------------------------------------- VOD --------------------------------------------#####
# class: Vod
#     Class representing the Volunteer's Dilemma game. It holds the basic game logic (function 
#     "computeRound"), the players, the game history and a counter for the rounds played.
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
Vod <- setRefClass("Vod",
                   
                   #---------------------------------------------------------------------------------# 
                   #  class parameters (public by default)
                   #    param:  players 
                   #        list of players involved in the VOD
                   #    param:  history
                   #        game history, consisting of round, player actions, player utilities
                   #    param:  roundsPlayed
                   #        counter for played rounds of the VOD
                   #---------------------------------------------------------------------------------# 
                   fields = c("players", "history", "roundsPlayed"),
                   
                   #---------------------------------------------------------------------------------# 
                   #  class methods (public by defualt)
                   #---------------------------------------------------------------------------------# 
                   methods = list(
                     
                     #-------------------------------------------------------------------------------# 
                     #  function: initialize
                     #    Initializes the VOD: players are set, history is initialized with an empty
                     #    data frame, round counter is initialized with 0 and VOD is validated
                     #    param:  players
                     #        the players involved in the VOD
                     #-------------------------------------------------------------------------------# 
                     initialize = function(players) {
                       players <<- players
                       history <<- data.frame("round" = numeric(1), "player1" = numeric(1), 
                                              "player2" = numeric(1), "player3" = numeric(1),
                                              "util1" = numeric(1), "util2" = numeric(1), 
                                              "util3" = numeric(1), stringsAsFactors=FALSE)
                       
                       # additional player details
                       for (i in 1:length(players)) {
                         additionaCols <- players[[i]]$getPersonalDetailColumns()
                         if (!all(is.na(additionaCols))) {
                           for (currCol in additionaCols) {
                             history[currCol] <<- numeric(1)
                           }
                         }
                       }
                       
                       roundsPlayed <<- 0
                       validate()
                       if (LOG_LEVEL == "all") {
                         print("VOD successfully created!")
                       }
                     },
                     
                     #-------------------------------------------------------------------------------# 
                     #  function: validate
                     #    Integrity check of the VOD: checking the right amount of players.
                     #-------------------------------------------------------------------------------# 
                     validate = function() {
                       if (length(players) != PLAYERS_CNT) {
                         stop(paste("Invalid amount of players. ", 
                                    "Required: ", PLAYERS_CNT, "; ",
                                    "Received: ", length(players)))
                       } 
                     },
                     
                     #-------------------------------------------------------------------------------# 
                     #  function: computeRound
                     #    Funciton holding the actual game logic of a single round of the VOD.
                     #-------------------------------------------------------------------------------# 
                     computeRound = function() {
                       
                       # 1. simulation of actions for each player
                       allPlayersActions <- c()
                       coop <- FALSE
                       for (i in 1:length(players)) {
                         # retrieval of list objects with [[]], instead of just reference pointer 
                         # with [] --- for further information, see: 
                         # https://rforpublichealth.blogspot.nl/2015/03/basics-of-lists.html
                         action <- players[[i]]$computeAction()
                         if (action == COOPERATE) {
                           coop <- TRUE 
                         }
                         allPlayersActions <- c(allPlayersActions, action)
                       }
                       
                       # 2. calculation of utilities, based on all player's actions
                       utils <- c()
                       for (i in 1:length(players)) {
                         if (!coop) {
                           utils <- c(utils, UTIL_NONE)
                         } else {
                           if (allPlayersActions[i] == COOPERATE) {
                             utils <- c(utils, UTIL_MAX - players[[i]]$coopCost)
                           } 
                           if (allPlayersActions[i] == DEVIATE) {
                             utils <- c(utils, UTIL_MAX)
                           }
                         }
                       }
                       
                       # round simulation completed
                       roundsPlayed <<- roundsPlayed + 1
                       
                       # assessing the player's action
                       for (i in 1:length(players)) {
                         action <- players[[i]]$assessAction(roundsPlayed, allPlayersActions, utils[i])
                       }
                       
                       # updating the VOD's game history
                       playersDetails <- c()
                       for (i in 1:length(players)) {
                         playerDetails <- players[[i]]$getCurrentPersonalDetails()
                         if (!all(is.na(playerDetails))) {
                           playersDetails <- c(playersDetails, playerDetails)
                         }
                       }
                       history <<- rbind(history, c(roundsPlayed, allPlayersActions, utils, playersDetails))
                       
                       if (LOG_LEVEL == "all") {
                         print(paste("Round", roundsPlayed, "successfully computed!"))
                       }
                     }
                   )
)
