PLAYERS_CNT <- 3
LOG_LEVEL <- "verbose"
COOP_COST <- 40
UTIL_MAX <- 100
UTIL_MIN <- UTIL_MAX - COOP_COST
UTIL_NONE <- 0
COOPERATE <- "c"
DEVIATE <- "d"


Game <- setRefClass("Game",
                    fields = c("players", "gameHistory", "roundsPlayed"),
                    methods = list(
                      initialize = function(players) {
                        players <<- players
                        gameHistory <<- data.frame("round" = numeric(1), "player1" = character(1), 
                                                   "player2" = character(1), "player3" = character(1),
                                                   "util1" = numeric(1), "util2" = numeric(1), 
                                                   "util3" = numeric(1), stringsAsFactors=FALSE)
                        roundsPlayed <<- 0
                        validate("Game successfully initialized!")
                      },
                      validate = function(msg = "Game successfully validated!") {
                        if (length(players) != PLAYERS_CNT) {
                          stop(paste("Invalid amount of players. ", 
                                     "Required: ", PLAYERS_CNT, "; ",
                                     "Received: ", length(players)))
                        } 
                        print(msg)
                      },
                      
                      ### 
                      computeRound = function() {
                        
                        moves <- c()
                        coop <- FALSE
                        for (i in 1:length(players)) {
                          # retrieval of actual object with [[]], instead of just reference pointer with [] 
                          # for further information: https://rforpublichealth.blogspot.nl/2015/03/basics-of-lists.html
                          move <- players[[i]]$computeMove()
                          if (move == COOPERATE) {
                            coop <- TRUE 
                          }
                          moves <- c(moves, move)
                        }
                        
                        utils <- c()
                        for (i in 1:length(players)) {
                          if (!coop) {
                            utils <- c(utils, UTIL_NONE)
                          } else {
                            if (moves[i] == COOPERATE) {
                              utils <- c(utils, UTIL_MIN)
                            } 
                            if (moves[i] == DEVIATE) {
                              utils <- c(utils, UTIL_MAX)
                            }
                          }
                        }
                        
                        roundsPlayed <<- roundsPlayed + 1
                        
                        for (i in 1:length(players)) {
                          move <- players[[i]]$updateHistory(roundsPlayed, moves, utils[i])
                        }
                        
                        gameHistory <<- rbind(gameHistory, c(roundsPlayed, moves, utils))
                        
                        if (LOG_LEVEL == "verbose") {
                          print(paste("Round", roundsPlayed, "successfully computed!"))
                        }
                      }
                    )
)


# "Reference Class" (RC) for player model - source: http://adv-r.had.co.nz/OO-essentials.html
Player <- setRefClass("Player",
                      fields = c("ID", "history"),
                      methods = list(
                        initialize = function(ID) {
                          ID <<- ID
                          history <<- data.frame("round" = numeric(1), "player1" = character(1), 
                                                 "player2" = character(1), "player3" = character(1),
                                                 "util" = numeric(1), stringsAsFactors=FALSE)
                          validate(paste("Player", ID, "successfully created!"))
                        },
                        validate = function(msg = "Player successfully validated!") {
                          if (!is.numeric(ID)) {
                            stop(paste("Error during player validation: ID must be numeric!"))
                          }
                          print(msg)
                        },
                        computeMove = function() {
                          if (runif(1, 0, 1) >= 0.66) {
                            return(COOPERATE)
                          } else {
                            return(DEVIATE)
                          }
                        },
                        updateHistory = function(round, moves, util) {
                          history <<- rbind(history, c(round, moves, util))
                        }
                      )
)

# Player model using reinforcement learning
ReinforcementPlayer <- setRefClass("ReinforcementPlayer",
                                   contains = "Player",
                                   methods = list(
                                     computeMove = function() {
                                       print(personalHistory)
                                       return("coordinate")
                                     }
                                   )
)

performSimulation <- function() {
  
  game <- Game$new(list(Player$new(1), Player$new(2), Player$new(3)))
  
  game$computeRound()
  game$gameHistory
  game$roundsPlayed
  game$players[[3]]$history
}
