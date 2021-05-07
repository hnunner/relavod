# Copyright (C) 2017 - 2021
#      Hendrik Nunner    <h.nunner@gmail.com>
#
# This file is part of the ReLAVOD project <https://github.com/hnunner/relavod>.
#
# This project is a stand-alone R program of reinforcement learning agents interacting in the
# repeated Volunteer's Dilemma (VOD). The purpose of ReLAVOD is to use reinforcement learning
# to investigate the role of cognitive mechanisms in the emergence of conventions.
#
# This program is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with this program.
# If not, see <http://www.gnu.org/licenses/>.

######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
X_MAX <<- 15              # triggers a warning during initialization, when X exceeds X_MAX
# PROP_START <<- 50
# EPSILON_START <<- 0.1
# EPSILON_DECAY <<- 0.995
# ALPHA <<- 0.4
# GAMMA <<- 0.6

#####-------------------------------------- ClassicQPlayer --------------------------------------#####
# class: ClassicQPlayer
#     Class extending the basic Player class. This class represents a player using a classical
#     Q-Learning approach, where action-state-pairs are being reinforced. A state is represented
#     by actions of a single player in the previous rounds.
#
#     Free parameters:
#
#       X                     how many past actions define a state
#       PLAYERS_PER_STATE     actions of how many players define a state
#       BALANCING             either greedy or noise
#
#       PROP_START            initial propensity for each strategy
#       EPSILON_START         initial balance between exploration (epsilon) and exploration (1-epsilon)
#       EPSILON_DECAY         rate at which epsilon is decreasing after each completion of a strategy
#       ALPHA                 RL learning rate, the higher the more important recently learned
#                             information; 0 < ALPHA <= 1
#       GAMMA                 RL discount factor, the higher the more important the previous rewards;
#                             0 <= GAMMA <= 1
#
#       SOCIAL_BEHAVIOR       either selfish or altruistic
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
ClassicQPlayer <- setRefClass("ClassicQPlayer",

                              #----------------------------------------------------------------------#
                              #  class inheritance
                              #----------------------------------------------------------------------#
                              contains = "Player",

                              #----------------------------------------------------------------------#
                              #   class parameters (public by default)
                              #      param:  X
                              #           the amount of rounds representing a state; e.g., 3 means
                              #           that a player looks at all actions in the previous 3 rounds
                              #      param:  PLAYERS_PER_STATE
                              #           action of how many players define a state; e.g., 1 means
                              #           that only the actions of the current player in the previous
                              #           X rounds define a state, PLAYERS_CNT means that the actions
                              #           of all players in the previous X rounds define a state
                              #      param:  BALANCING
                              #           how to balance between exploration and exploitation (see
                              #           BALANCING_TYPE in constants.R)
                              #      param:  PROP_START
                              #           initial propensity for each strategy
                              #      param:  EPSILON_START
                              #           initial balance between exploration (epsilon) and
                              #           exploration (1-epsilon)
                              #      param:  EPSILON_DECAY
                              #           rate at which epsilon is decreasing after each completion
                              #           of a strategy
                              #      param:  ALPHA
                              #           RL learning rate, the higher the more important recently
                              #           learned information; 0 < ALPHA <= 1
                              #      param:  GAMMA
                              #           RL discount factor, the higher the more important the
                              #           previous rewards; 0 <= GAMMA <= 1
                              #      param:  SOCIAL_BEHAVIOR
                              #           either selfish (max. own rewards) or
                              #           altruistic (max. mutual rewards)
                              #      param:  qTable
                              #           q-table holding the propensities for cooperation and
                              #           deviation in the according states
                              #      param:  epsilon
                              #           balance between exploration and exploitation
                              #      param:  currState
                              #           current state the player is in; state is defined as a
                              #           combination if X and PLAYERS_PER_STATE
                              #      param:  oeu
                              #           optimal expected utility
                              #----------------------------------------------------------------------#
                              fields = c("X", "PLAYERS_PER_STATE", "BALANCING", "PROP_START",
                                         "EPSILON_START", "EPSILON_DECAY", "ALPHA", "GAMMA",
                                         "SOCIAL_BEHAVIOR", "qTable", "epsilon", "currState", "oeu"),

                              #----------------------------------------------------------------------#
                              #  class methods (public by defualt)
                              #----------------------------------------------------------------------#
                              methods = list(

                                #--------------------------------------------------------------------#
                                #   function: initialize
                                #     Initializes the Player.
                                #     param:  ID
                                #           the player's ID
                                #     param:  coopCosts
                                #           all players' cost to cooperate
                                #
                                #     param:  X
                                #           see "class parameters"
                                #     param:  PLAYERS_PER_STATE
                                #           see "class parameters"
                                #     param:  BALANCING
                                #           see "class parameters"
                                #     param:  PROP_START
                                #           see "class parameters"
                                #     param:  EPSILON_START
                                #           see "class parameters"
                                #     param:  EPSILON_DECAY
                                #           see "class parameters"
                                #     param:  ALPHA
                                #           see "class parameters"
                                #     param:  GAMMA
                                #           see "class parameters"
                                #--------------------------------------------------------------------#
                                initialize = function(ID, coopCosts,
                                                      X, PLAYERS_PER_STATE, BALANCING, PROP_START,
                                                      EPSILON_START, EPSILON_DECAY, ALPHA, GAMMA,
                                                      SOCIAL_BEHAVIOR) {

                                  X <<- X
                                  BALANCING <<- BALANCING
                                  PLAYERS_PER_STATE <<- PLAYERS_PER_STATE
                                  PROP_START <<- PROP_START
                                  EPSILON_START <<- EPSILON_START
                                  EPSILON_DECAY <<- EPSILON_DECAY
                                  ALPHA <<- ALPHA
                                  GAMMA <<- GAMMA
                                  SOCIAL_BEHAVIOR <<- SOCIAL_BEHAVIOR

                                  # initialization of the q-table
                                  initQTable()

                                  # initialization of current state
                                  currState <<- NA

                                  # initialization of epsilon
                                  epsilon <<- EPSILON_START

                                  # initialization of the optimal utility estimate
                                  oeu <<- UTIL_NONE

                                  # initializations of super class
                                  callSuper(ID, coopCosts)

                                  if (LOG_LEVEL == "all") {
                                    print(paste("Classic-Q Player", ID, "successfully created!"))
                                  }
                                },


                                #--------------------------------------------------------------------#
                                #   function: initQTable
                                #     Q-Table initialization.
                                #--------------------------------------------------------------------#
                                initQTable = function() {
                                  library(gtools)
                                  states <- permutations(2, X*PLAYERS_PER_STATE, c(0, 1),
                                                         set = FALSE, repeats.allowed = TRUE)
                                  state <- c()
                                  for (i in 1:nrow(states)) {
                                    state <- c(state, paste(states[i,], collapse = ""))
                                  }

                                  c_prop <- c(rep(PROP_START, length(state)))
                                  d_prop <- c(rep(PROP_START, length(state)))
                                  qTable <<- data.frame(state, c_prop, d_prop)
                                },


                                #--------------------------------------------------------------------#
                                #   function: validate
                                #     Integrity check for player: X must be numeric.
                                #--------------------------------------------------------------------#
                                validate = function() {
                                  if (!is.numeric(X)) {
                                    stop("Error during player validation: 'X' must be numeric!")
                                  }
                                  if (X > X_MAX) {
                                    warning(paste("X =", X, "appears to be irrationally high!"))
                                  }
                                  if (!(PLAYERS_PER_STATE == 1
                                        || PLAYERS_PER_STATE == PLAYERS_CNT)) {
                                    stop(paste("invalid argument for PLAYERS_PER_STATE:",
                                               PLAYERS_PER_STATE))
                                  }
                                  callSuper()
                                },


                                #--------------------------------------------------------------------#
                                #   function: assessAction
                                #     Assesses the player's action.
                                #     param:  round
                                #          the round
                                #     param:  allPlayersActions
                                #          actions played in the corresponding round by all players
                                #     param:  allPlayersUtils
                                #          utilities earned in the corresponding round for all
                                #          players, based on the action taken
                                #--------------------------------------------------------------------#
                                assessAction = function(round, allPlayersActions, allPlayersUtils) {

                                  if (is.na(currState)) {
                                    callSuper(round, allPlayersActions, allPlayersUtils)
                                    return()
                                  }

                                  ownAction <- allPlayersActions[ID]
                                  qRow <- qTable[qTable$state == currState, ]

                                  oldProp <- NA
                                  if (ownAction == COOPERATE) {
                                    oldProp <- qRow$c_prop
                                  } else if (ownAction == DEVIATE) {
                                    oldProp <- qRow$d_prop
                                  } else {
                                    stop(paste("Unknown action: ", ownAction))
                                  }

                                  util <- NA
                                  if (SOCIAL_BEHAVIOR == "selfish") {
                                    util <- allPlayersUtils[ID]
                                  } else if (SOCIAL_BEHAVIOR == "altruistic") {
                                    util <- sum(allPlayersUtils) / 3
                                  } else {
                                    stop(paste("Unknown social behavior: ", SOCIAL_BEHAVIOR))
                                  }

                                  # calculate new propensity based on update function by
                                  # Sutton & Barto (1998), p.148
                                  newProp <- oldProp + ALPHA *
                                    (util + GAMMA * oeu - oldProp)

                                  if (ownAction == COOPERATE) {
                                    qTable[qTable$state == currState, ]$c_prop <<- newProp
                                  } else if (ownAction == DEVIATE) {
                                    qTable[qTable$state == currState, ]$d_prop <<- newProp
                                  } else {
                                    stop(paste("Unknown action: ", ownAction))
                                  }

                                  # Decay of epsilon: after a strategy has been fully
                                  # performed. Not after each action, to ensure that each
                                  # player has an equal probability to explore the same
                                  # amount of strategies.
                                  epsilon <<- epsilon * EPSILON_DECAY

                                  callSuper(round, allPlayersActions, allPlayersUtils)
                                },


                                #--------------------------------------------------------------------#
                                #  function: computeAction
                                #    Checks the state the player is currently in and selects the
                                #    best response. A state is represented by the actions a player
                                #    took in the previous rounds.
                                #--------------------------------------------------------------------#
                                computeAction = function() {

                                  action <- NA

                                  # get all previous actions
                                  prevActions <- getPreviousActions()

                                  # if not enough actions performed yet, choose random action
                                  if ((PLAYERS_PER_STATE == 1 && length(prevActions) < X) ||
                                      (PLAYERS_PER_STATE == PLAYERS_CNT && nrow(prevActions) < X)) {
                                    action <- if(runif(1) <= 0.33) COOPERATE else DEVIATE

                                  # if enough actions performed, choose accordingly
                                  } else {

                                    #current state = actions in previous rounds
                                    setCurrentState(prevActions)
                                    # get q-table row corresponding to current state
                                    qRow <- qTable[qTable$state == currState, ]

                                    # balancing between exploration and exploitation
                                    if (BALANCING == "greedy") {
                                      action <- getGreedyAction(qRow)
                                    }
                                    else if (BALANCING == "noise") {
                                      action <- getNoiseAction(qRow)
                                    }
                                    else {
                                      stop(paste("Unknown balancing type:", BALANCING))
                                    }
                                  }

                                  setOEU(action)
                                  return(action)
                                },


                                #--------------------------------------------------------------------#
                                #   function: getPreviousActions
                                #     Gets all previous actions (here: for current player)
                                #--------------------------------------------------------------------#
                                getPreviousActions = function() {
                                  if (PLAYERS_PER_STATE == 1) {
                                    # players own actions
                                    return(history[history$round >= 1, ID + 1])

                                  } else if (PLAYERS_PER_STATE == PLAYERS_CNT) {
                                    # all players' actions
                                    return(history[history$round >= 1, 2:(PLAYERS_CNT+1)])

                                  } else {
                                    stop(paste("invalid argument for PLAYERS_PER_STATE:",
                                               PLAYERS_PER_STATE))
                                  }
                                },


                                #--------------------------------------------------------------------#
                                #   function: setCurrentState
                                #     Sets the state the player is currently in.
                                #--------------------------------------------------------------------#
                                setCurrentState = function(prevActions) {
                                  if (PLAYERS_PER_STATE == 1) {
                                    currState <<- paste(tail(prevActions, X), collapse = "")

                                  } else if (PLAYERS_PER_STATE == PLAYERS_CNT) {
                                    lastActions <- tail(prevActions, X)
                                    currState <<- paste(paste(lastActions[,1], collapse = ""),
                                                        paste(lastActions[,2], collapse = ""),
                                                        paste(lastActions[,3], collapse = ""), sep = "")
                                  } else {
                                    stop(paste("invalid argument for PLAYERS_PER_STATE:",
                                               PLAYERS_PER_STATE))
                                  }
                                },


                                #--------------------------------------------------------------------#
                                #   function: getGreedyAction
                                #     Gets an action according to epsilon-greedy approach.
                                #--------------------------------------------------------------------#
                                getGreedyAction = function(qRow) {
                                  # choose random action, if none is preferred
                                  if (qRow$c_prop == qRow$d_prop) {
                                    return(if(runif(1) <= 0.5) COOPERATE else DEVIATE)

                                  # else use epsilon-greedy balancing between explore and exploit
                                  } else {
                                    if (runif(1) <= epsilon) {     # explore (epsilon %)
                                      if (qRow$c_prop < qRow$d_prop) {
                                        return(COOPERATE)
                                      } else {
                                        return(DEVIATE)
                                      }
                                    } else {                       # exploit (1-epsilon %)
                                      if (qRow$c_prop < qRow$d_prop) {
                                        return(DEVIATE)
                                      } else {
                                        return(COOPERATE)
                                      }
                                    }
                                  }
                                },


                                #--------------------------------------------------------------------#
                                #   function: getNoiseAction
                                #     Gets an action according to epsilon-noise approach.
                                #--------------------------------------------------------------------#
                                getNoiseAction = function(qRow) {

                                  # add noise to state's propensities - uniform distribution
                                  #c_prop <- qRow$c_prop + (qRow$c_prop * runif(1, -epsilon, epsilon))
                                  #d_prop <- qRow$d_prop + (qRow$d_prop * runif(1, -epsilon, epsilon))

                                  # add noise to state's propensities - normal distribution
                                  c_prop <- rnorm(1, qRow$c_prop, qRow$c_prop * epsilon)
                                  d_prop <- rnorm(1, qRow$d_prop, qRow$d_prop * epsilon)

                                  # select action
                                  if (c_prop > d_prop) {
                                    return(COOPERATE)
                                  } else if (d_prop > c_prop) {
                                    return(DEVIATE)
                                  } else {
                                    return(if(runif(1) <= 0.5) COOPERATE else DEVIATE)
                                  }
                                },


                                #--------------------------------------------------------------------#
                                #   function: setOEU
                                #     Sets the overall expected utility based on the prospective
                                #     action.
                                #--------------------------------------------------------------------#
                                setOEU = function(action) {
                                  if (SOCIAL_BEHAVIOR == "selfish") {
                                    if (action == COOPERATE) {
                                      oeu <<- UTIL_MAX - ownCoopCosts
                                    } else if (action == DEVIATE) {
                                      oeu <<- UTIL_MAX
                                    } else {
                                      stop(paste("Unknown action: ", action))
                                    }
                                  } else if (SOCIAL_BEHAVIOR == "altruistic") {
                                    oeu <<- (UTIL_MAX + UTIL_MAX + (UTIL_MAX - lowestCoopCosts)) / 3
                                  } else {
                                    stop(paste("Unknown social behavior: ", SOCIAL_BEHAVIOR))
                                  }
                                },


                                #--------------------------------------------------------------------#
                                #   function: getModelParameters
                                #     Returns the player's parametrical settings.
                                #--------------------------------------------------------------------#
                                getModelParameters = function() {
                                  return(c(callSuper(),
                                           paste("p", ID,  "_params", sep = ""), "#####",
                                           paste("p", ID, "_X", sep = ""), X,
                                           paste("p", ID, "_players_per_state", sep = ""), PLAYERS_PER_STATE,
                                           paste("p", ID, "_balancing", sep = ""), BALANCING,
                                           paste("p", ID, "_prop_start", sep = ""), PROP_START,
                                           paste("p", ID, "_epsilon_start", sep = ""), EPSILON_START,
                                           paste("p", ID, "_epsilon_decay", sep = ""), EPSILON_DECAY,
                                           paste("p", ID, "_alpha", sep = ""), ALPHA,
                                           paste("p", ID, "_gamma", sep = ""), GAMMA,
                                           paste("p", ID, "_social_behavior", sep = ""), SOCIAL_BEHAVIOR))
                                },


                                #--------------------------------------------------------------------#
                                #   function: getPersonalDetailColumns
                                #     Returns the player's columns for personal details.
                                #--------------------------------------------------------------------#
                                getPersonalDetailColumns = function() {
                                  columns <- c(paste("p", ID, sep = ""),
                                               paste("p", ID, "_epsilon", sep = ""),
                                               paste("p", ID, "_currstate", sep = ""),
                                               paste("p", ID, "_oeu", sep = ""))
                                  for (i in 1:nrow(qTable)) {
                                    columns <- c(columns,
                                                 paste("p", ID, "_c_", paste(qTable[i,]$state), sep = ""),
                                                 paste("p", ID, "_d_", paste(qTable[i,]$state), sep = ""))
                                  }
                                  return(columns)
                                },


                                #--------------------------------------------------------------------#
                                #   function: getCurrentPersonalDetails
                                #     Returns the player's current personal details.
                                #--------------------------------------------------------------------#
                                getCurrentPersonalDetails = function() {
                                  details <- c("#####",
                                               round(epsilon, digits = 5),
                                               currState,
                                               oeu)

                                  for (i in 1:nrow(qTable)) {
                                    details <- c(details,
                                                 qTable[qTable$state == paste(qTable[i,]$state), ]$c_prop,
                                                 qTable[qTable$state == paste(qTable[i,]$state), ]$d_prop)
                                  }
                                  return(details)
                                }
                              )
)
