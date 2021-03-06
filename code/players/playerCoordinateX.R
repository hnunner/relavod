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
# PROP_START <<- 100
# EPSILON_START <<- 0.1
# EPSILON_DECAY <<- 1
# ALPHA <<- 0.4
# GAMMA <<- 0.6

#####------------------------------------ CoordinateXPlayer -------------------------------------#####
# class: CoordinateXPlayer
#     Class extending the basic Player class. This class represents a player using a coordinate-x
#     strategy, with x representing at which position of an action sequence the player cooperates.
#     E.g., x=4 means that the player will deviate three times and then coordinate. Reinforcement
#     learning is used to evaluate taken actions and epsilon-decay is used to balance between
#     exploration and exploitation.
#
#     Free parameters:
#
#       X
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
#       SOCIAL_BEHAVIOR       either selfish (max. own rewards) or altruistic (max. mutual rewards)
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
CoordinateXPlayer <- setRefClass("CoordinateXPlayer",

                                 #-------------------------------------------------------------------#
                                 #  class inheritance
                                 #-------------------------------------------------------------------#
                                 contains = "Player",

                                 #-------------------------------------------------------------------#
                                 #   class parameters (public by default)
                                 #      param:  X
                                 #           the amount of rounds representing a state; e.g., 3 means
                                 #           that a player looks at all actions in the previous 3 rounds
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
                                 #      param:  strategies
                                 #          the player's strategies (see "initialize")
                                 #      param:  currStrat
                                 #          the strategy currently used by the player
                                 #      param:  actions
                                 #          vector of action sequence for the upcoming rounds
                                 #      param:  epsilon
                                 #          balance between exploration and exploitation
                                 #      param:  oeu
                                 #          the optimal expected utility
                                 #-------------------------------------------------------------------#
                                 fields = c("X", "BALANCING", "PROP_START", "EPSILON_START",
                                            "EPSILON_DECAY", "ALPHA", "GAMMA", "SOCIAL_BEHAVIOR",
                                            "strategies", "currStrat", "actions",
                                            "epsilon", "oeu"),

                                 #-------------------------------------------------------------------#
                                 #  class methods (public by defualt)
                                 #-------------------------------------------------------------------#
                                 methods = list(

                                   #-----------------------------------------------------------------#
                                   #   function: initialize
                                   #     Initializes the Player.
                                   #     param:  ID
                                   #         the player's ID
                                   #     param:  coopCosts
                                   #         all players' cost to cooperate
                                   #
                                   #     param:  X
                                   #         see "class parameters"
                                   #     param:  BALANCING
                                   #         see "class parameters"
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
                                   #-----------------------------------------------------------------#
                                   initialize = function(ID, coopCosts,
                                                         X, BALANCING, PROP_START, EPSILON_START,
                                                         EPSILON_DECAY, ALPHA, GAMMA, SOCIAL_BEHAVIOR) {

                                     X <<- X
                                     BALANCING <<- BALANCING
                                     PROP_START <<- PROP_START
                                     EPSILON_START <<- EPSILON_START
                                     EPSILON_DECAY <<- EPSILON_DECAY
                                     ALPHA <<- ALPHA
                                     GAMMA <<- GAMMA
                                     SOCIAL_BEHAVIOR <<- SOCIAL_BEHAVIOR

                                     # initialization of strategies
                                     #    a single strategy is a tuple of:
                                     #      1. after how many deviations a player cooperates
                                     #      2. the player's propensity for the strategy
                                     coord <- c(0:X)
                                     prop <- rep(PROP_START, X+1)
                                     strategies <<- data.frame(coord, prop)
                                     currStrat <<- NA

                                     # initialization of actions
                                     actions <<- c()

                                     # initialization of epsilon
                                     epsilon <<- EPSILON_START

                                     # initialization of the optimal utility estimate
                                     oeu <<- UTIL_NONE

                                     # initializations of super class
                                     callSuper(ID, coopCosts)

                                     if (LOG_LEVEL == "all") {
                                       print(paste("Coordinate-X Player", ID, "whith X =", X,
                                                   "successfully created!"))
                                     }
                                   },


                                   #-----------------------------------------------------------------#
                                   #   function: validate
                                   #     Integrity check for player: ID must be numeric.
                                   #-----------------------------------------------------------------#
                                   validate = function() {
                                     if (!is.numeric(X)) {
                                       stop("Error during player validation: 'X' must be numeric!")
                                     }
                                     if (X > X_MAX) {
                                       warning(paste("X =", X, "appears to be irrationally high!"))
                                     }
                                     callSuper()
                                   },


                                   #-----------------------------------------------------------------#
                                   #   function: assessAction
                                   #     Assesses the player's action.
                                   #     param:  round
                                   #          the round
                                   #     param:  allPlayersActions
                                   #          actions played in the corresponding round by all players
                                   #     param:  allPlayersUtils
                                   #          utilities earned in the corresponding round for all
                                   #          players, based on the action taken
                                   #-----------------------------------------------------------------#
                                   assessAction = function(round, allPlayersActions, allPlayersUtils) {

                                     util <- NA
                                     if (SOCIAL_BEHAVIOR == "selfish") {
                                       util <- allPlayersUtils[ID]
                                     } else if (SOCIAL_BEHAVIOR == "altruistic") {
                                       util <- sum(allPlayersUtils) / 3
                                     } else {
                                       stop(paste("Unknown social behavior: ", SOCIAL_BEHAVIOR))
                                     }

                                     # if all actions have been performed, calculate new propensity
                                     # based on update function by Sutton & Barto (1998), p.148
                                     oldProp <- strategies[strategies$coord == currStrat,2]
                                     newProp <- oldProp + ALPHA *
                                       (util + GAMMA * oeu - oldProp)
                                     strategies[strategies$coord == currStrat,2] <<- newProp

                                     # Decay of epsilon: after a strategy has been fully
                                     # performed. Not after each action, to ensure that each
                                     # player has an equal probability to explore the same
                                     # amount of strategies.
                                     epsilon <<- epsilon * EPSILON_DECAY

                                     if (LOG_LEVEL == "all") {
                                       if (ID == 1) {
                                         print(strategies)
                                       }
                                     }

                                     callSuper(round, allPlayersActions, allPlayersUtils)
                                   },


                                   #-----------------------------------------------------------------#
                                   #  function: computeAction
                                   #    Selects the first action from the actions list. If the
                                   #    action list is empty, it will be refilled based on the
                                   #    propensities for the different strategies.
                                   #-----------------------------------------------------------------#
                                   computeAction = function() {

                                     # if no more actions planned ahead, choose a strategy
                                     if (length(actions) <= 0) {

                                       # balancing between exploration and exploitation
                                       if (BALANCING == "greedy") {
                                         currStrat <<- getGreedyStrat()
                                       }
                                       else if (BALANCING == "noise") {
                                         currStrat <<- getNoisyStrat()
                                       }
                                       else {
                                         stop(paste("Unknown balancing type:", BALANCING))
                                       }

                                       # set new list of actions and OEU
                                       setActionsAndOEU()
                                     }

                                     # extract current action
                                     action <- actions[[1]]
                                     actions <<- tail(actions, (length(actions) - 1))
                                     return(action)
                                   },


                                   #-----------------------------------------------------------------#
                                   #  function: getGreedyStrat
                                   #    Gets a strategy according to epsilon-greedy approach.
                                   #-----------------------------------------------------------------#
                                   getGreedyStrat = function() {
                                     # either explore other strategies, or exploit best strategy
                                     # using epsilon-greedy approach, as suggested by
                                     # Sutton & Barto (1998), p.148f.
                                     pickableStrategies <- NA

                                     if (runif(1) <= epsilon) {     # explore (epsilon %)
                                       # pick a random strategy with a lower than highest strategy
                                       pickableStrategies <-
                                         strategies[strategies$prop < max(strategies$prop),]
                                       if (!nrow(pickableStrategies)) {
                                         pickableStrategies <- strategies
                                       }

                                     } else {                       # exploit (1-epsilon %)
                                       # pick a random strategy with the highest propensity
                                       pickableStrategies <-
                                         strategies[strategies$prop == max(strategies$prop),]
                                     }

                                     return(pickableStrategies[
                                       sample(1:length(pickableStrategies$coord), 1), ]$coord)
                                   },


                                   #-----------------------------------------------------------------#
                                   #  function: getNoisyStrat
                                   #    Gets a strategy according to epsilon-noise approach.
                                   #-----------------------------------------------------------------#
                                   getNoisyStrat = function() {
                                     # balancing between exploration and exploitation
                                     # by using epsilon as noise factor
                                     noisyStrategies <- strategies

                                     for (i in 1:length(strategies$prop)) {
                                       # noise - uniform distribution
                                       #noisyStrategies$prop[i] <- strategies$prop[i] +
                                       #  (strategies$prop[i] * runif(1, -epsilon, epsilon))

                                       # noise - normal distribution
                                       noisyStrategies$prop[i] <- rnorm(1, strategies$prop[i],
                                                                        strategies$prop[i] * epsilon)
                                     }

                                     return(noisyStrategies[with(noisyStrategies, order(-prop)),1][1])
                                   },


                                   #-----------------------------------------------------------------#
                                   #  function: setActionsAndOEU
                                   #    Selects the list of actions and the corresponding optimal
                                   #    expected utility (OEU) based on the current strategy.
                                   #-----------------------------------------------------------------#
                                   setActionsAndOEU = function() {
                                     # choose action sequence and corresponding optimal expected
                                     # utility based on strategy
                                     oeu <<- UTIL_NONE

                                     # strategy = deviate once
                                     if (currStrat == 0) {
                                       actions <<- c(DEVIATE)
                                       oeu <<- UTIL_MAX

                                     # strategy = cooperate at position i
                                     } else {
                                       actions <<- c()
                                       i <- 1
                                       while (i < currStrat) {
                                         actions <<- c(actions, DEVIATE)
                                         oeu <<-
                                           oeu + UTIL_MAX
                                         i <- i+1
                                       }
                                       actions <<- c(actions, COOPERATE)
                                       oeu <<-
                                         oeu + (UTIL_MAX - ownCoopCosts)
                                       oeu <<-
                                         oeu / length(actions)
                                     }

                                     if (SOCIAL_BEHAVIOR == "selfish") {
                                       # leave previous calculation
                                     } else if (SOCIAL_BEHAVIOR == "altruistic") {
                                       oeu <<- (UTIL_MAX + UTIL_MAX + (UTIL_MAX - lowestCoopCosts)) / 3
                                     } else {
                                       stop(paste("Unknown social behavior:", SOCIAL_BEHAVIOR))
                                     }

                                   },


                                   #-----------------------------------------------------------------#
                                   #   function: getModelParameters
                                   #     Returns the player's parametrical settings.
                                   #-----------------------------------------------------------------#
                                   getModelParameters = function() {
                                     return(c(callSuper(),
                                              paste("p", ID,  "_params", sep = ""), "#####",
                                              paste("p", ID, "_X", sep = ""), X,
                                              paste("p", ID, "_balancing", sep = ""), BALANCING,
                                              paste("p", ID, "_prop_start", sep = ""), PROP_START,
                                              paste("p", ID, "_epsilon_start", sep = ""), EPSILON_START,
                                              paste("p", ID, "_epsilon_decay", sep = ""), EPSILON_DECAY,
                                              paste("p", ID, "_alpha", sep = ""), ALPHA,
                                              paste("p", ID, "_gamma", sep = ""), GAMMA,
                                              paste("p", ID, "_social_behavior", sep = ""), SOCIAL_BEHAVIOR))
                                   },


                                   #-----------------------------------------------------------------#
                                   #   function: getPersonalDetailColumns
                                   #     Returns the player's columns for personal details.
                                   #-----------------------------------------------------------------#
                                   getPersonalDetailColumns = function() {
                                     columns <- c(paste("p", ID, sep = ""),
                                                  paste("p", ID, "_epsilon", sep = ""),
                                                  paste("p", ID, "_currstrat", sep = ""),
                                                  paste("p", ID, "_actions", sep = ""),
                                                  paste("p", ID, "_optexputil", sep = ""))
                                     for (i in 1:length(strategies$coord)) {
                                       columns <- c(columns, paste("p", ID, "_coord",
                                                                   strategies[i,1], sep = ""))
                                     }
                                     return(columns)
                                   },


                                   #-----------------------------------------------------------------#
                                   #   function: getCurrentPersonalDetails
                                   #     Returns the player's current personal details. In this case
                                   #     the player's peronal strategy settings.
                                   #     Formatting: c(prop_coord-0, prop_coord-1, ..., prop_coord-x)
                                   #-----------------------------------------------------------------#
                                   getCurrentPersonalDetails = function() {
                                     actionSeq <- ""
                                     for (action in actions) {
                                       actionSeq <- paste(actionSeq, action)
                                     }
                                     details <- c("#####",
                                                  round(epsilon, digits = 5),
                                                  currStrat,
                                                  actionSeq,
                                                  round(oeu, digits = 2))
                                     for (i in 1:length(strategies$coord)) {
                                       details <- c(details, round(strategies[i,2], digits = 2))
                                     }
                                     return(details)
                                   }
                                 )
)
