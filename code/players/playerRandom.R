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

#####--------------------------------------- RandomPlayer ---------------------------------------#####
# class: RandomPlayer
#     Class extending the basic Player class. This class represents a player playing fully at random.
#
#     Free parameters:
#       - COOP_RATIO
#
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
RandomPlayer <- setRefClass("RandomPlayer",

                            #------------------------------------------------------------------------#
                            #  class inheritance
                            #------------------------------------------------------------------------#
                            contains = "Player",

                            #------------------------------------------------------------------------#
                            #   class parameters (public by default)
                            #      param:  COOP_RATIO
                            #           the ratio in favor of cooperation
                            #------------------------------------------------------------------------#
                            fields = c("COOP_RATIO"),

                            #------------------------------------------------------------------------#
                            #  class methods (public by defualt)
                            #------------------------------------------------------------------------#
                            methods = list(

                              #----------------------------------------------------------------------#
                              #   function: initialize
                              #     Initializes the Player.
                              #     param:  ID
                              #         the player's ID
                              #     param:  coopCosts
                              #         all players' cost to cooperate
                              #
                              #     param:  COOP_RATIO
                              #         see "class parameters"
                              #----------------------------------------------------------------------#
                              initialize = function(ID, coopCosts,
                                                    COOP_RATIO) {

                                COOP_RATIO <<- COOP_RATIO
                                callSuper(ID, coopCosts)

                                if (LOG_LEVEL == "all") {
                                  print(paste("Random Player", ID, "whith COOP_RATIO =",
                                              COOP_RATIO, "successfully created!"))
                                }
                              },


                              #----------------------------------------------------------------------#
                              #   function: validate
                              #     Integrity check for player.
                              #----------------------------------------------------------------------#
                              validate = function() {
                                if (!is.numeric(COOP_RATIO)) {
                                  stop("Error during player validation:
                                       'COOP_RATIO' must be numeric!")
                                }
                                if (COOP_RATIO > 1.0) {
                                  stop(paste("COOP_RATIO =", COOP_RATIO,
                                             "must not be greater than 1.0!"))
                                }
                                callSuper()
                              },


                              #----------------------------------------------------------------------#
                              #  function: computeAction
                              #    Selects the first action from the actions list. If the
                              #    action list is empty, it will be refilled based on the
                              #    propensities for the different strategies.
                              #----------------------------------------------------------------------#
                              computeAction = function() {
                                action <- if(runif(1) <= COOP_RATIO) COOPERATE else DEVIATE
                                return(action)
                              },


                              #----------------------------------------------------------------------#
                              #   function: getModelParameters
                              #     Returns the player's parametrical settings.
                              #----------------------------------------------------------------------#
                              getModelParameters = function() {
                                return(c(callSuper(),
                                         paste("p", ID, "_params", sep = ""), "#####",
                                         paste("p", ID, "_coop_ratio", sep = ""), COOP_RATIO))
                              }
                            )
)
