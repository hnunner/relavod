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

#----------------------------------------------------------------------------------------------------#
#   function: checkIntegrity
#     Checks the integrity of the constants.
#----------------------------------------------------------------------------------------------------#
checkIntegrity <- function() {
  if (length(VOD_TYPES) != 3) stop("Integrity check failed: invalid amount of VOD types")
  if (VOD_TYPES[1] != "sym") stop("Integrity check failed: invalid first VOD type")
  if (VOD_TYPES[2] != "asym1") stop("Integrity check failed: invalid second VOD type")
  if (VOD_TYPES[3] != "asym2") stop("Integrity check failed: invalid third VOD type")

  if (length(MODEL_TYPES) != 4) stop("Integrity check failed: invalid amount of model types")
  if (MODEL_TYPES[1] != "Random") stop("Integrity check failed: invalid first model type")
  if (MODEL_TYPES[2] != "ClassicQ") stop("Integrity check failed: invalid second model type")
  if (MODEL_TYPES[3] != "CoordinateX") stop("Integrity check failed: invalid third model type")
  if (MODEL_TYPES[4] != "SequenceX") stop("Integrity check failed: invalid fourth model type")

  if (length(BALANCING_TYPES) != 2) stop("Integrity check failed: invalid amount of balancing types")
  if (BALANCING_TYPES[1] != "greedy") stop("Integrity check failed: invalid first balancing type")
  if (BALANCING_TYPES[2] != "noise") stop("Integrity check failed: invalid second balancing type")

  if (!dir.exists(SIM_DIR)) {
    stop("Integrity check failed: simulation directory missing")
  }

  if (!dir.exists(PLAYERS_DIR)) {
    stop("Integrity check failed: players directory missing")
  }

  print("Success: Constants integrity check completed.")
}
checkIntegrity()
