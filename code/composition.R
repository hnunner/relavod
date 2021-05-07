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

########################################## GLOBAL PARAMETERS #########################################
if (!exists("BASE_DIR")) BASE_DIR <<- paste(dirname(sys.frame(1)$ofile), "/", sep = "")

############################################# FUNCTIONS ##############################################
#----------------------------------------------------------------------------------------------------#
#   function: initComposition
#     Sources required files and classes.
#----------------------------------------------------------------------------------------------------#
initComposition <- function() {
  source(paste(BASE_DIR, "constants.R", sep = ""))
  source(paste(BASE_DIR, "simulation.R", sep = ""))
  source(paste(BASE_DIR, "analysis.R", sep = ""))
}
initComposition()


#----------------------------------------------------------------------------------------------------#
#   function: simulateAndAnalyze
#     Composition of a full simulation including data analysis.
#----------------------------------------------------------------------------------------------------#
simulateAndAnalyze <- function(modelType = MODEL_TYPES[3],
                               vodType = "all",
                               vodCount = 2,
                               roundsPerVod = 20,
                               date = "latest",
                               dateCount = "latest") {

  # composition
  computeSimulation(modelType = modelType,
                    vodType = vodType,
                    vodCount = vodCount,
                    roundsPerVod = roundsPerVod)

  analyzeData(modelType = modelType,
              date = date,
              dateCount = dateCount,
              vodType = vodType)
}


#----------------------------------------------------------------------------------------------------#
#   function: fitParameters
#     Simulates a whole range of all available parameters.
#----------------------------------------------------------------------------------------------------#
fitParameters <- function() {

  # number of simulations and rounds per simulation
  vodCount <- 10
  roundsPerVod <- 150

  # Random, ClassicQ, SequenceX
  modelTypes <- MODEL_TYPES

  ##### Random #####
  # coop ratio
  randomCoopRatio <- 1/3

  ##### ClassicQ, SequenceX #####
  # exploration vs. exploitation: e-noise
  balancingTypes <- c(BALANCING_TYPES[2])             # e-noise
  epsilonDecays <- c(1)                               # no decay
  epsilonStarts <- c(0.1, 0.2)                        # 0.X = X standard deviation(s)

  # social preference: selfish, altruistic
  sBehavs <- SOCIAL_BEHAVIORS

  # initial propensity
  propStarts <- c(43.33, 67.5)

  # learning rate
  alphas <- c(0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70)
  # discount rate
  gammas <- c(0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)

  # ClassicQ
  classicXs <- c(2)               # actions per state
  cQPlayersPerStates <- c(1)      # players per state

  # CoordinateX
  coordXs <- c(3)                 # expected group size
  seqXs <- c(3)                   # expected group size


  for (modelType in modelTypes) {

    # Random
    if (modelType == "Random") {
      i <- 1
      fitCSV <- paste(SIM_DIR, gsub("-", "", Sys.Date(), fixed = TRUE), "-random-fit.csv", sep = "")
      cat(paste("\nsimulation", i,"out of", length(randomCoopRatio), ":\n",
                "\tmodel type:", modelType, "\n",
                "\trounds per VOD:", roundsPerVod, "\n",
                "\trandom coop ratio:", randomCoopRatio, "\n"))
      computeRandomSimulation(vodCount = vodCount,roundsPerVod = roundsPerVod,randomCoopRatio = randomCoopRatio)
      analyzeData(modelType = modelType, fit = TRUE, fitCSV = fitCSV)
      i <- i+1
      cat("done\n")

    } else {
      i <- 1
      for (socialBehavior in sBehavs) {
        for (balancingType in balancingTypes) {
          for (epsilonDecay in epsilonDecays) {
            for (epsilonStart in epsilonStarts) {
              for (propStart in propStarts) {
                for (alpha in alphas) {
                  for (gamma in gammas) {

                    # ClassicQ
                    if (modelType == "ClassicQ") {
                      fitCSV <- paste(SIM_DIR, gsub("-", "", Sys.Date(), fixed = TRUE), "-classicQ-fit.csv", sep = "")
                      for (classicX in classicXs) {
                        for (cQPlayersPerState in cQPlayersPerStates) {
                          cat(paste("\nsimulation", i,"out of",
                                      length(cQPlayersPerState) *
                                      length(classicXs) *
                                      length(gammas) *
                                      length(alphas) *
                                      length(epsilonDecays) *
                                      length(epsilonStarts) *
                                      length(propStarts) *
                                      length(sBehavs), ":\n",
                                    "\tmodel type:", modelType, "\n",
                                    "\trounds per VOD:", roundsPerVod, "\n",
                                    "\tbalancing type:", balancingType, "\n",
                                    "\tsocial behavior:", socialBehavior, "\n",
                                    "\tpropensity start:", propStart, "\n",
                                    "\tepsilon start:", epsilonStart, "\n",
                                    "\tepsilon decay:", epsilonDecay, "\n",
                                    "\talpha:", alpha, "\n",
                                    "\tgamma:", gamma, "\n",
                                    "\tclassicX:", classicX, "\n",
                                    "\tclassic players per state:", cQPlayersPerState, "\n"))
                          computeClassicQSimulation(vodCount = vodCount,
                                                    roundsPerVod = roundsPerVod,
                                                    balancingType = balancingType,
                                                    socialBehavior = socialBehavior,
                                                    propStart = propStart,
                                                    epsilonStart = epsilonStart,
                                                    epsilonDecay = epsilonDecay,
                                                    alpha = alpha,
                                                    gamma = gamma,
                                                    classicX = classicX,
                                                    cQPlayersPerState = cQPlayersPerState)
                          analyzeData(modelType = modelType, fit = TRUE, fitCSV = fitCSV)
                          i <- i+1
                          cat("done\n")
                        }
                      }
                    }

                    # CoordinateX
                    if (modelType == "CoordinateX") {
                      fitCSV <- paste(SIM_DIR, gsub("-", "", Sys.Date(), fixed = TRUE), "-coordinateX-fit.csv", sep = "")
                      for (coordX in coordXs) {
                        cat(paste("\nsimulation", i,"out of",
                                    length(coordXs) *
                                    length(gammas) *
                                    length(alphas) *
                                    length(epsilonDecays) *
                                    length(epsilonStarts) *
                                    length(propStarts) *
                                    length(sBehavs), ":\n",
                                  "\tmodel type:", modelType, "\n",
                                  "\trounds per VOD:", roundsPerVod, "\n",
                                  "\tbalancing type:", balancingType, "\n",
                                  "\tsocial behavior:", socialBehavior, "\n",
                                  "\tpropensity start:", propStart, "\n",
                                  "\tepsilon start:", epsilonStart, "\n",
                                  "\tepsilon decay:", epsilonDecay, "\n",
                                  "\talpha:", alpha, "\n",
                                  "\tgamma:", gamma, "\n",
                                  "\tcoordX:", coordX, "\n"))
                        computeCoordinateXSimulation(vodCount = vodCount,
                                                     roundsPerVod = roundsPerVod,
                                                     balancingType = balancingType,
                                                     socialBehavior = socialBehavior,
                                                     propStart = propStart,
                                                     epsilonStart = epsilonStart,
                                                     epsilonDecay = epsilonDecay,
                                                     alpha = alpha,
                                                     gamma = gamma,
                                                     coordX = coordX)
                        analyzeData(modelType = modelType, fit = TRUE, fitCSV = fitCSV)
                        i <- i+1
                        cat("done\n")
                      }
                    }

                    # SequenceX
                    if (modelType == "SequenceX") {
                      fitCSV <- paste(SIM_DIR, gsub("-", "", Sys.Date(), fixed = TRUE), "-sequencex-fit.csv", sep = "")
                      for (seqX in seqXs) {
                        cat(paste("\nsimulation", i,"out of",
                                    length(seqXs) *
                                    length(gammas) *
                                    length(alphas) *
                                    length(epsilonDecays) *
                                    length(epsilonStarts) *
                                    length(propStarts) *
                                    length(sBehavs), ":\n",
                                  "\trounds per VOD:", roundsPerVod, "\n",
                                  "\tmodel type:", modelType, "\n",
                                  "\tbalancing type:", balancingType, "\n",
                                  "\tsocial behavior:", socialBehavior, "\n",
                                  "\tpropensity start:", propStart, "\n",
                                  "\tepsilon start:", epsilonStart, "\n",
                                  "\tepsilon decay:", epsilonDecay, "\n",
                                  "\talpha:", alpha, "\n",
                                  "\tgamma:", gamma, "\n",
                                  "\tseqX:", seqX, "\n"))
                        computeSequenceXSimulation(vodCount = vodCount,
                                                   roundsPerVod = roundsPerVod,
                                                   balancingType = balancingType,
                                                   socialBehavior = socialBehavior,
                                                   propStart = propStart,
                                                   epsilonStart = epsilonStart,
                                                   epsilonDecay = epsilonDecay,
                                                   alpha = alpha,
                                                   gamma = gamma,
                                                   seqX = seqX)
                        analyzeData(modelType = modelType, fit = TRUE, fitCSV = fitCSV)
                        i <- i+1
                        cat("done\n")
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
