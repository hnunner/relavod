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
  
  vodCount <- 10
  roundsPerVod <- 150
  
  for (modelType in MODEL_TYPES[3]) {
    
    # Random
    if (modelType == "Random") {
      i <- 1

      fitCSV <- paste(SIM_DIR, gsub("-", "", Sys.Date(), fixed = TRUE), "-random-fit.csv", sep = "")
      randomCoopRatios <- c(0.25, RANDOM_COOP_RATIO, 0.5, 2*RANDOM_COOP_RATIO, 0.75)

      for (randomCoopRatio in randomCoopRatios) {
        cat(paste("\nsimulation", i,"out of", length(randomCoopRatios), ":\n",
                  "\tmodel type:", modelType, "\n",
                  "\trandom coop ratio:", randomCoopRatio, "\n"))

        computeRandomSimulation(vodCount = vodCount,
                                roundsPerVod = roundsPerVod,
                                randomCoopRatio = randomCoopRatio)
        analyzeData(modelType = modelType, fit = TRUE, fitCSV = fitCSV)

        i <- i+1
        cat("done\n")
      }
    } else {
      i <- 1
      
      bTypes <- BALANCING_TYPES[2]
      sBehavs <- SOCIAL_BEHAVIORS[1]
      for (balancingType in bTypes) {
        for (socialBehavior in sBehavs) {
          
          propStarts <- c(100)
          for (propStart in propStarts) {
            
            epsilonStarts <- c(0.05)
            for (epsilonStart in epsilonStarts) {
              
              epsilonDecays <- c(0.98)
              for (epsilonDecay in epsilonDecays) {

                alphas <- c(0.4)
                for (alpha in alphas) {

                  gammas <- c(0.5)
                  for (gamma in gammas) {

                    
                    # ClassicQ                    
                    if (modelType == "ClassicQ") {
                      
                      fitCSV <- paste(SIM_DIR, gsub("-", "", Sys.Date(), fixed = TRUE), "-classicQ-fit.csv", sep = "")
                      
                      classicXs <- c(3)
                      for (classicX in classicXs) {
                        
                        classicPlayersPerStates <- c(3)
                        for (classicPlayersPerState in classicPlayersPerStates) {
                          
                          cat(paste("\nsimulation", i,"out of",
                                      length(classicPlayersPerStates) *
                                      length(classicXs) *
                                      length(gammas) *
                                      length(alphas) *
                                      length(epsilonDecays) *
                                      length(epsilonStarts) *
                                      length(propStarts) *
                                      length(sBehavs) *
                                      length(bTypes), ":\n",
                                    "\tmodel type:", modelType, "\n",
                                    "\tbalancing type:", balancingType, "\n",
                                    "\tsocial behavior:", socialBehavior, "\n",
                                    "\tpropensity start:", propStart, "\n",
                                    "\tepsilon start:", epsilonStart, "\n",
                                    "\tepsilon decay:", epsilonDecay, "\n",
                                    "\talpha:", alpha, "\n",
                                    "\tgamma:", gamma, "\n",
                                    "\tclassicX:", classicX, "\n",
                                    "\tclassic players per state:", classicPlayersPerState, "\n"))
                          
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
                                                    classicPlayersPerState = classicPlayersPerState)
                          
                          analyzeData(modelType = modelType, fit = TRUE, fitCSV = fitCSV)
                          
                          i <- i+1
                          cat("done\n")
                        }
                      }
                    }

                                        
                    # CoordinateX
                    if (modelType == "CoordinateX") {
                      
                      fitCSV <- paste(SIM_DIR, gsub("-", "", Sys.Date(), fixed = TRUE), "-coordinateX-fit.csv", sep = "")
                      
                      coordXs <- c(3)
                      for (coordX in coordXs) {
                        
                        cat(paste("\nsimulation", i,"out of",
                                    length(coordXs) *
                                    length(gammas) *
                                    length(alphas) *
                                    length(epsilonDecays) *
                                    length(epsilonStarts) *
                                    length(propStarts) *
                                    length(sBehavs) *
                                    length(bTypes), ":\n",
                                  "\tmodel type:", modelType, "\n",
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
                        
                        analyzeData(modelType = modelType, fit = FALSE, fitCSV = fitCSV)
                        
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