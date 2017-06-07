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
  
  vodCount <- 2
  roundsPerVod <- 2
  
  for (modelType in MODEL_TYPES) {
    
    # Random
    if (modelType == "Random") {
      randomCoopRatio <- RANDOM_COOP_RATIO
      
      cat(paste("\nstart simulating:\n",
                "\tmodel type:", modelType, "\n",
                "\trandom coop ratio:", randomCoopRatio))
      
      computeRandomSimulation(vodCount = vodCount,
                              roundsPerVod = roundsPerVod,
                              randomCoopRatio = randomCoopRatio)
      analyzeData(modelType = modelType)
      
      cat("done")
    } else {
      
      for (balancingType in BALANCING_TYPES) {
        for (socialBehavior in SOCIAL_BEHAVIORS) {
          
          propStarts <- seq(0, 250, 10)
          for (propStart in propStarts) {
            
            epsilonStarts <- seq(0, 0.5, 0.02)
            for (epsilonStart in epsilonStarts) {
              
              epsilonDecays <- seq(0.8, 1, 0.005)
              for (epsilonDecay in epsilonDecays) {

                alphas <- seq(0.05, 1, 0.025)
                for (alpha in alphas) {

                  gammas <- seq(0.05, 1, 0.025)
                  for (gamma in gammas) {

                    
                    # ClassicQ                    
                    if (modelType == "ClassicQ") {
                      
                      classicXs <- seq(1, 5, 1)
                      for (classicX in classicXs) {
                        
                        classicPlayersPerStates <- c(1)
                        for (classicPlayersPerState in classicPlayersPerStates) {
                          
                          cat(paste("\nstart simulating:\n",
                                    "\tmodel type:", modelType, "\n",
                                    "\tbalancing type:", balancingType, "\n",
                                    "\tsocial behavior:", socialBehavior, "\n",
                                    "\tpropensity start:", propStart, "\n",
                                    "\tepsilon start:", epsilonStart, "\n",
                                    "\tepsilon decay:", epsilonDecay, "\n",
                                    "\talpha:", alpha, "\n",
                                    "\tgamma:", gamma, "\n",
                                    "\tclassicX:", classicX, "\n",
                                    "\tclassic players per state:", classicPlayersPerState))
                          
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
                          
                          analyzeData(modelType = modelType)
                          
                          cat("done")
                        }
                      }
                    }

                                        
                    # CoordinateX
                    if (modelType == "CoordinateX") {
                      
                      coordXs <- seq(1, 8, 1) 
                      for (coordX in coordXs) {
                        
                        cat(paste("\nstart simulating:\n",
                                  "\tmodel type:", modelType, "\n",
                                  "\tbalancing type:", balancingType, "\n",
                                  "\tsocial behavior:", socialBehavior, "\n",
                                  "\tpropensity start:", propStart, "\n",
                                  "\tepsilon start:", epsilonStart, "\n",
                                  "\tepsilon decay:", epsilonDecay, "\n",
                                  "\talpha:", alpha, "\n",
                                  "\tgamma:", gamma, "\n",
                                  "\tcoordX:", coordX))
                        
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
                        
                        analyzeData(modelType = modelType)
                        
                        cat("done")
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