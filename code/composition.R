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
                               vodCount = 15,
                               roundsPerVod = 150,
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