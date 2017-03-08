########################################## GLOBAL PARAMETERS #########################################
BASE_DIR <<- paste(dirname(sys.frame(1)$ofile), "/", sep = "")

source(paste(BASE_DIR, "simulation.R", sep = ""))
source(paste(BASE_DIR, "analysis.R", sep = ""))

simulateAndAnalyze <- function() {
  computeSimulation()
  analyzeData()
}