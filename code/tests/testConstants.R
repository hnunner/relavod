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
  if (MODEL_TYPES[1] != "OneShot") stop("Integrity check failed: invalid first model type")
  if (MODEL_TYPES[2] != "CoordinateX") stop("Integrity check failed: invalid second model type")
  if (MODEL_TYPES[3] != "ClassicQ") stop("Integrity check failed: invalid third model type")
  if (MODEL_TYPES[4] != "Random") stop("Integrity check failed: invalid fourth model type")
  
  if (!file.exists(SIM_DIR)) {
    stop("Integrity check failed: simulation directory missing")
  }
  
  if (!file.exists(PLAYERS_DIR)) {
    stop("Integrity check failed: players directory missing")
  }
  
  print("Success: Constants integrity check completed.")
}
checkIntegrity()
