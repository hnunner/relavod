###################################### SHARED GLOBAL PARAMETERS ###################################### 
# file system
PLAYERS_DIR <<- paste(BASE_DIR, "players/", sep = "")
SIM_DIR <<- paste(BASE_DIR, "../simulations/", sep = "")
BASE_FILENAME <<- "sim-"

# logging
LOG_LEVEL <<- "debug"    # possible: "all", debug", "none"

# VOD types
# !!! ALWAYS APPEND NEW MODEL TYPES !!!
# !!! DO NOT CHANGE ORDERING !!!
VOD_TYPES <<- c("sym", 
                "asym1", 
                "asym2")

# model types 
# !!! ALWAYS APPEND NEW MODEL TYPES !!!
# !!! DO NOT CHANGE ORDERING !!!
MODEL_TYPES <<- c("default",                # one-shot VOD mixed strategy equilibria
                  "CoordinateX")            # coordinate-x reinforcement strategy

# actions
COOPERATE <<- 1
DEVIATE <<- 0

# game design
PLAYERS_CNT <<- 3
UTIL_MAX <- 80
COOP_COST_SYMM <<- 50
COOP_COST_ASYMM1 <<- 30
COOP_COST_ASYMM2 <<- 10
UTIL_NONE <<- 0

#----------------------------------------------------------------------------------------------------#
#   function: checkIntegrity
#     Checks the integrity of the constants.
#----------------------------------------------------------------------------------------------------#
checkIntegrity <- function() {
  if (length(VOD_TYPES) != 3) stop("Integrity check failed: invalid amount of VOD types")
  if (VOD_TYPES[1] != "sym") stop("Integrity check failed: invalid first VOD type")
  if (VOD_TYPES[2] != "asym1") stop("Integrity check failed: invalid second VOD type")
  if (VOD_TYPES[3] != "asym2") stop("Integrity check failed: invalid third VOD type")
  
  if (length(MODEL_TYPES) != 2) stop("Integrity check failed: invalid amount of model types")
  if (MODEL_TYPES[1] != "default") stop("Integrity check failed: invalid first model type")
  if (MODEL_TYPES[2] != "CoordinateX") stop("Integrity check failed: invalid second model type")
  
  if (!file.exists(SIM_DIR)) {
    stop("Integrity check failed: simulation directory missing")
  }
  
  if (!file.exists(PLAYERS_DIR)) {
    stop("Integrity check failed: players directory missing")
  }
  
  if (LOG_LEVEL == "all") {
    print("Success: Integrity check completed.")
  }
}
checkIntegrity()
