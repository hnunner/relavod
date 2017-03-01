###################################### SHARED GLOBAL PARAMETERS ###################################### 
# file system
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
                  "Coordinate4")            # coordinate-x reinforcement strategy, with x=4

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
  if (length(VOD_TYPES) != 2) stop("Data integrity check failed: wrong amount of VOD types")
}
checkIntegrity()