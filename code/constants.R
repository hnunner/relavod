###################################### SHARED GLOBAL PARAMETERS ###################################### 
# file system
PLAYERS_DIR <<- paste(BASE_DIR, "players/", sep = "")
SIM_DIR <<- paste(BASE_DIR, "../simulations/", sep = "")
BASE_FILENAME <<- "sim-"

# logging
LOG_LEVEL <<- "debug"    # possible: "all", debug", "none"

# VOD types
# !!! ALWAYS APPEND NEW VOD TYPES !!!
# !!! DO NOT CHANGE ORDERING !!!
VOD_TYPES <<- c("sym", 
                "asym1", 
                "asym2")

# model types 
# !!! ALWAYS APPEND NEW MODEL TYPES !!!
# !!! DO NOT CHANGE ORDERING !!!
MODEL_TYPES <<- c("OneShot",                # one-shot VOD mixed strategy equilibria
                  "CoordinateX",            # coordinate-x reinforcement strategy
                  "ClassicQ",               # classical Q-Learning reinforcement strategy
                  "Random")                 # classical Q-Learning reinforcement strategy

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

# LNIs taken from experiment 1 (Diekmann & Przepiorka, 2016, p.1321, Table 3.)
LNIS_EXP1 <- data.frame("sym_h1" = 3.3, "sym_h2" = 8.0, "sym_h3" = 49.5,
                        "asym1_h1" = 34.9, "asym1_h2" = 8.2, "asym1_h3" = 13.4,
                        "asym2_h1" = 61.7, "asym2_h2" = 1.5, "asym2_h3" = 6.7)


###################################### EVENTUAL INTEGRITY CHECKS #####################################
source(paste(BASE_DIR, "/tests/testConstants.R", sep = ""))
