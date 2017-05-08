###################################### SHARED GLOBAL PARAMETERS ###################################### 

########### FILE SYSTEM ############
PLAYERS_DIR <<- paste(BASE_DIR, "players/", sep = "")
SIM_DIR <<- paste(BASE_DIR, "../simulations/", sep = "")
BASE_FILENAME <<- "sim-"


############# LOGGING ##############
LOG_LEVEL <<- "debug"    # possible: "all", debug", "none"


########### GAME DESIGN ############
PLAYERS_CNT <<- 3
UTIL_MAX <- 80
COOP_COST_SYMM <<- 50
COOP_COST_ASYMM1 <<- 30
COOP_COST_ASYMM2 <<- 10
UTIL_NONE <<- 0

VOD_TYPES <<- c("sym",                    # all players have same coop costs
                "asym1",                  # one player has slightly lower coop costs
                "asym2")                  # one player has significantly lower coop costs

# actions
COOPERATE <<- 1
DEVIATE <<- 0


############## MODELS ##############
MODEL_TYPES <<- c("Random",               # purely random action selection
                  "ClassicQ",             # classical Q-Learning reinforcement strategy
                  "CoordinateX")          # coordinate-x reinforcement strategy

BALANCING_TYPES <<- c("greedy",           # espilon-greedy
                     "noise")             # espilon-noise
BALANCING_TYPE <<- BALANCING_TYPES[1]     # balancing type currently in use

# Random
RANDOM_COOP_RATIO <<- 1/3                 # ratio (cooperate:deviate) in favor of cooperation

# ClassicQ
CLASSIC_X <<- 3                           # amount of previous rounds defining a state
CLASSIC_PLAYERS_PER_STATE <<- PLAYERS_CNT # actions of how many players defining a state 
                                          # possible values: 
                                          #   1 = only own actions
                                          #   PLAYERS_CNT = all players' actions

# CoordinateX
COORD_X <<- 4                             # max. after how many rounds to cooperate


# LNIs taken from experiment 1 (Diekmann & Przepiorka, 2016, p.1321, Table 3.)
LNIS_EXP1 <- data.frame("sym_h1" = 3.3, "sym_h2" = 8.0, "sym_h3" = 49.5, "sym_others" = 39.2,
                        "asym1_h1" = 34.9, "asym1_h2" = 8.2, "asym1_h3" = 13.4, "asym1_others" = 43.5,
                        "asym2_h1" = 61.7, "asym2_h2" = 1.5, "asym2_h3" = 6.7, "asym2_others" = 30.1)


###################################### EVENTUAL INTEGRITY CHECKS #####################################
source(paste(BASE_DIR, "/tests/testConstants.R", sep = ""))
