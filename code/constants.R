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
BALANCING_TYPE <<- BALANCING_TYPES[2]     # balancing type currently in use

SOCIAL_BEHAVIORS <<- c("selfish",         # maximization of own rewards
                       "altruistic")      # maximization of mutual rewards
SOCIAL_BEHAVIOR <<- SOCIAL_BEHAVIORS[2]   # social behavior currently in use

# Random
RANDOM_COOP_RATIO <<- 1/3                 # ratio (cooperate:deviate) in favor of cooperation

# ClassicQ
CLASSIC_X <<- 3                           # amount of previous rounds defining a state
CLASSIC_PLAYERS_PER_STATE <<- 1           # actions of how many players defining a state 
                                          # possible values: 
                                          #   1 = only own actions
                                          #   PLAYERS_CNT = all players' actions

# CoordinateX
COORD_X <<- 4                             # max. after how many rounds to cooperate

# Reinforcement Learning 
# (ClassicQ & CoordinateX)
PROP_START <<- 250                        # initial propensity for each strategy
EPSILON_START <<- 0.1                     # initial balance between exploration (epsilon) 
                                          # and exploration (1-epsilon)
EPSILON_DECAY <<- 0.995                       # rate at which epsilon is decreasing after 
                                          # each completion of a strategy
ALPHA <<- 0.4                             # RL learning rate, the higher the more important
                                          # recently learned information; 0 < ALPHA <= 1
GAMMA <<- 0.6                             # RL discount factor, the higher the more important
                                          # the previous rewards; 0 <= GAMMA <= 1

############# REF DATA ############
# LNIs taken from experiment 1 (Diekmann & Przepiorka, 2016, p.1321, Table 3.)
LNIS_EXP1 <- data.frame("sym_h1" = 3.3, "sym_h2" = 8.0, "sym_h3" = 49.5, "sym_others" = 39.2,
                        "asym1_h1" = 34.9, "asym1_h2" = 8.2, "asym1_h3" = 13.4, "asym1_others" = 43.5,
                        "asym2_h1" = 61.7, "asym2_h2" = 1.5, "asym2_h3" = 6.7, "asym2_others" = 30.1)
# dummy data
LNIS_TEST <- data.frame("sym_h1" = 1.5, "sym_h2" = 4.0, "sym_h3" = 23.5, "sym_others" = 71.0,
                        "asym1_h1" = 34.9, "asym1_h2" = 7.2, "asym1_h3" = 14.4, "asym1_others" = 43.5,
                        "asym2_h1" = 75.0, "asym2_h2" = 0.5, "asym2_h3" = 4.5, "asym2_others" = 20)

# SIMULATIONS
LNIS_CQ362 <- data.frame("sym_h1" = 10.33, "sym_h2" = 4, "sym_h3" = 0, "sym_others" = 86.33,
                         "asym1_h1" = 44.33, "asym1_h2" = 2.33, "asym1_h3" = 0, "asym1_others" = 52.33,
                         "asym2_h1" = 61.33, "asym2_h2" = 1, "asym2_h3" = 0, "asym2_others" = 36.67)
LNIS_CQ402 <- data.frame("sym_h1" = 4, "sym_h2" = 2, "sym_h3" = 0, "sym_others" = 93.33,
                         "asym1_h1" = 41.66, "asym1_h2" = 4, "asym1_h3" = 2, "asym1_others" = 53.67,
                         "asym2_h1" = 64, "asym2_h2" = 2, "asym2_h3" = 0, "asym2_others" = 34)
LNIS_CQ1442 <- data.frame("sym_h1" = 10, "sym_h2" = 1, "sym_h3" = 0, "sym_others" = 90,
                          "asym1_h1" = 35, "asym1_h2" = 3, "asym1_h3" = 2, "asym1_others" = 60.33,
                          "asym2_h1" = 61.7, "asym2_h2" = 1.5, "asym2_h3" = 6.7, "asym2_others" = 30.1)
LNIS_CQ1947 <- data.frame("sym_h1" = 0.65, "sym_h2" = 1.36, "sym_h3" = 1.07, "sym_others" = 96.68,
                          "asym1_h1" = 38.9, "asym1_h2" = 2.13, "asym1_h3" = 0.68, "asym1_others" = 58.18,
                          "asym2_h1" = 47.4, "asym2_h2" = 2, "asym2_h3" = 0.31, "asym2_others" = 50.39)

LNIS_CX200 <- data.frame("sym_h1" = 0, "sym_h2" = 2, "sym_h3" = 6.67, "sym_others" = 91.33,
                         "asym1_h1" = 38, "asym1_h2" = 2, "asym1_h3" = 3, "asym1_others" = 57,
                         "asym2_h1" = 63.67, "asym2_h2" = 2, "asym2_h3" = 0, "asym2_others" = 34.33)
LNIS_CX1983 <- data.frame("sym_h1" = 0, "sym_h2" = 2, "sym_h3" = 40.34, "sym_others" = 57.67,
                          "asym1_h1" = 23, "asym1_h2" = 0, "asym1_h3" = 2, "asym1_others" = 75,
                          "asym2_h1" = 67, "asym2_h2" = 0, "asym2_h3" = 0, "asym2_others" = 33)
LNIS_CX2648 <- data.frame("sym_h1" = 1, "sym_h2" = 4, "sym_h3" = 3, "sym_others" = 92,
                          "asym1_h1" = 36.67, "asym1_h2" = 2, "asym1_h3" = 4, "asym1_others" = 57.33,
                          "asym2_h1" = 58.33, "asym2_h2" = 1, "asym2_h3" = 4, "asym2_others" = 36.67)
LNIS_CX2807 <- data.frame("sym_h1" = 0, "sym_h2" = 2, "sym_h3" = 37.67, "sym_others" = 60.33,
                          "asym1_h1" = 0, "asym1_h2" = 2, "asym1_h3" = 9, "asym1_others" = 89,
                          "asym2_h1" = 45.33, "asym2_h2" = 2, "asym2_h3" = 2, "asym2_others" = 50.67)

############## PLOTS ##############
PLOT_MAX_ROUNDS <<- 150
PRE_GAP_LENGTH <<- 30                     # rounds before gap for plots > 150 rounds
POST_GAP_LENGTH <<- 110                   # rounds after gap for plots > 150 rounds
GAP_LENGTH <<- PLOT_MAX_ROUNDS -          # gap rounds for plots > 150 rounds
  (PRE_GAP_LENGTH + POST_GAP_LENGTH)
LABEL_STEP_SIZE <<- 5                     # display a label every x steps

###################################### EVENTUAL INTEGRITY CHECKS #####################################
source(paste(BASE_DIR, "/tests/testConstants.R", sep = ""))
