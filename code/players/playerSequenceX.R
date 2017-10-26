######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
X_MAX <<- 9              # triggers a warning during initialization, when X exceeds X_MAX
# PROP_START <<- 100
# EPSILON_START <<- 0.1
# EPSILON_DECAY <<- 1
# ALPHA <<- 0.4
# GAMMA <<- 0.6

#####------------------------------------ SequenceXPlayer -------------------------------------#####
# class: SequenceXPlayer
#     Class extending the basic Player class. This class represents a player using a sequence-x
#     strategy, with x representing the length of action sequences. 
#     E.g., x=3 means that the player will choose from a set of 8 actions: CCC, CCD, CDC, CDD,
#     DCC, DCD, DDC, DDD. Reinforcement learning is used to evaluate taken actions and epsilon-decay 
#     is used to balance between exploration and exploitation.
#
#     Free parameters:
#
#       X
#       BALANCING             either greedy or noise
#
#       PROP_START            initial propensity for each strategy
#       EPSILON_START         initial balance between exploration (epsilon) and exploration (1-epsilon)
#       EPSILON_DECAY         rate at which epsilon is decreasing after each completion of a strategy
#       ALPHA                 RL learning rate, the higher the more important recently learned 
#                             information; 0 < ALPHA <= 1
#       GAMMA                 RL discount factor, the higher the more important the previous rewards;
#                             0 <= GAMMA <= 1
#
#       SOCIAL_BEHAVIOR       either selfish (max. own rewards) or altruistic (max. mutual rewards)
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
SequenceXPlayer <- setRefClass("SequenceXPlayer",
                                 
                                 #-------------------------------------------------------------------# 
                                 #  class inheritance
                                 #-------------------------------------------------------------------#
                                 contains = "Player",
                                 
                                 #-------------------------------------------------------------------#
                                 #   class parameters (public by default)
                                 #      param:  X
                                 #           the amount of actions in the available sequences 
                                 #      param:  BALANCING
                                 #           how to balance between exploration and exploitation (see 
                                 #           BALANCING_TYPE in constants.R)
                                 #      param:  PROP_START
                                 #           initial propensity for each strategy
                                 #      param:  EPSILON_START
                                 #           initial balance between exploration (epsilon) and 
                                 #           exploration (1-epsilon)
                                 #      param:  EPSILON_DECAY
                                 #           rate at which epsilon is decreasing after each completion 
                                 #           of a strategy
                                 #      param:  ALPHA
                                 #           RL learning rate, the higher the more important recently 
                                 #           learned information; 0 < ALPHA <= 1
                                 #      param:  GAMMA
                                 #           RL discount factor, the higher the more important the 
                                 #           previous rewards; 0 <= GAMMA <= 1
                                 #      param:  SOCIAL_BEHAVIOR
                                 #           either selfish (max. own rewards) or 
                                 #           altruistic (max. mutual rewards)
                                 #      param:  sequences
                                 #          the player's available sequences (see "initialize")
                                 #      param:  currSeq
                                 #          the sequence currently used by the player
                                 #      param:  actions
                                 #          vector of action sequence for the upcoming rounds
                                 #      param:  epsilon
                                 #          balance between exploration and exploitation
                                 #      param:  oeu
                                 #          the optimal expected utility
                                 #-------------------------------------------------------------------#
                                 fields = c("X", "BALANCING", "PROP_START", "EPSILON_START", 
                                            "EPSILON_DECAY", "ALPHA", "GAMMA", "SOCIAL_BEHAVIOR", 
                                            "sequences", "currSeq", "actions", "epsilon", "oeu"),
                                 
                                 #-------------------------------------------------------------------#
                                 #  class methods (public by defualt)
                                 #-------------------------------------------------------------------#
                                 methods = list(
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: initialize
                                   #     Initializes the Player.
                                   #     param:  ID
                                   #         the player's ID
                                   #     param:  coopCosts
                                   #         all players' cost to cooperate
                                   #
                                   #     param:  X
                                   #         see "class parameters"
                                   #     param:  BALANCING
                                   #         see "class parameters"
                                   #     param:  PROP_START
                                   #           see "class parameters"
                                   #     param:  EPSILON_START
                                   #           see "class parameters"
                                   #     param:  EPSILON_DECAY
                                   #           see "class parameters"
                                   #     param:  ALPHA
                                   #           see "class parameters"
                                   #     param:  GAMMA
                                   #           see "class parameters"
                                   #-----------------------------------------------------------------#
                                   initialize = function(ID, coopCosts, 
                                                         X, BALANCING, PROP_START, EPSILON_START, 
                                                         EPSILON_DECAY, ALPHA, GAMMA, SOCIAL_BEHAVIOR) {
                                     
                                     X <<- X
                                     BALANCING <<- BALANCING
                                     PROP_START <<- PROP_START
                                     EPSILON_START <<- EPSILON_START
                                     EPSILON_DECAY <<- EPSILON_DECAY
                                     ALPHA <<- ALPHA
                                     GAMMA <<- GAMMA
                                     SOCIAL_BEHAVIOR <<- SOCIAL_BEHAVIOR
                                     
                                     # initialization of sequences
                                     sequences <<- initSequences()
                                     currSeq <<- NA
                                    
                                     # initialization of actions
                                     actions <<- c()
                                     
                                     # initialization of epsilon
                                     epsilon <<- EPSILON_START
                                     
                                     # initialization of the optimal utility estimate
                                     oeu <<- UTIL_NONE
                                     
                                     # initializations of super class
                                     callSuper(ID, coopCosts)
                                     
                                     if (LOG_LEVEL == "all") {
                                       print(paste("Sequence-X Player", ID, "whith X =", X,
                                                   "successfully created!"))
                                     }
                                   },
                                   
                                   
                                   #--------------------------------------------------------------------#
                                   #   function: initSequences
                                   #     Initialization of action sequences. X defines the length of
                                   #     available action sequences. For example: X=2 -- CC, CD, DC, DD
                                   #--------------------------------------------------------------------#
                                   initSequences = function() {
                                     library(gtools)
                                     seqs <- permutations(2, X, c(0, 1), set = FALSE, 
                                                          repeats.allowed = TRUE)
                                     seq <- c()
                                     for (i in 1:nrow(seqs)) {
                                       seq <- c(seq, paste(seqs[i,], collapse = ""))
                                     }
                                     
                                     props <- c(rep(PROP_START, length(seq)))
                                     
                                     return(data.frame(seq, props))
                                   },
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: validate
                                   #     Integrity check for player: ID must be numeric.
                                   #-----------------------------------------------------------------#
                                   validate = function() {
                                     if (!is.numeric(X)) {
                                       stop("Error during player validation: 'X' must be numeric!")
                                     }
                                     if (X > X_MAX) {
                                       warning(paste("X =", X, "appears to be irrationally high!"))
                                     }
                                     callSuper()
                                   },
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: assessAction
                                   #     Assesses the player's action.
                                   #     param:  round
                                   #          the round
                                   #     param:  allPlayersActions
                                   #          actions played in the corresponding round by all players
                                   #     param:  allPlayersUtils
                                   #          utilities earned in the corresponding round for all 
                                   #          players, based on the action taken
                                   #-----------------------------------------------------------------#
                                   assessAction = function(round, allPlayersActions, allPlayersUtils) {
                                     
                                     util <- NA
                                     if (SOCIAL_BEHAVIOR == "selfish") {
                                       util <- allPlayersUtils[ID]
                                     } else if (SOCIAL_BEHAVIOR == "altruistic") {
                                       util <- sum(allPlayersUtils) / 3
                                     } else {
                                       stop(paste("Unknown social behavior: ", SOCIAL_BEHAVIOR))
                                     }
                                     
                                     # calculate new propensity based on update function by 
                                     # Sutton & Barto (1998), p.148
                                     oldProp <- sequences[sequences$seq == currSeq, 2]
                                     newProp <- oldProp + ALPHA * 
                                       (util + GAMMA * oeu - oldProp)
                                     sequences[sequences$seq == currSeq, 2] <<- newProp
                                     
                                     # decay of epsilon
                                     epsilon <<- epsilon * EPSILON_DECAY
                                     
                                     if (LOG_LEVEL == "all") {
                                       if (ID == 1) {
                                         print(sequences)
                                       }
                                     }
                                     
                                     callSuper(round, allPlayersActions, allPlayersUtils)
                                   },
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #  function: computeAction
                                   #    Selects the first action from the actions list. If the 
                                   #    action list is empty, it will be refilled based on the
                                   #    propensities for the different sequences.
                                   #-----------------------------------------------------------------#
                                   computeAction = function() {
                                     
                                     # if no more actions planned ahead, choose a strategy
                                     if (length(actions) <= 0) {
                                       
                                       # balancing between exploration and exploitation
                                       if (BALANCING == "greedy") {
                                         currSeq <<- getGreedySeq()
                                       }
                                       else if (BALANCING == "noise") {
                                         currSeq <<- getNoisySeq() 
                                       }
                                       else {
                                         stop(paste("Unknown balancing type:", BALANCING))
                                       }
                                       
                                       # set new list of actions and OEU
                                       setActionsAndOEU()
                                     }
                                     
                                     # extract current action
                                     action <- actions[[1]]
                                     actions <<- tail(actions, (length(actions) - 1))
                                     return(action)
                                   },
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #  function: getGreedySeq
                                   #    Gets a sequence according to epsilon-greedy approach.
                                   #-----------------------------------------------------------------#
                                   getGreedySeq = function() {
                                     # either explore other sequences, or exploit best sequence
                                     # using epsilon-greedy approach, as suggested by
                                     # Sutton & Barto (1998), p.148f.
                                     pickableSeqs <- NA
                                     
                                     if (runif(1) <= epsilon) {     # explore (epsilon %)
                                       # pick a random sequence with a lower than highest sequence
                                       pickableSeqs <- 
                                         sequences[sequences$props < max(sequences$props),]
                                       if (!nrow(pickableSeqs)) {
                                         pickableSeqs <- sequences
                                       }
                                       
                                     } else {                       # exploit (1-epsilon %)
                                       # pick the strategy with the highest propensity
                                       pickableSeqs <- 
                                         sequences[sequences$props == max(sequences$props),]
                                     }
                                     
                                     return(pickableSeqs[sample(1:length(pickableSeqs$seq), 1), ]$seq)
                                   },   
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #  function: getNoisySeq
                                   #    Gets a sequence according to epsilon-noise approach.
                                   #-----------------------------------------------------------------#
                                   getNoisySeq = function() {
                                     # balancing between exploration and exploitation
                                     # by using epsilon as noise factor
                                     noisySeqs <- sequences
                                     
                                     for (i in 1:length(sequences$props)) {
                                       noisySeqs$props[i] <- sequences$props[i] + 
                                         (sequences$props[i] * runif(1, -epsilon, epsilon))
                                     }
                                     
                                     return(noisySeqs[with(noisySeqs, order(-props)),1][1])
                                   },                                         
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #  function: setActionsAndOEU
                                   #    Selects the list of actions and the corresponding optimal
                                   #    expected utility (OEU) based on the current sequence.
                                   #-----------------------------------------------------------------#
                                   setActionsAndOEU = function() {
                                     # choose action sequence and corresponding optimal expected 
                                     # utility based on sequence
                                     oeu <<- UTIL_NONE
                                     actions <<- c()
                                     
                                     seqSplit <- strsplit(toString(currSeq), "")[[1]]
                                     
                                     for (action in seqSplit) {
                                       if (action == DEVIATE) {
                                         actions <<- c(actions, DEVIATE)
                                         oeu <<- oeu + UTIL_MAX
                                         
                                       } else if (action == COOPERATE) {
                                         actions <<- c(actions, COOPERATE)
                                         oeu <<- oeu + (UTIL_MAX - ownCoopCosts)
                                         
                                       } else {
                                         stop(paste("Unknown action type:", actions[i]))
                                       }
                                     }
                                     
                                     oeu <<- oeu / length(seqSplit)
                                     
                                     if (SOCIAL_BEHAVIOR == "selfish") {
                                       # leave previous calculation
                                       
                                     } else if (SOCIAL_BEHAVIOR == "altruistic") {
                                       oeu <<- (UTIL_MAX + UTIL_MAX + (UTIL_MAX - lowestCoopCosts)) / 3
                                       
                                     } else {
                                       stop(paste("Unknown social behavior:", SOCIAL_BEHAVIOR))
                                     }
                                     
                                   },
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: getModelParameters
                                   #     Returns the player's parametrical settings.
                                   #-----------------------------------------------------------------#
                                   getModelParameters = function() {
                                     return(c(callSuper(), 
                                              paste("p", ID, "_params", sep = ""), "#####",
                                              paste("p", ID, "_X", sep = ""), X, 
                                              paste("p", ID, "_balancing", sep = ""), BALANCING, 
                                              paste("p", ID, "_prop_start", sep = ""), PROP_START, 
                                              paste("p", ID, "_epsilon_start", sep = ""), EPSILON_START,
                                              paste("p", ID, "_epsilon_decay", sep = ""), EPSILON_DECAY,
                                              paste("p", ID, "_alpha", sep = ""), ALPHA, 
                                              paste("p", ID, "_gamma", sep = ""), GAMMA,
                                              paste("p", ID, "_social_behavior", sep = ""), SOCIAL_BEHAVIOR))
                                   },
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: getPersonalDetailColumns
                                   #     Returns the player's columns for personal details.
                                   #-----------------------------------------------------------------#
                                   getPersonalDetailColumns = function() {
                                     columns <- c(paste("p", ID, sep = ""),
                                                  paste("p", ID, "_epsilon", sep = ""),
                                                  paste("p", ID, "_currseq", sep = ""),
                                                  paste("p", ID, "_actions", sep = ""),
                                                  paste("p", ID, "_optexputil", sep = ""))
                                     for (i in 1:length(sequences$seq)) {
                                       columns <- c(columns, paste("p", ID, "_seq", 
                                                                   sequences[i,1], sep = ""))
                                     }
                                     return(columns)
                                   },
                                   
                                   
                                   #-----------------------------------------------------------------#
                                   #   function: getCurrentPersonalDetails
                                   #     Returns the player's current personal details. In this case
                                   #     the player's peronal sequence settings.
                                   #-----------------------------------------------------------------#
                                   getCurrentPersonalDetails = function() {
                                     actionSeq <- ""
                                     for (action in actions) {
                                       actionSeq <- paste(actionSeq, action, sep = "")
                                     }
                                     details <- c("#####",
                                                  round(epsilon, digits = 5), 
                                                  currSeq, 
                                                  actionSeq, 
                                                  round(oeu, digits = 2))
                                     for (i in 1:length(sequences$seq)) {
                                       details <- c(details, round(sequences[i,2], digits = 2))
                                     }
                                     return(details)
                                   }
                                 )
)
