######################################## SOURCING MOTHER CLASS #######################################
if(!exists("Player", mode="function")) source(paste(PLAYERS_DIR, "player.R", sep = ""))
########################################## GLOBAL PARAMETERS #########################################
ASSESSMENT_INTERVAL <<- 1         # after how many rounds is the Q-Table being assessed
QL <<- 1                          # probability to switch actions, when aspiration level is not met

#####--------------------------------- WinStayLooseShiftPlayer ----------------------------------#####
# class: WinStayLooseShiftPlayer
#     Class extending the basic Player class. This class represents a player using the generalized
#     win-stay loose-shift strategy by Helbing et al. (2008). A player looks at her own and the 
#     actions taken by the other players in the previous round. Based on that she makes a 
#     deterministic decision either to cooperate or to deviate. Over time the success of her 
#     actions are compared with an aspiration level to evaluate the success and switch the 
#     resulting action for a certain situation if necessary.
#
#     Free parameters:
#       - ASSESSMENT_INTERVAL
#       - ASP_C                     # moved to initialize()
#       - ASP_D                     # moved to initialize()
#       - QL
#
#     ________________________________________________________________________________________
#     "Reference Class" (RC) concept found at http://adv-r.had.co.nz/OO-essentials.html
#----------------------------------------------------------------------------------------------------#
WinStayLooseShiftPlayer <- setRefClass("WinStayLooseShiftPlayer",
                                       
                                       #-------------------------------------------------------------# 
                                       #  class inheritance
                                       #-------------------------------------------------------------#
                                       contains = "Player",
                                       
                                       #-------------------------------------------------------------#
                                       #   class parameters (public by default)
                                       #      param:  qTable
                                       #          the Q-Table holding the information whether to 
                                       #          cooperate or deviate in a given state for; a state
                                       #          is defined by the action taken by the player 
                                       #          herself and the amount of players having deviated 
                                       #          in the previous round
                                       #-------------------------------------------------------------#
                                       fields = c("qTable", "currentState", "assessmentRound",
                                                  "ASP_C", "ASP_D"),
                                       
                                       #-------------------------------------------------------------#
                                       #  class methods (public by defualt)
                                       #-------------------------------------------------------------#
                                       methods = list(
                                         
                                         #-----------------------------------------------------------#
                                         #   function: initialize
                                         #     Initializes the Player.
                                         #     param:  ID
                                         #         the player's ID
                                         #     param:  coopCost
                                         #         the player's cost to cooperate
                                         #-----------------------------------------------------------#
                                         initialize = function(ID, coopCost) {

                                           # aspiration levels
                                           ASP_C <<- UTIL_MAX - coopCost
                                           ASP_D <<- ASP_C
                                           
                                           # Q-Table initialization
                                           # states
                                           state <- c("C0", "D0", "C1", "D1", "C2", "D2")
                                           # random initialization of propensities
                                           c_prop <- c()
                                           for (i in 1:length(state)) {
                                             c_prop <- c(c_prop, as.integer(runif(1) > 0.5))
                                           }
                                           d_prop <- 1 - c_prop
                                           # payoffs
                                           payoff_per_ar <- c(rep(0,length(state)))
                                           av_payoff_per_ar <- c(rep(0,length(state)))
                                           # aspiration level
                                           aspiration_level <- c(rep(c(ASP_C, ASP_D), length(state)/2))
                                           # composition of Q-Table
                                           qTable <<- data.frame(state, aspiration_level, payoff_per_ar, 
                                                                 av_payoff_per_ar, c_prop, d_prop)
                                           
                                           # current state initialization = random
                                           currentState <<- state[sample(length(state),1)]
                                           
                                           # assessment round initialization
                                           assessmentRound <<- 0
                                           
                                           # initializations of super class
                                           callSuper(ID, coopCost)
                                           
                                           if (LOG_LEVEL == "all") {
                                             print(paste("Win-Stay Loose-Shift Player", ID,
                                                         "successfully created!"))
                                           }
                                         },
                                         
                                         #-----------------------------------------------------------#
                                         #   function: assessAction
                                         #     Assesses the player's action.
                                         #     param:  round
                                         #          the round
                                         #     param:  allPlayersActions
                                         #          actions played in the corresponding round 
                                         #          by all players
                                         #     param:  util
                                         #          utility earned in the corresponding round, 
                                         #          based on the action taken
                                         #-----------------------------------------------------------#
                                         assessAction = function(round, allPlayersActions, util) {
                                           
                                           # Which round of the assessment period am I in?
                                           if (round %% ASSESSMENT_INTERVAL == 0) {
                                             assessmentRound <<- ASSESSMENT_INTERVAL
                                           } else {
                                             assessmentRound <<- round %% ASSESSMENT_INTERVAL
                                           }
                                           
                                           # updating payoffs
                                           qTable[qTable$state == currentState, ]$payoff_per_ar <<-
                                             qTable[qTable$state == currentState, ]$payoff_per_ar + util
                                           for (i in 1:length(qTable$state)) {
                                             qTable[i,]$av_payoff_per_ar <<- 
                                               qTable[i, ]$payoff_per_ar / assessmentRound
                                           }
                                           
                                           if (ID == 1) {
                                             print("##############################")
                                             print(paste("round: ", round, sep = ""))
                                             print(paste("assessmentRound: ", assessmentRound, sep = ""))
                                             print(paste("currentState: ", currentState, sep = ""))
                                             print(qTable)
                                           }
                                           
                                           
                                           # assessment of Q-Table
                                           if (assessmentRound == ASSESSMENT_INTERVAL) {
                                             
                                             for (i in 1:length(qTable$state)) {
                                               # if aspiration level for a state is not met
                                               if (qTable[i,]$av_payoff_per_ar > 0 &&
                                                 qTable[i,]$av_payoff_per_ar < 
                                                   qTable[i,]$aspiration_level) {
                                                 # and considering a noise factor
                                                 if (runif(1) < QL) {
                                                   # switch the propensities for the actions 
                                                   # of that state
                                                   cProp <- qTable[i,]$c_prop
                                                   dProp <- qTable[i,]$d_prop
                                                   qTable[i,]$c_prop <<- dProp
                                                   qTable[i,]$d_prop <<- cProp
                                                 }
                                               }
                                               
                                               # resetting the actual payoffs
                                               qTable[i,]$payoff_per_ar <<- 0
                                               qTable[i,]$av_payoff_per_ar <<- 0
                                             }
                                             
                                           }
                                           
                                           # TODO: random exploration 
                                           
                                           
                                           
                                           # current state
                                           ownAction <- if (allPlayersActions[ID] == 1)
                                             "C" else "D"
                                           N1 <- sum(allPlayersActions[-c(ID)])
                                           currentState <<- paste(ownAction, 
                                                                  N1, sep = "")
                                           
                                           
                                           
                                           callSuper(round, allPlayersActions, util)
                                         },
                                         
                                         #-----------------------------------------------------------#
                                         #  function: computeAction
                                         #    Selects the preferred action for the current state
                                         #    from the Q-Table
                                         #-----------------------------------------------------------#
                                         computeAction = function() {
                                           action <- 
                                             if (qTable[qTable$state == currentState, ]$c_prop == 1)
                                               COOPERATE else DEVIATE
                                           return(action)
                                         },
                                         
                                         #-----------------------------------------------------------#
                                         #   function: getModelParameters
                                         #     Returns the player's parametrical settings.
                                         #-----------------------------------------------------------#
                                         getModelParameters = function() {
                                           return(c(callSuper(), 
                                                    paste("p", ID, "_assessment_interval", sep = ""), 
                                                    ASSESSMENT_INTERVAL, 
                                                    paste("p", ID, "_aspiration_level_c", sep = ""), 
                                                    ASP_C, 
                                                    paste("p", ID, "_aspiration_level_d", sep = ""), 
                                                    ASP_D,
                                                    paste("p", ID, "_ql", sep = ""), QL))
                                         },
                                         
                                         #-----------------------------------------------------------#
                                         #   function: getPersonalDetailColumns
                                         #     Returns the player's columns for personal details.
                                         #-----------------------------------------------------------#
                                         getPersonalDetailColumns = function() {
                                           columns <- c(paste("p", ID, "_currstate", sep = ""),
                                                        paste("p", ID, "_assessment_round", sep = ""),
                                                        
                                                        paste("p", ID, "_asp_level_c0", sep = ""),
                                                        paste("p", ID, "_payoff_per_ar_c0", sep = ""),
                                                        paste("p", ID, "_av_payoff_per_ar_c0", sep = ""),
                                                        paste("p", ID, "_c_c0", sep = ""),
                                                        paste("p", ID, "_d_c0", sep = ""),
                                                        
                                                        paste("p", ID, "_asp_level_d0", sep = ""),
                                                        paste("p", ID, "_payoff_per_ar_d0", sep = ""),
                                                        paste("p", ID, "_av_payoff_per_ar_d0", sep = ""),
                                                        paste("p", ID, "_c_d0", sep = ""),
                                                        paste("p", ID, "_d_d0", sep = ""),
                                                        
                                                        paste("p", ID, "_asp_level_c1", sep = ""),
                                                        paste("p", ID, "_payoff_per_ar_c1", sep = ""),
                                                        paste("p", ID, "_av_payoff_per_ar_c1", sep = ""),
                                                        paste("p", ID, "_c_c1", sep = ""),
                                                        paste("p", ID, "_d_c1", sep = ""),
                                                        
                                                        paste("p", ID, "_asp_level_d1", sep = ""),
                                                        paste("p", ID, "_payoff_per_ar_d1", sep = ""),
                                                        paste("p", ID, "_av_payoff_per_ar_d1", sep = ""),
                                                        paste("p", ID, "_c_d1", sep = ""),
                                                        paste("p", ID, "_d_d1", sep = ""),
                                                        
                                                        paste("p", ID, "_asp_level_c2", sep = ""),
                                                        paste("p", ID, "_payoff_per_ar_c2", sep = ""),
                                                        paste("p", ID, "_av_payoff_per_ar_c2", sep = ""),
                                                        paste("p", ID, "_c_c2", sep = ""),
                                                        paste("p", ID, "_d_c2", sep = ""),
                                                        
                                                        paste("p", ID, "_asp_level_d2", sep = ""),
                                                        paste("p", ID, "_payoff_per_ar_d2", sep = ""),
                                                        paste("p", ID, "_av_payoff_per_ar_d2", sep = ""),
                                                        paste("p", ID, "_c_d2", sep = ""),
                                                        paste("p", ID, "_d_d2", sep = ""))
                                           return(columns)
                                         },
                                         
                                         #-----------------------------------------------------------#
                                         #   function: getCurrentPersonalDetails
                                         #     Returns the player's current personal details.
                                         #-----------------------------------------------------------#
                                         getCurrentPersonalDetails = function() {
                                           details <- c(currentState,
                                                        assessmentRound,
                                                        
                                                        qTable[qTable$state == "C0", ]$aspiration_level,
                                                        qTable[qTable$state == "C0", ]$payoff_per_ar,
                                                        qTable[qTable$state == "C0", ]$av_payoff_per_ar,
                                                        qTable[qTable$state == "C0", ]$c_prop,
                                                        qTable[qTable$state == "C0", ]$d_prop,
                                                        
                                                        qTable[qTable$state == "D0", ]$aspiration_level,
                                                        qTable[qTable$state == "D0", ]$payoff_per_ar,
                                                        qTable[qTable$state == "D0", ]$av_payoff_per_ar,
                                                        qTable[qTable$state == "D0", ]$c_prop,
                                                        qTable[qTable$state == "D0", ]$d_prop,
                                                        
                                                        qTable[qTable$state == "C1", ]$aspiration_level,
                                                        qTable[qTable$state == "C1", ]$payoff_per_ar,
                                                        qTable[qTable$state == "C1", ]$av_payoff_per_ar,
                                                        qTable[qTable$state == "C1", ]$c_prop,
                                                        qTable[qTable$state == "C1", ]$d_prop,
                                                        
                                                        qTable[qTable$state == "D1", ]$aspiration_level,
                                                        qTable[qTable$state == "D1", ]$payoff_per_ar,
                                                        qTable[qTable$state == "D1", ]$av_payoff_per_ar,
                                                        qTable[qTable$state == "D1", ]$c_prop,
                                                        qTable[qTable$state == "D1", ]$d_prop,
                                                        
                                                        qTable[qTable$state == "C2", ]$aspiration_level,
                                                        qTable[qTable$state == "C2", ]$payoff_per_ar,
                                                        qTable[qTable$state == "C2", ]$av_payoff_per_ar,
                                                        qTable[qTable$state == "C2", ]$c_prop,
                                                        qTable[qTable$state == "C2", ]$d_prop,
                                                        
                                                        qTable[qTable$state == "D2", ]$aspiration_level,
                                                        qTable[qTable$state == "D2", ]$payoff_per_ar,
                                                        qTable[qTable$state == "D2", ]$av_payoff_per_ar,
                                                        qTable[qTable$state == "D2", ]$c_prop,
                                                        qTable[qTable$state == "D2", ]$d_prop)
                                           return(details)
                                         }
                                       )
)
