########################################## GLOBAL PARAMETERS #########################################
if (!exists("BASE_DIR")) BASE_DIR <<- paste(dirname(sys.frame(1)$ofile), "/", sep = "")
if (!exists("MODEL_TYPES")) source(paste(BASE_DIR, "constants.R", sep = ""))


######################################## FILE SYSTEM / IMPORTS #######################################
getSimCount <- function(modelDir, simCount = "latest") {
  if (simCount == "latest") {
    simDirs <- list.dirs(modelDir, recursive = FALSE)
    simCounts <- gsub(paste(modelDir, "/", sep = ""), "", simDirs, fixed = TRUE)
    simCount <- max(as.numeric(simCounts))
  }
  return(simCount)
}

getLatestSimCount <- function(modelType) {
  modelDir <- paste(SIM_DIR, modelType, sep = "")
  return(getSimCount(modelDir))
}


#----------------------------------------------------------------------------------------------------#
# function: getVodBaseDir
#     Returns the base directory of a model.
#     Data is stored as: "SIM_DIR/modelType/simCount/vodType/sim-X.Rdata"
#     Base directory corresponds to: "SIM_DIR/modelType/simCount/."
#     For available for modelTypes, see constants.R - 'MODEL_TYPES'
#     param:  modelType
#         the model type used by the simulation
#     param:  simCount
#         defines the round of simulation
#         (if undefined: latest available round of simulations)
#----------------------------------------------------------------------------------------------------#
getVodBaseDir <- function(modelType, simCount = "latest") {
  modelDir <- paste(SIM_DIR, modelType, sep = "")
  baseDir <- paste(modelDir, "/", 
                   getSimCount(modelDir, simCount), 
                   sep = "")
  
  return(baseDir)
}

#----------------------------------------------------------------------------------------------------#
# function: importVodSimData
#     Imports VOD simulation data. 
#     Data is stored as: "SIM_DIR/modelType/simCount/vodType/sim-X.Rdata"
#     For available for modelTypes, see constants.R - 'MODEL_TYPES'
#     param:  modelType
#         the model type used by the simulation
#     param:  simCount
#         defines the round of simulation
#         (if undefined: latest available round of simulations)
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: "sym", "asym1", "asym2"
#----------------------------------------------------------------------------------------------------#
importVodSimData <- function(modelType = MODEL_TYPES[7],
                             simCount = "latest", 
                             vodType = "sym") {
  
  vodBaseDir <- getVodBaseDir(modelType, simCount)
  vodTypeDir <- paste(vodBaseDir, "/", vodType, sep = "")
  
  vodSimData = list()
  simCountFiles <- list.files(vodTypeDir, recursive = FALSE)
  for (i in 1:(length(simCountFiles))) {
    filename <- paste(vodTypeDir, "/", BASE_FILENAME, i, ".Rdata", sep = "")
    vodSimData[[i]] <- get(load(filename))
  }
  
  return(vodSimData)
}


###################################### STATISTICAL COMPUTATIONS ######################################
#----------------------------------------------------------------------------------------------------#
# function: extractLNISequence
#   Extracts the interaction sequence required to compute the LNI. For details, see Diekmann & 
#   Przepiorka (2016), p.1318.
#    "1" - player 1 coordinates, others deviate
#    "2" - player 2 coordinates, others deviate
#    "3" - player 3 coordinates, others deviate
#   "-1" - either more than one player coordinates, or all players deviate
#   param:  vodData
#       the VOD data to extract the LNI sequence from
#----------------------------------------------------------------------------------------------------#
extractLNISequence <- function(vodData) {
  moves <- vodData[vodData$round >= 1,2:4]
  
  moves$lniSequence <- -1
  moves[moves$player1 == COOPERATE 
        & moves$player2 == DEVIATE 
        & moves$player3 == DEVIATE, "lniSequence"] <- 1
  moves[moves$player1 == DEVIATE 
        & moves$player2 == COOPERATE 
        & moves$player3 == DEVIATE, "lniSequence"] <- 2
  moves[moves$player1 == DEVIATE 
        & moves$player2 == DEVIATE 
        & moves$player3 == COOPERATE, "lniSequence"] <- 3
  
  return(moves$lniSequence)
}

#----------------------------------------------------------------------------------------------------#
# function: computeLNIs
#   Computation of the Latent Norm Index (LNI) for the given LNI sequence.
#   param:  lniSequence
#       the LNI sequence 
#----------------------------------------------------------------------------------------------------#
computeConvergencePatterns <- function(lniSequence) {

  # -1's
  min1s <- as.numeric(lniSequence == "-1")
  
  # classical patterns
  h1 <- computeConvergencePattern(1, lniSequence)
  h2 <- computeConvergencePattern(2, lniSequence)
  h3 <- computeConvergencePattern(3, lniSequence)
  
  # hgher order patterns
  h4 <- computeConvergencePattern(4, lniSequence)
  h5 <- computeConvergencePattern(5, lniSequence)
  h6 <- computeConvergencePattern(6, lniSequence)
  h7 <- computeConvergencePattern(7, lniSequence)
  h8 <- computeConvergencePattern(8, lniSequence)
  h9 <- computeConvergencePattern(9, lniSequence)
  
  # everything that's not falling under any of the categories above
  others <- as.numeric(!(min1s|h1|h2|h3|h4|h5|h6|h7|h8|h9))
  
  res <- data.frame(min1s, h1, h2, h3, h4, h5, h6, h7, h8, h9, others)
  return(res)
}




#----------------------------------------------------------------------------------------------------#
# function: computeConvergencePattern
#   Generic computation of convergence patterns per sequence length.
#   param:  seqLength
#       the sequence length
#   param:  lniSequence
#       the LNI sequence 
#   param:  higherOrder
#       flag denoting whether analysis is for classical analysis (h1, h2, h3) as in Diekmann and
#       Przepiorka (2016), or for higher order (h3+) patterns in which numbers can reoccur
#----------------------------------------------------------------------------------------------------#
computeConvergencePattern <- function(seqLength, lniSequence) {
  
  pattern <- rep(0, length(lniSequence))
  i <- 1
  
  while (i+(seqLength-1) <= length(lniSequence)) {
    j <- i+(seqLength-1)
    k <- i+(seqLength)
    currInteraction <- lniSequence[i:j]
    
    # skip if there is a "-1" in the current interaction
    skip <- any(currInteraction == -1)

    # skip if there are duplicates in any triplet
    # e.g. current interaction of 8 actions = 2 2/3 triplets: "123|231|21"
    # this is to make sure that for higher order interactions (h3+) lower order 
    # instances (e.g., 111|111|11) are not considered as high order instance
    currInteractionCopy <- currInteraction
    while (!skip && length(currInteractionCopy) > 0) {
      lowerBound <- 1
      upperBound <- 2
      
      # compare each element of current triplet for equality
      while (!skip && lowerBound < 3 && lowerBound < length(currInteractionCopy)) {
        while (!skip && upperBound < 4 && upperBound <= length(currInteractionCopy)) {
          
          if (currInteractionCopy[lowerBound] == currInteractionCopy[upperBound]) {
            skip <- TRUE
          } 
          upperBound <- upperBound+1
        }
        lowerBound <- lowerBound+1
        upperBound <- lowerBound+1
      }
      
      # switch to next triplet
      if (length(currInteractionCopy) > 3) {
        currInteractionCopy <- tail(currInteractionCopy, length(currInteractionCopy)-3)
      } else {
        currInteractionCopy <- c()
      }
    }
    
    if (skip) {
      i <- i+1      
      j <- j+1
      k <- k+1
      next
    }
    
    tmpPattern <- rep(0, length(lniSequence))
    tmpPattern[i:(k-1)] <- 1
    
    alternatingIndex <- 0
    while (k <= length(lniSequence)
           & lniSequence[k] == currInteraction[(alternatingIndex%%seqLength)+1]) {
      tmpPattern[k] <- 1
      k <- k+1
      alternatingIndex <- alternatingIndex+1
    }
    if (seqLength > 3) {
      tmpPattern <- compressConvergencePattern(tmpPattern, seqLength)
    } else {
      tmpPattern <- compressConvergencePattern(tmpPattern, 3)
    }
    pattern <- as.numeric(pattern|tmpPattern)
    
    if ((k-1) <= i) {
      i <- i+1
    } else {
      i <- k-1
    }
  }
  
  return(pattern)
}

#----------------------------------------------------------------------------------------------------#
# function: compressConvergencePattern
#   Makes sure that a convergence pattern constitutes of at least three actions. Everything else
#   will be erased, resulting in a compressed pattern.
#   param:  pattern
#       the pattern to compress
#   param:  minLength
#       the minimum length of actions
#----------------------------------------------------------------------------------------------------#
compressConvergencePattern <- function(pattern, minLength) {
  res <- c()
  i <- 1
  
  # loop over whole pattern
  while (i <= length(pattern)) {
    
    # 0 = no pattern at all
    if (pattern[i] == 0) {
      res[i] <- 0
      i <- i+1
      
    } else {
      # 1. count the occurrence
      cnt <- 1
      j <- i+1
      while (j <= length(pattern) && pattern[j] == 1) {
        cnt <- cnt+1
        j <- j+1
      }
      
      # 2. add only, if at least three actions in a single pattern
      if (cnt >= minLength) {
        res[i:(i+(cnt-1))] <- 1
      } else {
        res[i:(i+(cnt-1))] <- 0
      }
      i <- i+cnt
    }
  }
  return(res)
}






#----------------------------------------------------------------------------------------------------#
# function: computeConvergencePatterns
#   Computation of predominant behavioral patterns (h1-h3 as in Diekmann and Przepiorka, and 
#   more complex, additonal patterns)
#   param:  lniSequence
#       the LNI sequence 
#----------------------------------------------------------------------------------------------------#
computeLNIs <- function(lniSequence) {
  
  
  # 1-sequences
  lni13 <- computeLNISequence(1, lniSequence)
  # 2-sequences
  lni23 <- computeLNISequence(2, lniSequence)
  # 3-sequences
  lni33 <- computeLNISequence(3, lniSequence)
  
  # 4-sequences
  lni43 <- computeLNISequence(4, lniSequence)
  # 5-sequences
  lni53 <- computeLNISequence(5, lniSequence)
  # 6-sequences
  lni63 <- computeLNISequence(6, lniSequence)
  # 7-sequences
  lni73 <- computeLNISequence(7, lniSequence)
  # 8-sequences
  lni83 <- computeLNISequence(8, lniSequence)
  # 9-sequences
  lni93 <- computeLNISequence(9, lniSequence)
  
  # minus-1-sequences
  elements <- table(lniSequence)
  lnimin13 <- 0
  if (length(elements[names(elements) == "-1"]) > 0) {
    lnimin13 <- as.numeric(elements[names(elements) == "-1"]) / length(lniSequence) * 100
  } 
  
  res <- data.frame(lni13, lni23, lni33, (100 - sum(lni13, lni23, lni33)),
                    lni43, lni53, lni63, lni73, lni83, lni93, lnimin13)
  
  return(res)
}

#----------------------------------------------------------------------------------------------------#
# function: computeLNISequence
#   Generic computation of LNI sequences.
#   param:  seqLength
#       the sequence length
#   param:  lniSequence
#       the LNI sequence 
#   param:  higherOrder
#       flag denoting whether analysis is for classical analysis (h1, h2, h3) as in Diekmann and
#       Przepiorka (2016), or for higher order (h3+) patterns in which numbers can reoccur
#----------------------------------------------------------------------------------------------------#
computeLNISequence <- function(seqLength, lniSequence, higherOrder = FALSE) {

  sequences <- c()
  i <- 1
  
  while (i+(seqLength-1) <= length(lniSequence)) {
    j <- i+(seqLength-1)
    k <- i+(seqLength)
    currInteraction <- lniSequence[i:j]
    
    # skip if there is a "-1" in the current interaction
    skip <- any(currInteraction == -1)
    
    # skip if there are duplicates in any triplet
    # e.g. current interaction of 8 actions = 2 2/3 triplets: "123|231|21"
    # this is to make sure that for higher order interactions (h3+) lower order 
    # instances (e.g., 111|111|11) are not considered as high order instance
    currInteractionCopy <- currInteraction
    while (!skip && length(currInteractionCopy) > 0) {
      lowerBound <- 1
      upperBound <- 2
      
      # compare each element of current triplet for equality
      while (!skip && lowerBound < 3 && lowerBound < length(currInteractionCopy)) {
        while (!skip && upperBound < 4 && upperBound <= length(currInteractionCopy)) {
          
          if (currInteractionCopy[lowerBound] == currInteractionCopy[upperBound]) {
            skip <- TRUE
          } 
          upperBound <- upperBound+1
        }
        lowerBound <- lowerBound+1
        upperBound <- lowerBound+1
      }
      
      # switch to next triplet
      if (length(currInteractionCopy) > 3) {
        currInteractionCopy <- tail(currInteractionCopy, length(currInteractionCopy)-3)
      } else {
        currInteractionCopy <- c()
      }
    }
    
    if (skip) {
      i <- i+1      
      j <- j+1
      k <- k+1
      next
    }
    
    seqLengthCopy <- seqLength
    alternatingIndex <- 0
    while (k <= length(lniSequence)
           & lniSequence[k] == currInteraction[(alternatingIndex%%seqLength)+1]) {
      seqLengthCopy <- seqLengthCopy+1
      k <- k+1
      alternatingIndex <- alternatingIndex+1
    }
    if (!higherOrder || (higherOrder && seqLengthCopy > seqLength)) {
      sequences <- c(sequences, seqLengthCopy)
    }
    
    if ((k-1) <= i) {
      i <- i+1
    } else {
      i <- k-1
    }
  }

  sequences <- sequences[sequences >= 3]
  return(100 * sum(sequences) / length(lniSequence))
}


########################################## PLOTS / EXPORTS ###########################################
#----------------------------------------------------------------------------------------------------#
# function: plotConvergencePattern
#   Plots the pattern for the given convergence data.
#   TODOs:
#       - generalize code (this is ugly as fuck)
#   param:  convergenceData
#       the convergence data to plot
#----------------------------------------------------------------------------------------------------#
plotConvergencePattern <- function(convergenceData, currentPlot = 1, overallPlots = 1) {
  
  #rownames(convergenceData) <- seq(length=nrow(convergenceData))
  
  h1Patterns <- data.frame("which" = numeric(1), "h1" = numeric(1))
  if (length(which(convergenceData[,1] == 0)) < length(convergenceData$h1)) {
    h1Patterns <- data.frame(which(convergenceData[,1] == 1), 1)
  }
  h2Patterns <- data.frame("which" = numeric(1), "h2" = numeric(1))
  if (length(which(convergenceData[,2] == 0)) < length(convergenceData$h2)) {
    h2Patterns <- data.frame(which(convergenceData[,2] == 1), 2)
  }
  h3Patterns <- data.frame("which" = numeric(1), "h3" = numeric(1))
  if (length(which(convergenceData[,3] == 0)) < length(convergenceData$h3)) {
    h3Patterns <- data.frame(which(convergenceData[,3] == 1), 3)
  }
  h4Patterns <- data.frame("which" = numeric(1), "h4" = numeric(1))
  if (length(which(convergenceData[,4] == 0)) < length(convergenceData$h4)) {
    h4Patterns <- data.frame(which(convergenceData[,4] == 1), 4)
  }
  h5Patterns <- data.frame("which" = numeric(1), "h5" = numeric(1))
  if (length(which(convergenceData[,5] == 0)) < length(convergenceData$h5)) {
    h5Patterns <- data.frame(which(convergenceData[,5] == 1), 5)
  }
  h6Patterns <- data.frame("which" = numeric(1), "h6" = numeric(1))
  if (length(which(convergenceData[,6] == 0)) < length(convergenceData$h6)) {
    h6Patterns <- data.frame(which(convergenceData[,6] == 1), 6)
  }
  h7Patterns <- data.frame("which" = numeric(1), "h7" = numeric(1))
  if (length(which(convergenceData[,7] == 0)) < length(convergenceData$h7)) {
    h7Patterns <- data.frame(which(convergenceData[,7] == 1), 7)
  }
  h8Patterns <- data.frame("which" = numeric(1), "h8" = numeric(1))
  if (length(which(convergenceData[,8] == 0)) < length(convergenceData$h8)) {
    h8Patterns <- data.frame(which(convergenceData[,8] == 1), 8)
  }
  h9Patterns <- data.frame("which" = numeric(1), "h9" = numeric(1))
  if (length(which(convergenceData[,9] == 0)) < length(convergenceData$h9)) {
    h9Patterns <- data.frame(which(convergenceData[,9] == 1), 9)
  }
  hmin1Patterns <- data.frame("which" = numeric(1), "h-1" = numeric(1))
  if (length(which(convergenceData[,10] == 0)) < length(convergenceData$hmin1)) {
    hmin1Patterns <- data.frame(which(convergenceData[,10] == 1), 10)
  }
  othersPatterns <- data.frame("which" = numeric(1), "others" = numeric(1))
  if (length(which(convergenceData[,11] == 0)) < length(convergenceData$others)) {
    othersPatterns <- data.frame(which(convergenceData[,11] == 1), 11)
  }
  
  
  # basic plot - showing -1 pattern
  plot(hmin1Patterns, ann=FALSE, xaxt = "n", yaxt = "n", 
       type = 'p', pch = 20,
       xlim = range(1:nrow(convergenceData)), ylim = range(0.5:(ncol(convergenceData)+0.5)))
  # y-axis per plot
  axis(side=2,
       at=seq(1,ncol(convergenceData),1),
       labels=c("-1","h1","h2","h3","h4","h5","h6","h7","h8","h9","others"), 
       cex.axis = 0.7)
  
  # adding other convergence patterns
  points(h1Patterns, type = 'p', pch = 20)
  points(h2Patterns, type = 'p', pch = 20)
  points(h3Patterns, type = 'p', pch = 20)
  points(h4Patterns, type = 'p', pch = 20)
  points(h5Patterns, type = 'p', pch = 20)
  points(h6Patterns, type = 'p', pch = 20)
  points(h7Patterns, type = 'p', pch = 20)
  points(h8Patterns, type = 'p', pch = 20)
  points(h9Patterns, type = 'p', pch = 20)
  points(othersPatterns, type = 'p', pch = 20)
  
  if (currentPlot == overallPlots) {
    # overall x-axis
    mtext(side=1,"Period",line=2.5)       
    axis(side=1,
         at=seq(0,nrow(convergenceData), by=(nrow(convergenceData)/10)),
         labels=seq(0,nrow(convergenceData), by=(nrow(convergenceData)/10)), 
         cex.axis = 0.7)
    # overall y-axis label
    mtext('LNI-order of prevalent behavioral pattern', side = 2, 
          outer = TRUE, line = -1.8)
  }
}







#----------------------------------------------------------------------------------------------------#
# function: plotInteractionPatterns
#   Orchestrates plotting of multiple interaction patterns into one plot
#   param:  vodData
#       the VOD data to plot
#----------------------------------------------------------------------------------------------------#
plotInteractionPatterns <- function(vodData) {
  
  # setting up multiple plots
  plotsPerImage <- 10
  par(mfrow=c(plotsPerImage,1),oma=c(3,0,0,0), mai = c(0.1, 0.6, 0.1, 0.2))
  
  # looping over the available VODs
  for (i in 1:length(vodData)) {
    singleVodData <- vodData[[i]]
    plotInteractionPattern(singleVodData, i, length(vodData))
  }
}

#----------------------------------------------------------------------------------------------------#
# function: plotInteractionPattern
#   Plots the interaction pattern for the given VOD data.
#   TODOs:
#       - generalize code (this is ugly as fuck)
#   param:  vodData
#       the VOD data to plot
#   param:  cut
#       flag denoting whether plots need to be cut (max. 150 rounds)
#----------------------------------------------------------------------------------------------------#
plotInteractionPattern <- function(vodData, currentPlot = 1, overallPlots = 1) {
  
  # only columns of players' actions
  vodData <- vodData[2:length(vodData$round),2:4]
  
  xAtSeq <- seq(0,nrow(vodData),1)
  xLabels <- seq(0,nrow(vodData),1)
  
  # only 150 (35 first + 5 gap + 110 last) rounds in total
  if (nrow(vodData) > PLOT_MAX_ROUNDS) {
    rounds <- nrow(vodData)
    
    gapRows <- PRE_GAP_LENGTH:(PRE_GAP_LENGTH + GAP_LENGTH)
    vodData[gapRows,] <- 0
    
    cutRows <- (PRE_GAP_LENGTH + GAP_LENGTH + 1):(rounds - (POST_GAP_LENGTH + 1))
    vodData <- vodData[-cutRows,]
    
    xAtSeq <- seq(0, nrow(vodData), LABEL_STEP_SIZE)
    xLabels <- c(seq(0, PRE_GAP_LENGTH, by = LABEL_STEP_SIZE), 
                 "...", seq(rounds-POST_GAP_LENGTH, rounds, by = 5))
  }
  
  rownames(vodData) <- seq(length=nrow(vodData))
  
  p1Cooperations <- data.frame("which" = numeric(1), "X1" = numeric(1))
  if (length(which(vodData[,1] == 0)) < length(vodData$player1)) {
    p1Cooperations <- data.frame(which(vodData[,1] == 1), 1)
  }
  p2Cooperations <- data.frame("which" = numeric(1), "X2" = numeric(1))
  if (length(which(vodData[,2] == 0)) < length(vodData$player2)) {
    p2Cooperations <- data.frame(which(vodData[,2] == 1), 2)
  }
  p3Cooperations <- data.frame("which" = numeric(1), "X3" = numeric(1))
  if (length(which(vodData[,3] == 0)) < length(vodData$player3)) {
    p3Cooperations <- data.frame(which(vodData[,3] == 1), 3)
  }
  
  # basic plot - including cooperation data for player 1
  plot(p1Cooperations, ann=FALSE, xaxt = "n", yaxt = "n", 
       type = 'p', pch = 'x',
       xlim = range(1:nrow(vodData)), ylim = range(0.5:3.5))
  # y-axis per plot
  axis(side=2,at=seq(0,3,1),labels=seq(0,3,1), cex.axis = 0.7)
  # adding cooperation data for players 2 and 3
  points(p2Cooperations, type = 'p', pch = 'x')
  points(p3Cooperations, type = 'p', pch = 'x')
  # adding horizontal lines
  segments(x0 = 0, x1 = nrow(vodData), y0 = c(1,2,3), col = "gray60")
  
  if (currentPlot == overallPlots) {
    # overall x-axis
    mtext(side=1,"Period",line=2.5)
    axis(side=1, at=xAtSeq, labels=xLabels, cex.axis = 0.7)  
    # overall y-axis label
    mtext('Group members\' decision across sessions (x = \'cooperation\')', side = 2, 
          outer = TRUE, line = -1.8)
  }
}

#----------------------------------------------------------------------------------------------------#
# function: exportInteractionPatterns
#   Exports the interaction patterns for the given VOD data.
#   param:  directory
#       the storage directory
#   param:  vodType
#       the type of VOD (e.g., sym, asym1, asym2)
#   param:  vodData
#       the VOD data to export
#----------------------------------------------------------------------------------------------------#
exportInteractionPatterns <- function(directory, vodType, vodData) {
  
  fileNumber <- 1
  while (length(vodData) > 0) {
    maxLength <- if(length(vodData) > 10) 10 else length(vodData)
    filename <- paste(directory, vodType, "-interaction-patterns-", fileNumber, ".png", sep = "") 

    png(filename,
        width = 1900, 
        height = 1200, 
        units = "px", 
        res = 196)
    plotInteractionPatterns(vodData[1:maxLength])
    dev.off() 
    
    vodData[1:maxLength] <- NULL
    fileNumber <- fileNumber+1
  }
}

#----------------------------------------------------------------------------------------------------#
# function: plotGOF
#   Plots the goodness of fit between the mean LNIs of a simulation and the mean LNIs of 
#   experiment 1 in Diekmann & Przepiorka (2016).
#   param:  meanLNIs
#       the mean LNIs of the simulation
#----------------------------------------------------------------------------------------------------#
plotGOF <- function(meanLNIs) {
  
  # three diagrams - one per VOD type (sym, asym1, asym2)
  par(mfrow=c(1,3), oma = c(0, 4, 3, 0), mar = c(5, 2, 1, 1))
  cols <- c("gray", "black")
  
  # binding of simulation and experimental data
  LNIs <- rbind(meanLNIs, LNIS_EXP1)
  
  # compare model and experimental data: Symmetric
  plotDataSym1 <- data.frame(h1 = LNIs$sym_h1,
                         h2 = LNIs$sym_h2,
                         h3 = LNIs$sym_h3,
                         others = LNIs$sym_others)
  barplot(as.matrix(plotDataSym1), xlab = "Symmetric", 
          beside = TRUE, col = cols, ylim = range(0:100))
  
  # compare model and experimental data: Asymmetric 1
  plotDataAsym1 <- data.frame(h1 = LNIs$asym1_h1,
                         h2 = LNIs$asym1_h2,
                         h3 = LNIs$asym1_h3,
                         others = LNIs$asym1_others)
  barplot(as.matrix(plotDataAsym1), yaxt = "n", xlab = "Asymmetric 1", 
          beside = TRUE, col = cols, ylim = range(0:100))
  
  # compare model and experimental data: Asymmetric 2
  plotDataAsym2 <- data.frame(h1 = LNIs$asym2_h1,
                              h2 = LNIs$asym2_h2,
                              h3 = LNIs$asym2_h3,
                              others = LNIs$asym2_others)
  barplot(as.matrix(plotDataAsym2), yaxt = "n", xlab = "Asymmetric 2", 
          beside = TRUE, col = cols, ylim = range(0:100))

  # GOF values
  library(hydroGOF)
  RMSE <- rmse(as.numeric(meanLNIs), as.numeric(LNIS_EXP1))
  NRMSE <- nrmse(as.numeric(meanLNIs), as.numeric(LNIS_EXP1))
  RSQ <- summary(lm(as.numeric(LNIS_EXP1) ~ as.numeric(meanLNIs)))$r.squared
  
  # legend and title
  legend(x = "topright", y = 10, c("Model","Diekmann & Przepiorka (2016)"), cex=0.7, fill=cols)
  title(bquote(paste("Model Data vs. Experimental Data (RMSE = ", .(round(RMSE, digits = 2)), 
              ", NRMSE = ", .(round(NRMSE, digits = 2)), "%, ", 
              R^2, " = ", .(round(RSQ, digits = 2)), ")", sep = "")), 
        outer=TRUE)
  mtext('average LNI', side = 2, outer = TRUE, line = 1.5, cex = 0.7)

  # return(data.frame(RMSE, NRMSE, RSQ))
}

#----------------------------------------------------------------------------------------------------#
# function: exportGOF
#   Exports the goodness of fit between the mean LNIs of a simulation and the mean LNIs of 
#   experiment 1 in Diekmann & Przepiorka (2016).
#   param:  directory
#       the storage directory
#   param:  meanLNIs
#       the mean LNIs of the simulation
#----------------------------------------------------------------------------------------------------#
exportGOF <- function(directory, meanLNIs) {
  png(paste(directory, "gof.png", sep = ""), 
      width = 1200, 
      height = 700, 
      units = "px", 
      res = 196)
  gofs <- plotGOF(meanLNIs)
  dev.off()  
}

#----------------------------------------------------------------------------------------------------#
# function: exportLNIComparison
#   Exports the table showing the direct comparison between the mean LNIs of a simulation and 
#   the mean LNIs of experiment 1 in Diekmann & Przepiorka (2016).
#   param:  directory
#       the storage directory
#   param:  meanLNIs
#       the mean LNIs of the simulation
#----------------------------------------------------------------------------------------------------#
exportLNIComparison <- function(directory, meanLNIs) {
  comparison <- rbind(meanLNIs, LNIS_EXP1)
  comparison$source <- c("model", "experiment")
  write.csv(comparison, file = paste(directory, "model-vs-experiment.csv", sep = ""))
}


############################################ COMPOSITIONS ############################################
#----------------------------------------------------------------------------------------------------#
# function: analyzeData
#     Starting point for the data analysis.
#     param:  modelType
#         the model type used by the simulation
#         For available for modelTypes, see constants.R - 'MODEL_TYPES'
#     param:  simCount
#         defines the number of the performed simulation
#         (if undefined: latest available simulation)
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: 'all', constants.R: 'VOD_TYPES[x]'
#----------------------------------------------------------------------------------------------------#
analyzeData <- function(modelType = MODEL_TYPES[2],
                        simCount = "latest",
                        vodType = "all",
                        fit = FALSE, 
                        fitCSV = NA) {
  
  # initializations
  exportDir <- paste(getVodBaseDir(modelType = modelType, 
                                   simCount = simCount),
                     "/", sep = "")
  if (vodType == "all") {
    vodType <- VOD_TYPES
  }
  LNIs <- data.frame()
  
  # looping over the VOD types to be analyzed (e.g.: sym -> asym1 -> asym2)
  comparableKeeps <- c()
  for (i in 1:length(vodType)) {
    currVodType <- vodType[i]
    
    # importing the required simulated data
    vodSimData <- importVodSimData(modelType = modelType,
                                   simCount = simCount, 
                                   vodType = currVodType)

    # exporting interaction patterns
    exportInteractionPatterns(exportDir, currVodType, vodSimData)
    
    # computation of LNIs per VOD type (e.g., sym, asym1, asym2)
    vodTypeLNIs <- data.frame()
    for (i in 1:length(vodSimData)) {
      lniSequence <- extractLNISequence(vodSimData[[i]])
      vodTypeLNIs <- rbind(vodTypeLNIs, computeLNIs(lniSequence))
    }
    colnames(vodTypeLNIs) <- c((paste(currVodType, "_h1", sep = "")),
                               (paste(currVodType, "_h2", sep = "")),
                               (paste(currVodType, "_h3", sep = "")),
                               (paste(currVodType, "_others", sep = "")),
                               (paste(currVodType, "_h4", sep = "")),
                               (paste(currVodType, "_h5", sep = "")),
                               (paste(currVodType, "_h6", sep = "")),
                               (paste(currVodType, "_h7", sep = "")),
                               (paste(currVodType, "_h8", sep = "")),
                               (paste(currVodType, "_h9", sep = "")),
                               (paste(currVodType, "_h_minus1", sep = "")))
    
    comparableKeeps <- c(comparableKeeps,
                         (paste(currVodType, "_h1", sep = "")),
                         (paste(currVodType, "_h2", sep = "")),
                         (paste(currVodType, "_h3", sep = "")),
                         (paste(currVodType, "_others", sep = "")))
    
    if (nrow(LNIs) == 0) {
      LNIs <- vodTypeLNIs
    } else {
      LNIs <- cbind(LNIs, vodTypeLNIs)
    }
  }
  
  # computation of mean LNIs
  meanLNIs <- apply(LNIs, 2, median)

  # exporting Goodness of Fit
  exportGOF(exportDir, meanLNIs[comparableKeeps])
  
  # exporting LNI comparison data (model vs. experiment)
  exportLNIComparison(exportDir, meanLNIs[comparableKeeps])

  
  if (fit) {
    
    # read model params
    vodBaseDir <- getVodBaseDir(modelType, simCount)
    
    modelParams <- read.csv(paste(vodBaseDir, "/model-params.csv", sep = ""), header = T, sep = ",")
    
    library(hydroGOF)

    symRMSE <- rmse(as.numeric(meanLNIs[comparableKeeps][1:4]), as.numeric(LNIS_EXP1[1:4]))
    symNRMSE <- nrmse(as.numeric(meanLNIs[comparableKeeps][1:4]), as.numeric(LNIS_EXP1[1:4]))
    symRSQ <- summary(lm(as.numeric(LNIS_EXP1[1:4]) ~ as.numeric(meanLNIs[comparableKeeps][1:4])))$r.squared
    
    asym1RMSE <- rmse(as.numeric(meanLNIs[comparableKeeps][5:8]), as.numeric(LNIS_EXP1[5:8]))
    asym1NRMSE <- nrmse(as.numeric(meanLNIs[comparableKeeps][5:8]), as.numeric(LNIS_EXP1[5:8]))
    asym1RSQ <- summary(lm(as.numeric(LNIS_EXP1[5:8]) ~ as.numeric(meanLNIs[comparableKeeps][5:8])))$r.squared
    
    asym2RMSE <- rmse(as.numeric(meanLNIs[comparableKeeps][9:12]), as.numeric(LNIS_EXP1[9:12]))
    asym2NRMSE <- nrmse(as.numeric(meanLNIs[comparableKeeps][9:12]), as.numeric(LNIS_EXP1[9:12]))
    asym2RSQ <- summary(lm(as.numeric(LNIS_EXP1[9:12]) ~ as.numeric(meanLNIs[comparableKeeps][9:12])))$r.squared
    
    RMSE <- rmse(as.numeric(meanLNIs[comparableKeeps]), as.numeric(LNIS_EXP1))
    NRMSE <- nrmse(as.numeric(meanLNIs[comparableKeeps]), as.numeric(LNIS_EXP1))
    RSQ <- summary(lm(as.numeric(LNIS_EXP1) ~ as.numeric(meanLNIs[comparableKeeps])))$r.squared
    
    GOF_per_vod_type <- c("###", "###", "###")
    RMSE_per_vod_type <- c(symRMSE, asym1RMSE, asym2RMSE)
    NRMSE_per_vod_type <- c(symNRMSE, asym1NRMSE, asym2NRMSE)
    RSQ_per_vod_type <- c(symRSQ, asym1RSQ, asym2RSQ)
    GOF_combined <- c("#", "#", "#")
    RMSE_combined <- c(RMSE, RMSE, RMSE)
    NRMSE_combined <- c(NRMSE, NRMSE, NRMSE)
    RSQ_combined <- c(RSQ, RSQ, RSQ)
    vod_params <- c("###", "###", "###")
    gofs <- data.frame(GOF_per_vod_type, RMSE_per_vod_type, NRMSE_per_vod_type, RSQ_per_vod_type, 
                       GOF_combined, RMSE_combined, NRMSE_combined, RSQ_combined, vod_params)
    
    library(dplyr)
    fitInfo <- cbind(gofs, modelParams[2:ncol(modelParams)]) %>% select(model_type, vod_type, everything())
    
    # adding simulation count (folder) and timestamp
    simCount <- getLatestSimCount(modelType)
    simCount <- c(simCount, simCount, simCount)
    timestamp <- as.character(Sys.time())
    timestamp <- c(timestamp, timestamp, timestamp)
    fitInfo <- cbind(simCount, timestamp, fitInfo)
    
    if (!file.exists(fitCSV)) {
      write.csv(fitInfo, file = fitCSV)
    } else {
      ff <- file(fitCSV, open="at")
      write.table(fitInfo, file=ff, sep = ",", col.names = F)
      close(ff)
    }
      
  }
  
}


############################################### TESTS ################################################
#----------------------------------------------------------------------------------------------------#
# function: testPlots
#     Test for plotting behavioral and convergence patterns.
#----------------------------------------------------------------------------------------------------#
testPlots <- function() {
  
  ####################################### DATA #######################################
  ### data from simulated VODs ###
  ################################
  modelType <- "CoordinateXEpsilonNoise"
  date <- "20170316"
  simCnt <- "1"
  vodType <- "sym"
  fileNumber <- 6
  file <- paste("sim-", fileNumber, ".Rdata", sep = "")
  filePath <- paste(SIM_DIR, "/", modelType, "/", date, "/", simCnt, "/", vodType,
                    "/", file, sep = "")
  vodSimData <- get(load(filePath))
  ################################
  ######## mock VOD data #########
  ################################
  round <- 0:150000
  player1 <- sample(c(0,1), 150001, replace = TRUE)
  player2 <- sample(c(0,1), 150001, replace = TRUE)
  player3 <- sample(c(0,1), 150001, replace = TRUE)
  vodMockData <- data.frame(round, player1, player2, player3)
  ################################
  
  ####################################### PLOTS #######################################
  ##### behavioral patterns ######
  ################################
  quartz()
  plotInteractionPattern(vodSimData)
  ################################
  ##### behavioral patterns ######
  ################################
  lniSequence <- extractLNISequence(vodSimData)
  lnis <- computeLNIs(lniSequence)
  convergencePatterns <- computeConvergencePatterns(lniSequence)
  quartz()  
  plotConvergencePattern(convergencePatterns)
  
}


importFitData <- function() {
  
  data <- rbind(read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170609-classicQ-fit.csv"),
                read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170610-classicQ-fit.csv"),
                read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170611-classicQ-fit.csv"))
  
  symData <- data[data$vod_type == "sym", ]  
  mean(symData$RMSE_per_vod_type)
  
}


###################################### EVENTUAL INTEGRITY CHECKS #####################################
source(paste(BASE_DIR, "/tests/testAnalysis.R", sep = ""))
