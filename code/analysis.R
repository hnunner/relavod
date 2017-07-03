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
  # h4 <- computeConvergencePattern(4, lniSequence)
  # h5 <- computeConvergencePattern(5, lniSequence)
  # h6 <- computeConvergencePattern(6, lniSequence)
  # h7 <- computeConvergencePattern(7, lniSequence)
  # h8 <- computeConvergencePattern(8, lniSequence)
  # h9 <- computeConvergencePattern(9, lniSequence)
  # 
  # # everything that's not falling under any of the categories above
  # others <- as.numeric(!(min1s|h1|h2|h3|h4|h5|h6|h7|h8|h9))
  # 
  # res <- data.frame(min1s, h1, h2, h3, h4, h5, h6, h7, h8, h9, others)
  
  others <- as.numeric(!(min1s|h1|h2|h3))
  res <- data.frame(min1s, h1, h2, h3, others)
  
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
plotConvergencePatterns <- function(convergencePatterns) {
  
  # setting up multiple plots
  plotsPerImage <- 10
  par(mfrow=c(plotsPerImage,1),oma=c(3,0,0,0), mai = c(0.1, 0.6, 0.1, 0.2))
  
  # looping over the available VODs
  for (i in 1:length(convergencePatterns)) {
    convergencePattern <- convergencePatterns[[i]]
    plotConvergencePattern(convergencePattern, i, length(convergencePatterns))
  }
}

exportConvergencePatterns <- function(directory, vodType, convergencePatterns) {
  fileNumber <- 1
  while (length(convergencePatterns) > 0) {
    maxLength <- if(length(convergencePatterns) > 10) 10 else length(convergencePatterns)
    filename <- paste(directory, vodType, "-convergence-patterns-", fileNumber, ".png", sep = "") 
    
    png(filename,
        width = 1900, 
        height = 2000, 
        units = "px", 
        res = 196)
    plotConvergencePatterns(convergencePatterns[1:maxLength])
    dev.off() 
    
    convergencePatterns[1:maxLength] <- NULL
    fileNumber <- fileNumber+1
  }
}


#----------------------------------------------------------------------------------------------------#
# function: plotConvergencePattern
#   Plots the pattern for the given convergence data.
#   TODOs:
#       - generalize code (this is ugly as fuck)
#   param:  convergenceData
#       the convergence data to plot
#----------------------------------------------------------------------------------------------------#
plotConvergencePattern <- function(convergenceData, currentPlot = 1, overallPlots = 1) {
  
  hmin1Patterns <- data.frame("which" = numeric(1), "h-1" = numeric(1))
  if (length(which(convergenceData[,1] == 0)) < length(convergenceData$min1s)) {
    hmin1Patterns <- data.frame(which(convergenceData[,1] == 1), 5)
  }
  h1Patterns <- data.frame("which" = numeric(1), "h1" = numeric(1))
  if (length(which(convergenceData[,2] == 0)) < length(convergenceData$h1)) {
    h1Patterns <- data.frame(which(convergenceData[,2] == 1), 1)
  }
  h2Patterns <- data.frame("which" = numeric(1), "h2" = numeric(1))
  if (length(which(convergenceData[,3] == 0)) < length(convergenceData$h2)) {
    h2Patterns <- data.frame(which(convergenceData[,3] == 1), 2)
  }
  h3Patterns <- data.frame("which" = numeric(1), "h3" = numeric(1))
  if (length(which(convergenceData[,4] == 0)) < length(convergenceData$h3)) {
    h3Patterns <- data.frame(which(convergenceData[,4] == 1), 3)
  }
  # h4Patterns <- data.frame("which" = numeric(1), "h4" = numeric(1))
  # if (length(which(convergenceData[,5] == 0)) < length(convergenceData$h4)) {
  #   h4Patterns <- data.frame(which(convergenceData[,5] == 1), 5)
  # }
  # h5Patterns <- data.frame("which" = numeric(1), "h5" = numeric(1))
  # if (length(which(convergenceData[,6] == 0)) < length(convergenceData$h5)) {
  #   h5Patterns <- data.frame(which(convergenceData[,6] == 1), 6)
  # }
  # h6Patterns <- data.frame("which" = numeric(1), "h6" = numeric(1))
  # if (length(which(convergenceData[,7] == 0)) < length(convergenceData$h6)) {
  #   h6Patterns <- data.frame(which(convergenceData[,7] == 1), 7)
  # }
  # h7Patterns <- data.frame("which" = numeric(1), "h7" = numeric(1))
  # if (length(which(convergenceData[,8] == 0)) < length(convergenceData$h7)) {
  #   h7Patterns <- data.frame(which(convergenceData[,8] == 1), 8)
  # }
  # h8Patterns <- data.frame("which" = numeric(1), "h8" = numeric(1))
  # if (length(which(convergenceData[,9] == 0)) < length(convergenceData$h8)) {
  #   h8Patterns <- data.frame(which(convergenceData[,9] == 1), 9)
  # }
  # h9Patterns <- data.frame("which" = numeric(1), "h9" = numeric(1))
  # if (length(which(convergenceData[,10] == 0)) < length(convergenceData$h9)) {
  #   h9Patterns <- data.frame(which(convergenceData[,10] == 1), 10)
  # }
  # othersPatterns <- data.frame("which" = numeric(1), "others" = numeric(1))
  # if (length(which(convergenceData[,11] == 0)) < length(convergenceData$others)) {
  #   othersPatterns <- data.frame(which(convergenceData[,11] == 1), 11)
  # }
  
  othersPatterns <- data.frame("which" = numeric(1), "others" = numeric(1))
  if (length(which(convergenceData[,5] == 0)) < length(convergenceData$others)) {
    othersPatterns <- data.frame(which(convergenceData[,5] == 1), 4)
  }
  
  
  # basic plot - showing -1 pattern
  plot(hmin1Patterns, ann=FALSE, 
       xaxt = "n", 
       yaxt = "n", 
       type = 'p', pch = 20,
       xlim = range(1:nrow(convergenceData)), ylim = range(0.5:
                                                             (5+0.5)))
                                                             #(ncol(convergenceData)+0.5)))
  
  # x-axis
  xAtSeq <- seq(0,nrow(convergenceData),by=5)
  xLabels <- seq(0,nrow(convergenceData),by=5)
  xLal <- 2
  
  # only 150 (35 first + 5 gap + 110 last) rounds in total
  if (nrow(convergenceData) > PLOT_MAX_ROUNDS) {
    xAtSeq <- seq(0,nrow(convergenceData),by=250)
    xLabels <- c("0", "", "500","","1.000","","1.500","","2.000","","2.500",
                 "","3.000","","3.500","","4.000","","4.500","","5.000")
    xLas <- 1
  }
  
  
  axis(side=1, at=xAtSeq, labels=xLabels, cex.axis = 0.6, las = xLas)
  mtext('Round', side = 1, outer = TRUE, line = 1.6, cex = 0.8)
  
  # y-axis per plot
  axis(side=2,
       at=seq(1,
              5,1),
              #ncol(convergenceData),1),
       #labels=c("-1","h1","h2","h3","h4","h5","h6","h7","h8","h9","others"), 
       labels=c("solitary volunteer.","turn-taking (2)","turn-taking (3)","other optimum", "sub-optimal"), 
       cex.axis = 0.6, las = 1)
  
  
  # adding other convergence patterns
  points(h1Patterns, type = 'p', pch = 20)
  points(h2Patterns, type = 'p', pch = 20)
  points(h3Patterns, type = 'p', pch = 20)
  # points(h4Patterns, type = 'p', pch = 20)
  # points(h5Patterns, type = 'p', pch = 20)
  # points(h6Patterns, type = 'p', pch = 20)
  # points(h7Patterns, type = 'p', pch = 20)
  # points(h8Patterns, type = 'p', pch = 20)
  # points(h9Patterns, type = 'p', pch = 20)
  points(othersPatterns, type = 'p', pch = 20)
  
  # lines 
  segments(x0 = 0, x1 = nrow(convergenceData), y0 = c(1,2,3,4,5), col = "gray60")
  
  if (currentPlot == overallPlots) {
    # overall x-axis
    #mtext(side=1,"Period",line=2.5)       
    # axis(side=1,
    #      at=seq(0,nrow(convergenceData), 
    #             by=5),
    #             #by=(nrow(convergenceData)/10)),
    #      labels="",
    #        #seq(0,nrow(convergenceData),by=5),
    #                 #by=(nrow(convergenceData)/10)), 
    #      cex.axis = 0.7)
    # 
    # overall y-axis label
    #mtext('LNI-order of prevalent behavioral pattern', side = 2, 
    #      outer = TRUE, line = -1.8)
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
  axis(side=2,at=seq(1,3,1),labels=c("coop. player 1", "coop. player 2", "coop. player 3"), cex.axis = 0.6, las = 1)
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
analyzeData <- function(modelType = MODEL_TYPES[3],
                        simCount = 1961,
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
  
  
  filePath <- "/Users/hendrik/Desktop/thesis-patterns/CoordinateX/1983/asym2/sim-6.Rdata"
  
  
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
  ##### convergence patterns #####
  ################################
  
  importDir <- "/Users/hendrik/Desktop/thesis-patterns/CoordinateX/2807/asym2/"
  exportDir <- "/Users/hendrik/Desktop/thesis-patterns/CoordinateX/2807/"
  simCountFiles <- list.files(importDir, recursive = FALSE)
  
  vodSimData <- list()
  for (i in 1:(length(simCountFiles))) {
    filename <- paste(importDir, "/", BASE_FILENAME, i, ".Rdata", sep = "")
    vodSimData[[i]] <- get(load(filename))
  }  
  
  convergencePatterns <- list()
  for (i in 1:(length(vodSimData))) {
    lniSequence <- extractLNISequence(vodSimData[[i]])
    lnis <- computeLNIs(lniSequence)
    convergencePatterns[[i]] <- computeConvergencePatterns(lniSequence)
  }
  
  exportConvergencePatterns(exportDir, "asym2", convergencePatterns)
}


importFitData <- function() {
  
  randomData <- read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170609-random-fit.csv")
  
  coordinateXData <- read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170611-coordinateX-fit.csv")
  
  classicQData <- rbind(read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170609-classicQ-fit.csv"),
                        read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170610-classicQ-fit.csv"),
                        read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170611-classicQ-fit.csv"))
  
  symData <- classicQData[data$vod_type == "sym", ]  
  mean(symData$RMSE_per_vod_type)
  
}


############################################ FIT ANALYIS #############################################
getMedians <- function(data, type) {
  
  symData <- data[data$vod_type == "sym", ]
  asym1Data <- data[data$vod_type == "asym1", ]
  asym2Data <- data[data$vod_type == "asym2", ]
  
  # symmetric
  print(paste("Symmetric - RMSE:",
              median(symData$RMSE_per_vod_type),
              "| NRMSE:",
              median(symData$NRMSE_per_vod_type),
              "| R2:",
              median(symData$RSQ_per_vod_type)))
  
  # asymmetric 1
  print(paste("Asymmetric 1 - RMSE:",
              median(asym1Data$RMSE_per_vod_type),
              "| NRMSE:",
              median(asym1Data$NRMSE_per_vod_type),
              "| R2:",
              median(asym1Data$RSQ_per_vod_type)))
  
  # asymmetric 2
  print(paste("Asymmetric 2 - RMSE:",
              median(asym2Data$RMSE_per_vod_type),
              "| NRMSE:",
              median(asym2Data$NRMSE_per_vod_type),
              "| R2:",
              median(asym2Data$RSQ_per_vod_type)))
  
  # combined
  print(paste("Combined - RMSE:",
              median(data$RMSE_combined),
              "| NRMSE:",
              median(data$NRMSE_combined),
              "| R2:",
              median(data$RSQ_combined)))
  
  print(
    cat(
      "\n\\parbox[c][0.65cm]{2.5cm}{",
      type,
      "} & \\parbox[c][0.65cm]{1.45cm}{\\centering $", 
      round(median(symData$RMSE_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(symData$NRMSE_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(symData$RSQ_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(asym1Data$RMSE_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(asym1Data$NRMSE_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(asym1Data$RSQ_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(asym2Data$RMSE_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(asym2Data$NRMSE_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $",
      round(median(asym2Data$RSQ_per_vod_type), digits = 2),
      "$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $\\mathbf{",
      round(median(data$RMSE_combined), digits = 2),
      "}$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $\\mathbf{",
      round(median(data$NRMSE_combined), digits = 2),
      "}$} & \\parbox[c][0.65cm]{1.45cm}{\\centering $\\mathbf{",
      round(median(data$RSQ_combined), digits = 2),
      "}$} \\\\ ", sep = ""
    )
  )
}


analyzeRandomFits <- function() {
  rData <- read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170609-random-fit.csv")
  getMedians(rData[rData$simCount == 2,], "Random")   # cooperation ratio = 1/3
}

analyzeClassicQFits <- function() {
  
  # all data
  cQData <- rbind(read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170609-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170610-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170611-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170615-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170616-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170617-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170618-classicQ-fit.csv"))
  
  # overall results
  # best fit
  getMedians(cQData[cQData$RMSE_combined == min(cQData$RMSE_combined),], "Best Fit")
  # average fit
  getMedians(cQData, "$\\varnothing$ Fit")
  
  # balancing
  getMedians(cQData[cQData$p1_balancing == "greedy" & cQData$p1_epsilon_decay == 1, ], "$\\varnothing$ $\\epsilon$-greedy")
  getMedians(cQData[cQData$p1_balancing == "greedy" & cQData$p1_epsilon_decay < 1, ], "$\\varnothing$ $\\epsilon$-decreasing")
  getMedians(cQData[cQData$p1_balancing == "noise" & cQData$p1_epsilon_decay == 1, ], "$\\varnothing$ $\\epsilon$-noise")
  getMedians(cQData[cQData$p1_balancing == "noise" & cQData$p1_epsilon_decay < 1, ], "$\\varnothing$ $\\epsilon$-noise-decreasing")
  
  # social preferences
  getMedians(cQData[cQData$p1_social_behavior == "selfish", ], "$\\varnothing$ Selfish")
  getMedians(cQData[cQData$p1_social_behavior == "altruistic", ], "$\\varnothing$ Altruistic")
  
  # initial props
  getMedians(cQData[cQData$p1_prop_start >= 100, ], "$\\varnothing$ Optimistic")
  getMedians(cQData[cQData$p1_prop_start < 100, ], "$\\varnothing$ Pessimistic")
  
  # learning rate
  getMedians(cQData[cQData$p1_alpha <= 0.3, ], "$\\varnothing$ Slow")
  getMedians(cQData[cQData$p1_alpha > 0.3, ], "$\\varnothing$ Fast")
  
  # discount rate
  getMedians(cQData[cQData$p1_gamma > 0.75, ], "$\\varnothing$ Slow")
  getMedians(cQData[cQData$p1_gamma <= 0.75, ], "$\\varnothing$ Fast")
  
  # exploration rate
  getMedians(cQData[cQData$p1_epsilon_start <= 0.1, ], "$\\varnothing$ Exploitative")
  getMedians(cQData[cQData$p1_epsilon_start > 0.1, ], "$\\varnothing$ Exploratory")
  
  # exploration decrease
  getMedians(cQData[cQData$p1_epsilon_decay == 0.995, ], "$\\varnothing$ Slow")
  getMedians(cQData[cQData$p1_epsilon_decay == 0.98, ], "$\\varnothing$ Fast")
  
  # actions per state
  getMedians(cQData[cQData$p1_X == 2, ], "$\\varnothing$ 2")
  getMedians(cQData[cQData$p1_X == 3, ], "$\\varnothing$ 3")
  
  # players per state
  getMedians(cQData[cQData$p1_players_per_state == 1, ], "$\\varnothing$ 1")
  getMedians(cQData[cQData$p1_players_per_state == 3, ], "$\\varnothing$ 3")
  
  # selfish, optimistic
  getMedians(cQData[cQData$p1_social_behavior == "selfish" & cQData$p1_prop_start >= 100, ], "$\\varnothing$")
  
  # altruistic, pessimistic
  getMedians(cQData[cQData$p1_social_behavior == "altruistic" & cQData$p1_prop_start < 100, ], "$\\varnothing$")
  
  
  # selfish, optimistic, slow learner
  getMedians(cQData[cQData$p1_social_behavior == "selfish" 
                    & cQData$p1_prop_start >= 100
                    & cQData$p1_alpha <= 0.3, ], "$\\varnothing$")
  
  
  # CQ.362
  getMedians(cQData[cQData$simCount == 362,], "NA")
  
  # CQ.402
  getMedians(cQData[cQData$simCount == 402,], "NA")
  
  # CQ.1442
  getMedians(cQData[cQData$simCount == 1442,], "NA")
  
  # CQ.1947
  getMedians(cQData[cQData$simCount == 1947,], "NA")
  
  # CQ.1949
  getMedians(cQData[cQData$simCount == 1949,], "NA")
  
}

analyzeCoordinateXFits <- function() {
  
  cXData <- read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170611-coordinateX-fit.csv")
  
  # overall results
  # best fit
  getMedians(cXData[cXData$RMSE_combined == min(cXData$RMSE_combined),], "Best Fit")
  # average
  getMedians(cXData, "$\\varnothing$ Fit")
  
  # balancing
  getMedians(cXData[cXData$p1_balancing == "greedy" & cXData$p1_epsilon_decay == 1, ], "$\\varnothing$ $\\epsilon$-greedy")
  getMedians(cXData[cXData$p1_balancing == "greedy" & cXData$p1_epsilon_decay < 1, ], "$\\varnothing$ $\\epsilon$-decreasing")
  getMedians(cXData[cXData$p1_balancing == "noise" & cXData$p1_epsilon_decay == 1, ], "$\\varnothing$ $\\epsilon$-noise")
  getMedians(cXData[cXData$p1_balancing == "noise" & cXData$p1_epsilon_decay < 1, ], "$\\varnothing$ $\\epsilon$-noise-decreasing")
  
  # social preferences
  getMedians(cXData[cXData$p1_social_behavior == "selfish", ], "$\\varnothing$ Selfish")
  getMedians(cXData[cXData$p1_social_behavior == "altruistic", ], "$\\varnothing$ Altruistic")
  
  # initial props
  getMedians(cXData[cXData$p1_prop_start >= 100, ], "$\\varnothing$ Optimistic")
  getMedians(cXData[cXData$p1_prop_start < 100, ], "$\\varnothing$ Pessimistic")
  
  # learning rate
  getMedians(cXData[cXData$p1_alpha <= 0.3, ], "$\\varnothing$ Slow")
  getMedians(cXData[cXData$p1_alpha > 0.3, ], "$\\varnothing$ Fast")
  
  # discount rate
  getMedians(cXData[cXData$p1_gamma > 0.75, ], "$\\varnothing$ Slow")
  getMedians(cXData[cXData$p1_gamma <= 0.75, ], "$\\varnothing$ Fast")
  
  # exploration rate
  getMedians(cXData[cXData$p1_epsilon_start <= 0.1, ], "$\\varnothing$ Exploitative")
  getMedians(cXData[cXData$p1_epsilon_start > 0.1, ], "$\\varnothing$ Exploratory")
  
  # exploration decrease
  getMedians(cXData[cXData$p1_epsilon_decay == 0.995, ], "$\\varnothing$ Slow")
  getMedians(cXData[cXData$p1_epsilon_decay == 0.98, ], "$\\varnothing$ Fast")
  
  # Max. coord. pos.
  getMedians(cXData[cXData$p1_X == 2, ], "$\\varnothing$ 2")
  getMedians(cXData[cXData$p1_X == 3, ], "$\\varnothing$ 3")
  getMedians(cXData[cXData$p1_X == 4, ], "$\\varnothing$ 4")
  
  # selfish, optimistic
  getMedians(cXData[cXData$p1_social_behavior == "selfish" 
                    & cXData$p1_prop_start >= 100, ], "$\\varnothing$")
  
  # altruistic, pessimistic
  getMedians(cXData[cXData$p1_social_behavior == "altruistic" 
                    & cXData$p1_prop_start < 100, ], "$\\varnothing$")
  
  
  # selfish, optimistic, slow learner
  getMedians(cXData[cXData$p1_social_behavior == "selfish" 
                    & cXData$p1_prop_start >= 100
                    & cXData$p1_alpha <= 0.3, ], "$\\varnothing$")
  
  
  # selfish, optimistic, slow learner, max. coord. pos. 2
  getMedians(cXData[cXData$p1_social_behavior == "selfish" 
                    & cXData$p1_prop_start >= 100
                    & cXData$p1_alpha <= 0.3
                    & cXData$p1_X == 2, ], "$\\varnothing$")
  
  # altruistic, pessimistic, slow learner, max. coord. pos. 4
  getMedians(cXData[cXData$p1_social_behavior == "selfish" 
                    & cXData$p1_prop_start >= 100
                    & cXData$p1_alpha <= 0.3
                    & cXData$p1_X == 4, ], "$\\varnothing$")
  
  
  # CX.200
  getMedians(cXData[cXData$simCount == 200,], "NA")
  
  # CX.1983
  getMedians(cXData[cXData$simCount == 1983,], "NA")
  
  # CX.2648
  getMedians(cXData[cXData$simCount == 2648,], "NA")
  
  # CX.2807
  getMedians(cXData[cXData$simCount == 2807,], "NA")
}

#
fitPlotWidth <- 1100
fitPlotHeight <- 850

exportFitComparisons <- function() {
  
  plotBestComparisons()
  plotAverageComparisons()
  plotSelfishComparisons()
  plotAltruisticComparisons()
  plotOptimisticComparisons()
  plotPessimisticComparisons()
  plotSelfishOptimisticComparisons()
  plotAltruisticPessimisticComparisons()
}
  
plotBestComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
             34.41, 9.76, 4.7, 20.83,
             5.79, 9.44, 4.57, 6.92)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  best <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "a-best-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(best, "a. Best Fit")
  dev.off() 
  
  
}  

plotAverageComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            37.92, 31.68, 44.54, 37.89,
            35.07, 25.55, 34.61, 30.33)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  average <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "b-average-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(average, "b. Average Fit", "bottomLeft")
  dev.off()
}  

plotSelfishComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            37.09, 26.41, 18.34, 28.89,
            33.64, 24.21, 26.71, 28.84)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  selfish <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "c-selfish-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(selfish, "c. Selfish Fit", "bottomLeft")
  dev.off()
}

plotAltruisticComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            38.71, 33.3, 46.37, 39.25,
            39.44, 34.12, 46.74, 40.43)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  altruistic <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "d-altruistic-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(altruistic, "d. Altruistic Fit", "bottomLeft")
  dev.off()
}

plotOptimisticComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            37.38, 29.41, 30.44, 31.63,
            34.01, 24.05, 23.91, 28.36)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  optimistic <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "e-optimistic-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(optimistic, "e. Optimistic Fit", "bottomLeft")
  dev.off()
}  

plotPessimisticComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            38.64, 32.43, 45.17, 38.32,
            38.67, 33.71, 46.37, 39.05)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  pessimistic <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "f-pessimistic-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(pessimistic, "f. Pessimistic Fit", "bottomLeft")
  dev.off()
}    

plotSelfishOptimisticComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            36.14, 20.89, 14.28, 25.48,
            33.29, 23.42, 21.13, 27.71)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  pessimistic <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "g-selfish-optimistic-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(pessimistic, "g. Selfish Optimistic Fit", "bottomLeft")
  dev.off()
}  

plotAltruisticPessimisticComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            39.03, 33.71, 46.37, 39.35,
            39.44, 34.12, 46.74, 40.43)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  pessimistic <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "h-altruistic-pessimistic-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(pessimistic, "h. Altruistic Pessimistic Fit", "bottomLeft")
  dev.off()
}

plotSlowLearningComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            37.83, 31.3, 44.22, 37.66,
            34.9, 23.1, 19.53, 26.76)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  slow <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "i-slow-learning-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(slow, "i. Slow Learning Fit", "bottomLeft")
  dev.off()
}  

plotFastLearningComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            38.13, 31.88, 44.55, 38.01,
            35.22, 26.61, 37.96, 31.16)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  fast <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "j-fast-learning-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  plotParameterComparisons(fast, "j. Fast Learning Fit", "bottomLeft")
  dev.off()
}  





plotMaxCoordPosComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3,
             4,4,4,4)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            37.19, 22.62, 15.32, 26.8,
            36.76, 29.59, 42.46, 35.36,
            27.94, 23.26, 31.45, 27.37)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  data <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "k-max-coord-pos-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight, 
      units = "px", 
      res = 196)
  
  # Create Line Chart
  # convert factor to numeric for convenience 
  data$Model <- as.numeric(data$Model)
  nmodels <- max(data$Model)
  data$VOD <- as.numeric(data$VOD)
  
  # get the range for the x and y axis 
  xrange <- range(data$VOD) 
  yrange <- range(data$RMSE) 
  
  # set up the plot 
  plot(xrange, c(0,50), type="n", xlab="VOD types",
       ylab="RSME", xaxt = "n")
  axis(1, labels = c("Symmetric", "Asymmetric 1", "Asymmetric 2", "combined"), at = c(1,2,3,4))
  colors <- c("gray", "#51beff", "#1d7cb5", "#052133")
  linetype <- c(1:nmodels) 
  plotchar <- seq(18,18+nmodels,1)
  
  # add lines 
  for (i in 1:nmodels) { 
    sub <- subset(data, Model==i) 
    lines(sub$VOD, sub$RMSE, type="b", lwd=1.5,
          lty=linetype[i], col=colors[i], pch=plotchar[i]) 
  } 
  
  # add a title and subtitle 
  title("k. Maximum Coordination Position Fit (CoordinateX)")
  
  # add a legend 
  legend(1.2, 18, c("NA (Random)", "X = 2", "X = 3", "X = 4"), cex=0.8, col=colors,
         pch=plotchar, lty=linetype)
  
  dev.off()
}  



plotBestCombinationMaxCoordPosComparisons <- function() {
  Model <- c(1,1,1,1,
             2,2,2,2,
             3,3,3,3,
             4,4,4,4,
             5,5,5,5)
  RMSE <- c(32.3, 27.96, 41.05, 34.2,
            33.29, 23.42, 21.13, 27.71,
            33.66, 18.98, 13.53, 24.54,
            36.23, 20.32, 12.6, 25.49,
            31.64, 20.39, 19.34, 25.48)
  VOD <- c(1,2,3,4,
           1,2,3,4,
           1,2,3,4,
           1,2,3,4,
           1,2,3,4)
  
  data <- data.frame(Model, RMSE, VOD)
  
  png(paste("/Users/hendrik/Desktop/", "l-best-max-coord-pos-comparisons.png", sep = ""), 
      width = fitPlotWidth, 
      height = fitPlotHeight+150, 
      units = "px", 
      res = 196)
  
  # Create Line Chart
  # convert factor to numeric for convenience 
  data$Model <- as.numeric(data$Model)
  nmodels <- max(data$Model)
  data$VOD <- as.numeric(data$VOD)
  
  # get the range for the x and y axis 
  xrange <- range(data$VOD) 
  yrange <- range(data$RMSE) 
  
  # set up the plot 
  plot(xrange, c(0,60), type="n", xlab="VOD types",
       ylab="RSME", xaxt = "n")
  axis(1, labels = c("Symmetric", "Asymmetric 1", "Asymmetric 2", "combined"), at = c(1,2,3,4))
  colors <- c("gray", "#9ad9ff", "#51beff", "#1d7cb5", "#052133")
  linetype <- c(1:nmodels) 
  plotchar <- seq(18,18+nmodels,1)
  
  # add lines 
  for (i in 1:nmodels) { 
    sub <- subset(data, Model==i) 
    lines(sub$VOD, sub$RMSE, type="b", lwd=1.5,
          lty=linetype[i], col=colors[i], pch=plotchar[i]) 
  } 
  
  # add a title and subtitle 
  title("k. Best Combinations Fit (CoordinateX)")
  
  # add a legend 
  legend(0.95, 60, c("Random", 
                     "selfish, optimistic",
                     "selfish, optimistic, slow learner",
                     "selfish, optimistic, slow learner, X = 2", 
                     "selfish, optimistic, slow learner, X = 4"), cex=0.8, col=colors,
         pch=plotchar, lty=linetype)
  
  dev.off()
}  





  
plotParameterComparisons <- function(data, title, legendPos = "topLeft") {
  
  # Create Line Chart
  # convert factor to numeric for convenience 
  data$Model <- as.numeric(data$Model)
  nmodels <- max(data$Model)
  data$VOD <- as.numeric(data$VOD)
  
  # get the range for the x and y axis 
  xrange <- range(data$VOD) 
  yrange <- range(data$RMSE) 
  
  # set up the plot 
  plot(xrange, c(0,50), type="n", xlab="VOD types",
       ylab="RSME", xaxt = "n")
  axis(1, labels = c("Symmetric", "Asymmetric 1", "Asymmetric 2", "combined"), at = c(1,2,3,4))
  colors <- c("gray", "#D55E00", "#56B4E9")
  linetype <- c(1:nmodels) 
  plotchar <- seq(18,18+nmodels,1)
  
  # add lines 
  for (i in 1:nmodels) { 
    sub <- subset(data, Model==i) 
    lines(sub$VOD, sub$RMSE, type="b", lwd=1.5,
          lty=linetype[i], col=colors[i], pch=plotchar[i]) 
  } 
  
  # add a title and subtitle 
  title(title)
  
  # add a legend 
  
  if (legendPos == "topLeft") {
    legend(1.2, 50, c("Random", "ClassicQ", "CoordinateX"), cex=0.8, col=colors,
           pch=plotchar, lty=linetype, title="Model Class")
  } else {
    legend(1.2, 18, c("Random", "ClassicQ", "CoordinateX"), cex=0.8, col=colors,
           pch=plotchar, lty=linetype, title="Model Class")
  }
  
}











exportCombinedGof <- function() {
  plotWidth <- 2000
  plotHeight <- 1000
  
  png(paste("/Users/hendrik/Desktop/", "combined-gof.png", sep = ""), 
      width = plotWidth, 
      height = plotHeight, 
      units = "px", 
      res = 196)
  plotCombinedGof()
  dev.off()
}

plotCombinedGof <- function() {
  
  bestLnisClassQ <- data.frame("sym_h1" = 10.33, "sym_h2" = 4, "sym_h3" = 0, "sym_others" = 86.33,
                               "asym1_h1" = 44.33, "asym1_h2" = 2.33, "asym1_h3" = 0, "asym1_others" = 52.33,
                               "asym2_h1" = 61.33, "asym2_h2" = 1, "asym2_h3" = 0, "asym2_others" = 36.67)
  bestLnisCoordX <- data.frame("sym_h1" = 0, "sym_h2" = 2, "sym_h3" = 40.33, "sym_others" = 41,
                               "asym1_h1" = 23, "asym1_h2" = 0, "asym1_h3" = 2, "asym1_others" = 47.67,
                               "asym2_h1" = 67, "asym2_h2" = 0, "asym2_h3" = 0, "asym2_others" = 33)
  
  # binding of simulation and experimental data
  LNIs <- rbind(LNIS_EXP1, bestLnisClassQ, bestLnisCoordX)
  
  
  titleBig <- 1.5
  titleSmall <- 1.2
  legendSize <- 1.1
  ySize <- 0.9
  
  
  # three diagrams - one per VOD type (sym, asym1, asym2)
  par(mfrow=c(1,3), oma = c(0, 4, 3, 0), mar = c(5, 2, 1, 1))
  cols <- c("black", "#D55E00", "#56B4E9")
  
  # compare model and experimental data: Symmetric
  plotDataSym1 <- data.frame(h1 = LNIs$sym_h1,
                             h2 = LNIs$sym_h2,
                             h3 = LNIs$sym_h3,
                             others = LNIs$sym_others)
  barplot(as.matrix(plotDataSym1), xlab = "Symmetric", cex.axis = titleSmall, cex.names = titleSmall, cex.lab = titleSmall,
          beside = TRUE, col = cols, ylim = range(0:100))
  
  # compare model and experimental data: Asymmetric 1
  plotDataAsym1 <- data.frame(h1 = LNIs$asym1_h1,
                              h2 = LNIs$asym1_h2,
                              h3 = LNIs$asym1_h3,
                              others = LNIs$asym1_others)
  barplot(as.matrix(plotDataAsym1), yaxt = "n", xlab = "Asymmetric 1", cex.axis = titleSmall, cex.names = titleSmall,  cex.lab = titleSmall,
          beside = TRUE, col = cols, ylim = range(0:100))
  
  # compare model and experimental data: Asymmetric 2
  plotDataAsym2 <- data.frame(h1 = LNIs$asym2_h1,
                              h2 = LNIs$asym2_h2,
                              h3 = LNIs$asym2_h3,
                              others = LNIs$asym2_others)
  barplot(as.matrix(plotDataAsym2), yaxt = "n", xlab = "Asymmetric 2", cex.axis = titleSmall, cex.names = titleSmall, cex.lab = titleSmall,
          beside = TRUE, col = cols, ylim = range(0:100))
  
  
  # legend and title
  legend(x = "topright", y = 5, c("Diekmann & Przepiorka (2016)", "ClassicQ", "CoordinateX"), cex=legendSize, fill=cols)
  title("Model Data vs. Experimental Data", outer=TRUE, cex.main = titleBig)
  mtext('average LNI', side = 2, outer = TRUE, line = 1.5, cex = ySize)
   
}




exportSingleGofs <- function() {
  
  plotWidth <- 1600
  plotHeight <- 1200
  
  png(paste("/Users/hendrik/Desktop/", "classq-gof.png", sep = ""), 
      width = plotWidth, 
      height = plotHeight, 
      units = "px", 
      res = 196)
  plotClassicQGof()
  dev.off()
  
  png(paste("/Users/hendrik/Desktop/", "coordx-gof.png", sep = ""), 
      width = plotWidth, 
      height = plotHeight, 
      units = "px", 
      res = 196)
  plotCoordinateXGof()
  dev.off()  
}


plotClassicQGof <- function() {
  
  bestLnisClassQ <- data.frame("sym_h1" = 10.33, "sym_h2" = 4, "sym_h3" = 0, "sym_others" = 86.33,
                               "asym1_h1" = 44.33, "asym1_h2" = 2.33, "asym1_h3" = 0, "asym1_others" = 52.33,
                               "asym2_h1" = 61.33, "asym2_h2" = 1, "asym2_h3" = 0, "asym2_others" = 36.67)
  
  # binding of simulation and experimental data
  LNIs <- rbind(LNIS_EXP1, bestLnisClassQ)
  
  
  titleBig <- 2.5
  titleSmall <- 2.1
  xLegendSize <- 1.7
  legendSize <- 1.65
  xSize <- 1.4
  ySize <- 1.4
  yAxisSize <- 2.0
  
  
  # three diagrams - one per VOD type (sym, asym1, asym2)
  par(mfrow=c(1,3), oma = c(0, 4, 3, 0), mar = c(10, 2, 1, 4))
  cols <- c("black", "#D55E00")
  
  # compare model and experimental data: Symmetric
  plotDataSym1 <- data.frame(h1 = LNIs$sym_h1,
                             h2 = LNIs$sym_h2,
                             h3 = LNIs$sym_h3,
                             others = LNIs$sym_others)
  barplot(as.matrix(plotDataSym1), yaxt = "n", xlab = "", cex.axis = xLegendSize, cex.names = xLegendSize, cex.lab = xLegendSize, las = 2,
          beside = TRUE, col = cols, ylim = range(0:100))
  axis(side=2, at=seq(0, 100, by = 10), cex.axis = yAxisSize, las = 1) 
  mtext('                  average LNI', side = 2, outer = TRUE, line = 2.4, cex = ySize)
  mtext('Symmetric      ', side = 1, line = 4, cex = xSize)
  
  # compare model and experimental data: Asymmetric 1
  plotDataAsym1 <- data.frame(h1 = LNIs$asym1_h1,
                              h2 = LNIs$asym1_h2,
                              h3 = LNIs$asym1_h3,
                              others = LNIs$asym1_others)
  barplot(as.matrix(plotDataAsym1), yaxt = "n", xlab = "", cex.axis = xLegendSize, cex.names = xLegendSize,  cex.lab = xLegendSize, las = 2,
          beside = TRUE, col = cols, ylim = range(0:100))
  mtext('Asymmetric 1         ', side = 1, line = 4, cex = xSize)
  
  # compare model and experimental data: Asymmetric 2
  plotDataAsym2 <- data.frame(h1 = LNIs$asym2_h1,
                              h2 = LNIs$asym2_h2,
                              h3 = LNIs$asym2_h3,
                              others = LNIs$asym2_others)
  barplot(as.matrix(plotDataAsym2), yaxt = "n", xlab = "", cex.axis = xLegendSize, cex.names = xLegendSize, cex.lab = xLegendSize, las = 2,
          beside = TRUE, col = cols, ylim = range(0:100))
  mtext('Asymmetric 2         ', side = 1, line = 4, cex = xSize)
  
  
  # legend and title
  legend(x = "topright", y = 10, c("Experiment", "ClassicQ"), cex=legendSize, fill=cols, title = "Data Source")
  title("a. Experimental Data vs. ClassicQ Predictions", outer=TRUE, cex.main = titleBig)
  mtext('VOD type                                                                              ', side = 1, line = 8, cex = xSize)
  
}



plotCoordinateXGof <- function() {
  
  bestLnisCoordX <- data.frame("sym_h1" = 0, "sym_h2" = 2, "sym_h3" = 40.33, "sym_others" = 41,
                               "asym1_h1" = 23, "asym1_h2" = 0, "asym1_h3" = 2, "asym1_others" = 47.67,
                               "asym2_h1" = 67, "asym2_h2" = 0, "asym2_h3" = 0, "asym2_others" = 33)
  
  # binding of simulation and experimental data
  LNIs <- rbind(LNIS_EXP1, bestLnisCoordX)
  
  
  titleBig <- 2.5
  titleSmall <- 2.1
  xLegendSize <- 1.7
  legendSize <- 1.65
  xSize <- 1.4
  ySize <- 1.4
  yAxisSize <- 2.0
  
  
  # three diagrams - one per VOD type (sym, asym1, asym2)
  par(mfrow=c(1,3), oma = c(0, 4, 3, 0), mar = c(10, 2, 1, 4))
  cols <- c("black", "#56B4E9")
  
  # compare model and experimental data: Symmetric
  plotDataSym1 <- data.frame(h1 = LNIs$sym_h1,
                             h2 = LNIs$sym_h2,
                             h3 = LNIs$sym_h3,
                             others = LNIs$sym_others)
  barplot(as.matrix(plotDataSym1), yaxt = "n", xlab = "", cex.axis = xLegendSize, cex.names = xLegendSize, cex.lab = xLegendSize, las = 2,
          beside = TRUE, col = cols, ylim = range(0:100))
  axis(side=2, at=seq(0, 100, by = 10), cex.axis = yAxisSize, las = 1) 
  mtext('                  average LNI', side = 2, outer = TRUE, line = 2.4, cex = ySize)
  mtext('Symmetric      ', side = 1, line = 4, cex = xSize)
  
  # compare model and experimental data: Asymmetric 1
  plotDataAsym1 <- data.frame(h1 = LNIs$asym1_h1,
                              h2 = LNIs$asym1_h2,
                              h3 = LNIs$asym1_h3,
                              others = LNIs$asym1_others)
  barplot(as.matrix(plotDataAsym1), yaxt = "n", xlab = "", cex.axis = xLegendSize, cex.names = xLegendSize,  cex.lab = xLegendSize, las = 2,
          beside = TRUE, col = cols, ylim = range(0:100))
  mtext('Asymmetric 1         ', side = 1, line = 4, cex = ySize)
  
  # compare model and experimental data: Asymmetric 2
  plotDataAsym2 <- data.frame(h1 = LNIs$asym2_h1,
                              h2 = LNIs$asym2_h2,
                              h3 = LNIs$asym2_h3,
                              others = LNIs$asym2_others)
  barplot(as.matrix(plotDataAsym2), yaxt = "n", xlab = "", cex.axis = xLegendSize, cex.names = xLegendSize, cex.lab = xLegendSize, las = 2,
          beside = TRUE, col = cols, ylim = range(0:100))
  mtext('Asymmetric 2         ', side = 1, line = 4, cex = ySize)
  
  
  # legend and title
  legend(x = "topright", y = 5, c("Experiment", "CoordinateX"), cex=legendSize, fill=cols, title = "Data Source")
  title("b. Experimental Data vs. CoordinateX Predictions", outer=TRUE, cex.main = titleBig)
  mtext('VOD type                                                                              ', side = 1, line = 8, cex = xSize)
}






exportAllGofs <- function() {
  
  lnis <- list(LNIS_CQ362, "CQ.362",
               LNIS_CQ402, "CQ.402",
               LNIS_CQ1442, "CQ.1442",
               LNIS_CQ1947, "CQ.1947",
               LNIS_CQ1949, "CQ.1949",
            
               LNIS_CX200, "CX.200",
               LNIS_CX1983, "CX.1983",
               LNIS_CX2648, "CX.2648",
               LNIS_CX2807, "CX.2807")
  
  lni <- 1
  lowerLet <- 1
  stepWidth <- length(lnis) / 2
  
  while (lni < length(lnis)) {
    lets <- letters[seq(from = lowerLet, 
                        to = lowerLet + 2*stepWidth, 
                        by = stepWidth)]
    exportSingleGofs(lnis[[lni]], lnis[[lni+1]], lets)
    lni <- lni+2
    lowerLet <- lowerLet+1
  }
}


exportSingleGofs <- function(lnis, title, lets) {
  
  plotWidth <- 530
  plotHeight <- 850
  
  modelType <- substr(title, 1, 2)
  
  letCnt <- 1
  for (vodType in VOD_TYPES) {
    fileName <- paste("gof-", title,"-", vodType, ".png", sep = "")
    png(paste("/Users/hendrik/Desktop/", fileName, sep = ""), 
        width = plotWidth, 
        height = plotHeight, 
        units = "px", 
        res = 196)
    plotSingleGof(lnis, vodType, modelType, paste(lets[letCnt], ". ", title, sep=""))
    dev.off()
    letCnt <- letCnt+1
  }
  
}


plotSingleGof <- function(lnis ,vodType, modelType, title) {
  
  # binding of simulation and experimental data
  LNIs <- rbind(LNIS_EXP1, lnis)

  # font sizes  
  xSize <- 0.75
  ySize <- 0.9
  yAxisSize <- 0.75
  legendSize <- 0.75
  titleSize <- 1
  
  # margins
  par(mfrow=c(1,1), oma = c(0, 2, 1, 0), mar = c(2, 1, 0, 0))
  
  # colors
  cols <- c("black", "gray")
  if (modelType == "CQ") {
    cols <- c("black", "#D55E00")
  } else if (modelType == "CX") {
    cols <- c("black", "#56B4E9")    
  }
  
  # plot data
  plotData <- NA
  if (vodType == "sym") {
    plotData <- data.frame(h1 = LNIs$sym_h1,
                           h2 = LNIs$sym_h2,
                           h3 = LNIs$sym_h3)
                           #, others = LNIs$sym_others)
  } else if (vodType == "asym1") {
    plotData <- data.frame(h1 = LNIs$asym1_h1,
                           h2 = LNIs$asym1_h2,
                           h3 = LNIs$asym1_h3)
                           #, others = LNIs$asym1_others)
  } else if (vodType == "asym2") {
    plotData <- data.frame(h1 = LNIs$asym2_h1,
                           h2 = LNIs$asym2_h2,
                           h3 = LNIs$asym2_h3)
                           #, others = LNIs$asym2_others)
  }
  
  # plot
  barplot(as.matrix(plotData), 
          yaxt = "n", 
          names.arg = c("solitary \nvolunteer.",
                        "turn-taking \n(2 players)",
                        "turn-taking \n(3 players)"),
          cex.axis = xSize, cex.names = xSize, cex.lab = xSize,
          beside = TRUE, col = cols, ylim = range(0:100))
  
  # x-label
  # mtext('    Frequency', side = 1, outer = TRUE, line = 0.8, cex = xSize)
  
  # y-axis
  axis(side=2, at=seq(0, 100, by = 10), cex.axis = yAxisSize, las = 1)
  mtext('    Frequency', side = 2, outer = TRUE, line = 1, cex = ySize)
  
  # legend
  legend(x = 1, y = 100, c("Experiment", substr(title, 4, nchar(title))), cex=legendSize, fill=cols, title = "Data Source")
  
  # title
  #title(title, outer=TRUE, cex.main = titleSize)
}
















findExemplarySimulations <- function() {
  
  # 1. ClassicQ
  cQData <- rbind(read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170609-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170610-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170611-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170615-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170616-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170617-classicQ-fit.csv"),
                  read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170618-classicQ-fit.csv"))
  
  # 1.1. best fit combined
  cQDataOrdered <- cQData[order(cQData[,11]), ]
  cQDataOrdered[1,]$simCount                        # 362
  
  # 1.2. best fit symmetric
  cQSym <- cQData[cQData$vod_type == "sym", ]
  cQSymOrdered <- cQSym[order(cQSym[,7]), ]
  cQSymOrdered[1,]$simCount                         # 1557
  
  for (i in 1:20) {
    print(paste("sim:", cQSymOrdered[i,]$simCount, 
                "- RMSE sym:", cQSymOrdered[i,]$RMSE_per_vod_type,
                "|| RMSE combined:", 
                cQData[cQData$simCount == cQSymOrdered[i,]$simCount & cQData$vod_type == "sym", ]$RMSE_combined))
  }   # 1557
  
  # 1.3. best fit asymmetric 
  cQAsym1 <- cQData[cQData$vod_type == "asym1", ]
  cQAsym1Ordered <- cQAsym1[order(cQAsym1[,7]), ]
  cQAsym1Ordered[1,]$simCount
  
  for (i in 1:50) {
    print(paste("sim:", cQAsym1Ordered[i,]$simCount, 
                "- RMSE asym1:", cQAsym1Ordered[i,]$RMSE_per_vod_type,
                "|| RMSE asym2:", 
                cQData[cQData$simCount == cQAsym1Ordered[i,]$simCount & cQData$vod_type == "asym2", ]$RMSE_per_vod_type,
                " || balancing:",
                cQData[cQData$simCount == cQAsym1Ordered[i,]$simCount & cQData$vod_type == "asym2", ]$p1_balancing,
                "|| RMSE combined:", 
                cQData[cQData$simCount == cQAsym1Ordered[i,]$simCount & cQData$vod_type == "asym2", ]$RMSE_combined))
  }   # 402
  
  
  # 2. CoordinateX
  cXData <- read.csv("/Users/hendrik/git/uu/mscp-model/simulations/20170611-coordinateX-fit.csv")
  
  # 2.1. best fit combined
  cXDataOrdered <- cXData[order(cXData[,11]), ]
  cXDataOrdered[1,]$simCount                        # 1983
  
  # 2.2. best fit symmetric
  cXSym <- cXData[cXData$vod_type == "sym", ]
  cXSymOrdered <- cXSym[order(cXSym[,7]), ]
  cXSymOrdered[1,]$simCount                         # 1998
  
  for (i in 1:20) {
    print(paste("sim:", cXSymOrdered[i,]$simCount, 
                "- RMSE sym:", cXSymOrdered[i,]$RMSE_per_vod_type,
                "|| RMSE combined:", 
                cXData[cXData$simCount == cXSymOrdered[i,]$simCount & cXData$vod_type == "sym", ]$RMSE_combined))
  }   # 1983
  
  # 2.3. best fit asymmetric 
  cXAsym1 <- cXData[cXData$vod_type == "asym1", ]
  cXAsym1Ordered <- cXAsym1[order(cXAsym1[,7]), ]
  cXAsym1Ordered[1,]$simCount
  
  for (i in 1:20) {
    print(paste("sim:", cXAsym1Ordered[i,]$simCount, 
                "- RMSE asym1:", cXAsym1Ordered[i,]$RMSE_per_vod_type,
                "|| RMSE asym2:", 
                cXData[cXData$simCount == cXAsym1Ordered[i,]$simCount & cXData$vod_type == "asym2", ]$RMSE_per_vod_type,
                " || balancing:",
                cXData[cXData$simCount == cXAsym1Ordered[i,]$simCount & cXData$vod_type == "asym2", ]$p1_balancing,
                "|| RMSE combined:", 
                cXData[cXData$simCount == cXAsym1Ordered[i,]$simCount & cXData$vod_type == "asym2", ]$RMSE_combined))
  }   # 2648  
  
  cXData[cXData$simCount == 2807, ]$RMSE_per_vod_type
}








plotInteractionAndConvergencePattern <- function() {
  
  patternDirs <- c(#"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/ClassicQ/362_best_combined/",
                   #"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/ClassicQ/402_best_asym/",
                   #"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/ClassicQ/1442_best_asym-alt/",
                   #"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/ClassicQ/1947_best_sym-alt/")
                   #"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/CoordinateX/200_best_asym-alt/",
                   #"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/CoordinateX/1983_best_combined-and-scarce/",
                   #"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/CoordinateX/2648_best_asym/",
                   #"/Users/hendrik/git/uu/mscp-thesis/!data-patterns/CoordinateX/2807_best_sym-alt-and-scarce/")
                   "/Users/hendrik/git/uu/mscp-thesis/!data-patterns/ClassicQ/1949/")
  
  for (patternDir in patternDirs) {
    for (vodType in VOD_TYPES) {
      currentDir <- paste(patternDir, vodType, "/", sep="")
      
      vodSimData <- list()
      for (i in 1:length(list.files(currentDir, recursive = FALSE))) {
        filename <- paste(currentDir, BASE_FILENAME, i, ".Rdata", sep = "")
        vodSimData[[i]] <- get(load(filename))
      }
      
      convergencePatterns <- list()
      for (i in 1:(length(vodSimData))) {
        lniSequence <- extractLNISequence(vodSimData[[i]])
        lnis <- computeLNIs(lniSequence)
        convergencePatterns[[i]] <- computeConvergencePatterns(lniSequence)
      }
      
      
      for (i in 1:length(convergencePatterns)) {
        filename <- paste(patternDir, vodType, "-patterns-", i, ".png", sep="")
        png(filename,
            width = 1900, 
            height = 400, 
            units = "px", 
            res = 196)
        # setting up multiple plots
        plotsPerImage <- 2
        par(mfrow=c(plotsPerImage,1),oma=c(2.8,0,0,0), mai = c(0, 1, 0.1, 0.1))
        
        nf <- layout(matrix(c(1,2),ncol=1), widths=c(54,54), heights=c(3,5), TRUE) 
        plotInteractionPattern(vodSimData[[i]], 1, 2)
        plotConvergencePattern(convergencePatterns[[i]])
        
        dev.off() 
      }
    }
  }
}




###################################### EVENTUAL INTEGRITY CHECKS #####################################
source(paste(BASE_DIR, "/tests/testAnalysis.R", sep = ""))
