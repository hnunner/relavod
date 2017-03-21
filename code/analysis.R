########################################## GLOBAL PARAMETERS #########################################
if (!exists("BASE_DIR")) BASE_DIR <<- paste(dirname(sys.frame(1)$ofile), "/", sep = "")
if (!exists("MODEL_TYPES")) source(paste(BASE_DIR, "constants.R", sep = ""))


######################################## FILE SYSTEM / IMPORTS #######################################
#----------------------------------------------------------------------------------------------------#
# function: getVodBaseDir
#     Returns the base directory of a model.
#     Data is stored as: "SIM_DIR/modelType/Date/dateCount/vodType/sim-X.Rdata"
#     Base directory corresponds to: "SIM_DIR/modelType/Date/dateCount/."
#     For available for modelTypes, see constants.R - 'MODEL_TYPES'
#     Date format: yyyymmdd
#     param:  modelType
#         the model type used by the simulation
#     param:  date
#         the date of the simulation 
#         (if undefined: latest available date)
#     param:  dateCount
#         defines the round of simulation of the given date
#         (if undefined: latest available round of simulations)
#----------------------------------------------------------------------------------------------------#
getVodBaseDir <- function(modelType, date = "latest", dateCount = "latest") {
  modelDir <- paste(SIM_DIR, modelType, sep = "")
  
  if (date == "latest") {
    dateDirs <- list.dirs(modelDir, recursive = FALSE)
    dates <- gsub(paste(modelDir, "/", sep = ""), "", dateDirs, fixed = TRUE)
    date <- max(dates)
  }
  dateDir <- paste(modelDir, "/", date, sep = "")
  
  if (dateCount == "latest") {
    dateCountDirs <- list.dirs(dateDir, recursive = FALSE)
    dateCounts <- gsub(paste(dateDir, "/", sep = ""), "", dateCountDirs, fixed = TRUE)
    dateCount <- max(dateCounts)
  }
  baseDir <- paste(dateDir, "/", dateCount, sep = "")
  
  return(baseDir)
}

#----------------------------------------------------------------------------------------------------#
# function: importVodSimData
#     Imports VOD simulation data. 
#     Data is stored as: "SIM_DIR/modelType/Date/dateCount/vodType/sim-X.Rdata"
#     For available for modelTypes, see constants.R - 'MODEL_TYPES'
#     Date format: yyyymmdd
#     param:  modelType
#         the model type used by the simulation
#     param:  date
#         the date of the simulation 
#         (if undefined: latest available date)
#     param:  dateCount
#         defines the round of simulation of the given date
#         (if undefined: latest available round of simulations)
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: "sym", "asym1", "asym2"
#----------------------------------------------------------------------------------------------------#
importVodSimData <- function(modelType = MODEL_TYPES[7],
                             date = "latest",
                             dateCount = "latest", 
                             vodType = "sym") {
  
  vodBaseDir <- getVodBaseDir(modelType, date, dateCount)
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
#   TODOs: 
#       - generalize code for different sequence lengths (quite dodgy the way it is)
#   param:  lniSequence
#       the LNI sequence 
#----------------------------------------------------------------------------------------------------#
computeLNIs <- function(lniSequence) {
  
  # 1-sequences
  oneSequences <- c()
  i <- 1
  while (i < length(lniSequence)) {
    currInteraction <- lniSequence[i]
    
    if (currInteraction[1] == -1) {
      i <- i+1
      next
    }
    
    # compare current interaction with next interactions, 
    # as long as we haven't reached the end of the sequence
    # and as long as they are the same
    seqLength <- 1
    j <- i+1
    while (j <= length(lniSequence) 
           & currInteraction == lniSequence[j]) {
      seqLength <- seqLength+1
      j <- j+1
    }
    if (seqLength > 1) {
      oneSequences <- c(oneSequences, seqLength)
    }
    i <- j
  }
  oneSequences <- oneSequences[oneSequences >= 3]
  lni13 <- 100 * sum(oneSequences) / length(lniSequence)
  
  # 2-sequences
  twoSequences <- c()
  i <- 1
  while (i+1 <= length(lniSequence)) {
    j <- i+1
    k <- i+2
    currInteraction <- lniSequence[i:j]
    if (currInteraction[1] == -1
        | currInteraction[2] == -1
        | currInteraction[1] == currInteraction[2]) {
      i <- i+1
      j <- j+1
      k <- k+1
      next
    }
    seqLength <- 2
    alternatingIndex <- 0
    while (k <= length(lniSequence)
           & lniSequence[k] == currInteraction[(alternatingIndex%%2)+1]) {
      seqLength <- seqLength+1
      k <- k+1
      alternatingIndex <- alternatingIndex+1
    }
    twoSequences <- c(twoSequences, seqLength)
    i <- k
  }
  twoSequences <- twoSequences[twoSequences >= 3]
  lni23 <- 100 * sum(twoSequences) / length(lniSequence)
  
  # 3-sequences
  threeSequences <- c()
  i <- 1
  while (i+2 <= length(lniSequence)) {
    j <- i+1
    k <- i+2
    l <- i+3
    currInteraction <- lniSequence[i:k]
    if (currInteraction[1] == -1
        | currInteraction[2] == -1
        | currInteraction[3] == -1
        | currInteraction[1] == currInteraction[2]
        | currInteraction[1] == currInteraction[3]
        | currInteraction[2] == currInteraction[3]) {
      i <- i+1
      j <- j+1
      k <- k+1
      l <- l+1
      next
    }
    seqLength <- 3
    alternatingIndex <- 0
    while (l <= length(lniSequence)
           & lniSequence[l] == currInteraction[(alternatingIndex%%3)+1]) {
      seqLength <- seqLength+1
      l <- l+1
      alternatingIndex <- alternatingIndex+1
    }
    threeSequences <- c(threeSequences, seqLength)
    i <- k
  }
  threeSequences <- threeSequences[threeSequences >= 3]
  lni33 <- 100 * sum(threeSequences) / length(lniSequence)
  
  return(data.frame(lni13, lni23, lni33, (100 - sum(lni13, lni23, lni33))))
}


########################################## PLOTS / EXPORTS ###########################################
#----------------------------------------------------------------------------------------------------#
# function: plotInteractionPatterns
#   Plots the interaction patterns for the given VOD data.
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
    singleVodData <- singleVodData[2:length(singleVodData$round),2:4]
    rownames(singleVodData) <- seq(length=nrow(singleVodData))
    
    p1Cooperations <- data.frame("which" = numeric(1), "X1" = numeric(1))
    if (length(which(singleVodData[,1] == 0)) < length(singleVodData$player1)) {
      p1Cooperations <- data.frame(which(singleVodData[,1] == 1), 1)
    }
    p2Cooperations <- data.frame("which" = numeric(1), "X2" = numeric(1))
    if (length(which(singleVodData[,2] == 0)) < length(singleVodData$player2)) {
      p2Cooperations <- data.frame(which(singleVodData[,2] == 1), 2)
    }
    p3Cooperations <- data.frame("which" = numeric(1), "X3" = numeric(1))
    if (length(which(singleVodData[,3] == 0)) < length(singleVodData$player3)) {
      p3Cooperations <- data.frame(which(singleVodData[,3] == 1), 3)
    }
    
    # basic plot - including cooperation data for player 1
    plot(p1Cooperations, ann=FALSE, xaxt = "n", yaxt = "n", 
         type = 'p', pch = 'x',
         xlim = range(1:nrow(singleVodData)), ylim = range(0.5:3.5))
    # y-axis per plot
    axis(side=2,at=seq(0,3,1),labels=seq(0,3,1), cex.axis = 0.7)
    # adding cooperation data for players 2 and 3
    points(p2Cooperations, type = 'p', pch = 'x')
    points(p3Cooperations, type = 'p', pch = 'x')
    # adding horizontal lines
    segments(x0 = 0, x1 = nrow(singleVodData), y0 = c(1,2,3), col = "gray60")
    
    if (i == length(vodData)) {
      # overall x-axis
      mtext(side=1,"Period",line=2.5)       
      axis(side=1,at=seq(0,nrow(singleVodData),1),labels=seq(0,nrow(singleVodData),1), 
           cex.axis = 0.7)
      # overall y-axis label
      mtext('Group members\' decision across sessions (x = \'cooperation\')', side = 2, 
            outer = TRUE, line = -1.8)
    }
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
          beside = TRUE, col = cols, ylim = range(0:80))
  
  # compare model and experimental data: Asymmetric 1
  plotDataAsym1 <- data.frame(h1 = LNIs$asym1_h1,
                         h2 = LNIs$asym1_h2,
                         h3 = LNIs$asym1_h3,
                         others = LNIs$asym1_others)
  barplot(as.matrix(plotDataAsym1), yaxt = "n", xlab = "Asymmetric 1", 
          beside = TRUE, col = cols, ylim = range(0:80))
  
  # compare model and experimental data: Asymmetric 2
  plotDataAsym2 <- data.frame(h1 = LNIs$asym2_h1,
                              h2 = LNIs$asym2_h2,
                              h3 = LNIs$asym2_h3,
                              others = LNIs$asym2_others)
  barplot(as.matrix(plotDataAsym2), yaxt = "n", xlab = "Asymmetric 2", 
          beside = TRUE, col = cols, ylim = range(0:80))

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
  plotGOF(meanLNIs)
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
#     param:  date
#         the date of the simulation, format: 'yyyymmdd'
#         (if undefined: latest available date)
#     param:  dateCount
#         defines the number of the performed simulation of the given date
#         (if undefined: latest available simulation)
#     param:  vodType
#         the type of VOD used for the simulation
#         possible: 'all', constants.R: 'VOD_TYPES[x]'
#----------------------------------------------------------------------------------------------------#
analyzeData <- function(modelType = MODEL_TYPES[3],
                        date = "latest",
                        dateCount = "latest",
                        vodType = "all") {
  
  # initializations
  exportDir <- paste(getVodBaseDir(modelType = modelType, 
                                   date = date, 
                                   dateCount = dateCount),
                     "/", sep = "")
  if (vodType == "all") {
    vodType <- VOD_TYPES
  }
  LNIs <- data.frame()
  
  # looping over the VOD types to be analyzed (e.g.: sym -> asym1 -> asym2)
  for (i in 1:length(vodType)) {
    currVodType <- vodType[i]
    
    # importing the required simulated data
    vodSimData <- importVodSimData(modelType = modelType, date = date, 
                                   dateCount = dateCount, vodType = currVodType)
    
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
                               (paste(currVodType, "_others", sep = "")))
    
    # column binding of LNIs for all different VOD types
    if (nrow(LNIs) == 0) {
      LNIs <- vodTypeLNIs
    } else {
      LNIs <- cbind(LNIs, vodTypeLNIs)
    }
  }
  
  # computation of mean LNIs
  meanLNIs <- colMeans(LNIs)
  
  # exporting Goodness of Fit
  exportGOF(exportDir, meanLNIs)
  
  # exporting LNI comparison data (model vs. experiment)
  exportLNIComparison(exportDir, meanLNIs)
  
}


###################################### EVENTUAL INTEGRITY CHECKS #####################################
source(paste(BASE_DIR, "/tests/testAnalysis.R", sep = ""))
