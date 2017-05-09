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
  lni43 <- computeLNISequence(4, lniSequence, higherOrder = TRUE)
  # 5-sequences
  lni53 <- computeLNISequence(5, lniSequence, higherOrder = TRUE)
  # 6-sequences
  lni63 <- computeLNISequence(6, lniSequence, higherOrder = TRUE)
  # 7-sequences
  lni73 <- computeLNISequence(7, lniSequence, higherOrder = TRUE)
  # 8-sequences
  lni83 <- computeLNISequence(8, lniSequence, higherOrder = TRUE)
  # 9-sequences
  lni93 <- computeLNISequence(9, lniSequence, higherOrder = TRUE)
  
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
#       Przepiorka (2016), or for higher order (h3+) patterns
#----------------------------------------------------------------------------------------------------#
computeLNISequence <- function(seqLength, lniSequence, higherOrder = FALSE) {
  
  sequences <- c()
  i <- 1
  
  while (i+(seqLength-1) <= length(lniSequence)) {
    j <- i+(seqLength-1)
    k <- i+(seqLength)
    currInteraction <- lniSequence[i:j]
    
    skip <- FALSE
    y <- 1    
    while (!skip & y <= seqLength) {
      if (currInteraction[y] == -1) {      
        skip <- TRUE
      }
      y <- y+1
    }
    
    if (!higherOrder) {
      y <- 1
      while (!skip & seqLength > 1 & y < seqLength) {
        z <- y+1
        while (!skip & seqLength > 1 & z <= seqLength) {
          if (currInteraction[y] == currInteraction[z]) {
            skip <- TRUE
          }
          z <- z+1
        }
        y <- y+1
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
  # end: 
  
  sequences <- sequences[sequences >= 3]
  return(100 * sum(sequences) / length(lniSequence))
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
analyzeData <- function(modelType = MODEL_TYPES[1],
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
  comparableKeeps <- c()
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
    
    # column binding of LNIs for all different VOD types
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
  
}


###################################### EVENTUAL INTEGRITY CHECKS #####################################
source(paste(BASE_DIR, "/tests/testAnalysis.R", sep = ""))
