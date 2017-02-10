########################################## GLOBAL PARAMETERS #########################################
# file system
BASE_DIR <- paste(dirname(sys.frame(1)$ofile), "/simulations/", sep = "")
BASE_FILENAME <- "vod-simulation-"


#----------------------------------------------------------------------------------------------------#
# function: importVodSimData
#     Imports VOD simulation data. Data is stored as: "vod-simulation-MODELTYPE-TIMESTAMP.Rdata"
#     Available MODELTYPEs: "default", "reinf", "decl-mem", "mel-vs-max"
#     TIMESTAMP format: yyyymmdd-hhmmss
#     param:  modelType
#         the model type used by the simulation
#     param:  timestamp
#         the timestamp of the simulation (if undefined: latest available timestamp)
#----------------------------------------------------------------------------------------------------#
importVodSimData <- function(modelType = "default", timestamp = "latest") {
  filename <- paste(BASE_DIR, BASE_FILENAME, modelType, "-", timestamp, ".Rdata", sep = "")
  # if timestamp is undefined, take latest available timestamp
  if (timestamp == "latest") {
    files <- list.files(BASE_DIR)
    fileTimestamps <- gsub(".Rdata", "", 
                           gsub(paste(BASE_FILENAME, modelType, "-", sep = ""), "", 
                                files[grepl(modelType, files)], fixed = TRUE), fixed = TRUE)
    maxTimestamp <- NA
    for (i in 1:length(fileTimestamps)) {
      if (is.na(maxTimestamp)) {
        maxTimestamp <- fileTimestamps[i]
      } else if (fileTimestamps[i] > maxTimestamp) {
        maxTimestamp <- fileTimestamps[i]
      }
    }
    filename <- paste(BASE_DIR, BASE_FILENAME, modelType, "-", maxTimestamp, ".Rdata", sep = "")
  }
  vodSimData <- get(load(filename))
  print(paste("Success: Data import from:", filename))
  return(vodSimData)
}

#----------------------------------------------------------------------------------------------------#
# function: computeLNI
#   Computation of the Latent Norm Index (LNI) for the given VOD data.
#   param:  vodData
#       the VOD data 
#----------------------------------------------------------------------------------------------------#
computeLNI <- function(vodData) {
  vodData
}

#----------------------------------------------------------------------------------------------------#
# function: analyzeData
#     Starting point for the data analysis.
#----------------------------------------------------------------------------------------------------#
analyzeData <- function() {
  vodSimData <- importVodSimData(modelType = "reinf")
  vodSimData <- importVodSimData(modelType = "reinf", timestamp = "20170208-115642")
  vodSimData <- importVodSimData()
  computeLNI(vodSimData)
}

