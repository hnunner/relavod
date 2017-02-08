########################################## GLOBAL PARAMETERS #########################################
# file system
BASE_DIR <- paste(dirname(sys.frame(1)$ofile), "/simulations/", sep = "")
BASE_FILENAME <- "vod-simulation-"
# simulation type
SIM_TYPE <- "default"    # possible: "default", "reinf", "decl-mem", "mel-vs-max"



loadLatestSimulationData <- function() {
  files <- list.files(BASE_DIR)
  fileTimestamps <- gsub(".Rdata", "", 
                         gsub(paste(BASE_FILENAME, SIM_TYPE, "-", sep = ""), "", 
                              files[grepl(SIM_TYPE, files)], fixed = TRUE), fixed = TRUE)
  maxTimestamp <- NA
  for (i in 1:length(fileTimestamps)) {
    if (is.na(maxTimestamp)) {
      maxTimestamp <- fileTimestamps[i]
    } else if (fileTimestamps[i] > maxTimestamp) {
      maxTimestamp <- fileTimestamps[i]
    }
  }
  filename <- paste(BASE_DIR, BASE_FILENAME, SIM_TYPE, "-", maxTimestamp, ".Rdata", sep = "")
  load(filename)
}

