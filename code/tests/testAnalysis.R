#----------------------------------------------------------------------------------------------------#
# function: createVodTestData1
#     Creates the first LNI example data, as in Diekmann & Przepiorka (2016), p.1318
#----------------------------------------------------------------------------------------------------#
createVodTestData1 <- function() {
  round <- c(0,1,2,3,4,5,6,7,8,9,10)
  player1 <- c(NA,DEVIATE,COOPERATE,COOPERATE,DEVIATE,DEVIATE,
               COOPERATE,DEVIATE,COOPERATE,DEVIATE,DEVIATE)
  player2 <- c(NA,COOPERATE,DEVIATE,DEVIATE,DEVIATE,COOPERATE,
               DEVIATE,DEVIATE,DEVIATE,COOPERATE,DEVIATE)
  player3 <- c(NA,DEVIATE,DEVIATE,DEVIATE,COOPERATE,DEVIATE,
               DEVIATE,DEVIATE,DEVIATE,DEVIATE,COOPERATE)
  util1 <- c(0,100,60,60,100,100,60,0,60,100,100)
  util2 <- c(0,60,100,100,100,60,100,0,100,60,100)
  util3 <- c(0,100,100,100,60,100,100,0,100,100,60)
  return(data.frame(round,player1,player2,player3,util1,util2,util3))
}

#----------------------------------------------------------------------------------------------------#
# function: createVodTestData2
#     Creates the second LNI example data, as in Diekmann & Przepiorka (2016), p.1318
#----------------------------------------------------------------------------------------------------#
createVodTestData2 <- function() {
  round <- c(0,1,2,3,4,5,6,7,8,9,10)
  player1 <- c(NA,COOPERATE,DEVIATE,COOPERATE,DEVIATE,COOPERATE,
               DEVIATE,DEVIATE,DEVIATE,COOPERATE,DEVIATE)
  player2 <- c(NA,COOPERATE,COOPERATE,DEVIATE,COOPERATE,DEVIATE,
               COOPERATE,COOPERATE,COOPERATE,DEVIATE,DEVIATE)
  player3 <- c(NA,DEVIATE,DEVIATE,DEVIATE,DEVIATE,DEVIATE,DEVIATE,
               DEVIATE,DEVIATE,DEVIATE,COOPERATE)
  util1 <- c(0,60,100,60,100,60,100,100,100,60,100)
  util2 <- c(0,60,60,100,60,100,60,60,60,100,100)
  util3 <- c(0,100,100,100,100,100,100,100,100,100,60)
  return(data.frame(round,player1,player2,player3,util1,util2,util3))
}

#----------------------------------------------------------------------------------------------------#
# function: createLNITestSequence1
#     Creates an LNI test sequence
#----------------------------------------------------------------------------------------------------#
createLNITestSequence1 <- function() {
  return(c(1,1,2,2,2,3,3,3,3,1,2,3,2,2,2,2,2,2,2))
}

#----------------------------------------------------------------------------------------------------#
# function: createVodTestData3
#     Creates the second LNI example data, as in Diekmann & Przepiorka (2016), p.1318
#----------------------------------------------------------------------------------------------------#
createVodTestData2 <- function() {
  round <- c(0,1,2,3,4,5,6,7,8,9,10)
  player1 <- c(NA,COOPERATE,DEVIATE,COOPERATE,DEVIATE,COOPERATE,
               DEVIATE,DEVIATE,DEVIATE,COOPERATE,DEVIATE)
  player2 <- c(NA,COOPERATE,COOPERATE,DEVIATE,COOPERATE,DEVIATE,
               COOPERATE,COOPERATE,COOPERATE,DEVIATE,DEVIATE)
  player3 <- c(NA,DEVIATE,DEVIATE,DEVIATE,DEVIATE,DEVIATE,DEVIATE,
               DEVIATE,DEVIATE,DEVIATE,COOPERATE)
  util1 <- c(0,60,100,60,100,60,100,100,100,60,100)
  util2 <- c(0,60,60,100,60,100,60,60,60,100,100)
  util3 <- c(0,100,100,100,100,100,100,100,100,100,60)
  return(data.frame(round,player1,player2,player3,util1,util2,util3))
}

#----------------------------------------------------------------------------------------------------#
# function: createLNITestSequence2
#     Creates an LNI test sequence
#----------------------------------------------------------------------------------------------------#
createLNITestSequence2 <- function() {
  return(c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1))
}

#----------------------------------------------------------------------------------------------------#
# function: testAnalysis
#     Starting point for test analysis.
#----------------------------------------------------------------------------------------------------#
testAnalysis <- function() {
  
  # test case 1
  vodTestData <- createVodTestData1()
  lniSequence <- extractLNISequence(vodTestData)
  lnis <- computeLNIs(lniSequence)
  if (lnis$lni13 != 0) stop(paste("Error during computation of test case 1:\n",
                                  "\texpected value for lni13: 0\n",
                                  "\tcomputed value for lni13:", lnis$lni13))
  if (lnis$lni23 != 0) stop(paste("Error during computation of test case 1:\n",
                                  "\texpected value for lni23: 0\n",
                                  "\tcomputed value for lni23:", lnis$lni23))
  if (lnis$lni33 != 70) stop(paste("Error during computation of test case 1:\n",
                                   "\texpected value for lni33: 70\n",
                                   "\tcomputed value for lni33:", lnis$lni33))
  
  # test case 2
  vodTestData <- createVodTestData2()
  lniSequence <- extractLNISequence(vodTestData)
  lnis <- computeLNIs(lniSequence)
  if (lnis$lni13 != 30) stop(paste("Error during computation of test case 2:\n",
                                   "\texpected value for lni13: 30\n",
                                   "\tcomputed value for lni13:", lnis$lni13))
  if (lnis$lni23 != 50) stop(paste("Error during computation of test case 2:\n",
                                   "\texpected value for lni23: 50\n",
                                   "\tcomputed value for lni23:", lnis$lni23))
  if (lnis$lni33 != 30) stop(paste("Error during computation of test case 2:\n",
                                   "\texpected value for lni33: 30\n",
                                   "\tcomputed value for lni33:", lnis$lni33))
  
  # test case 3
  lnis <- computeLNIs(createLNITestSequence1())
  if (round(lnis$lni13, 3) != 73.684) stop(paste("Error during computation of test case 2:\n",
                                                 "\texpected value for lni13: 73.684\n",
                                                 "\tcomputed value for lni13:", round(lnis$lni13, 3)))
  if (round(lnis$lni23, 3) != 15.789) stop(paste("Error during computation of test case 2:\n",
                                                 "\texpected value for lni23: 15.789\n",
                                                 "\tcomputed value for lni23:", round(lnis$lni23, 3)))
  if (round(lnis$lni33, 3) != 21.053) stop(paste("Error during computation of test case 2:\n",
                                                 "\texpected value for lni33: 21.053\n",
                                                 "\tcomputed value for lni33:", round(lnis$lni33, 3)))
  
  # test case 4  
  lnis <- computeLNIs(createLNITestSequence2())
  if (lnis$lni13 != 0) stop(paste("Error during computation of test case 2:\n",
                                  "\texpected value for lni13: 0\n",
                                  "\tcomputed value for lni13:", round(lnis$lni13, 3)))
  if (lnis$lni23 != 0) stop(paste("Error during computation of test case 2:\n",
                                  "\texpected value for lni23: 0\n",
                                  "\tcomputed value for lni23:", round(lnis$lni23, 3)))
  if (lnis$lni33 != 0) stop(paste("Error during computation of test case 2:\n",
                                  "\texpected value for lni33: 0\n",
                                  "\tcomputed value for lni33:", round(lnis$lni33, 3)))
  
  # success message at the end of the test analysis
  print("Success: Test analysis completed.")
}
testAnalysis()
