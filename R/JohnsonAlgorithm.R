#' @export
getJohnsonPermutationA <- function(jobData){
  splitVector <- jobData$m1Time < jobData$m2Time
  partA <- jobData[splitVector, 1:3]
  partA <- partA[order(partA$m1Time, decreasing = F), ]

  partB <- jobData[!splitVector, 1:3]
  partB <- partB[order(partB$m2Time, decreasing = T), ]

  return(c(partA$job, partB$job))
}

#' @export
getJohnsonPermutationInt <- function(jobData){
  perm <- getJohnsonPermutationA(jobData)
  result <- as.integer(substring(perm, 2))
  return(result)
}

#' @export
getJohnsonPermutationB <- function(jobData){
  n <- nrow(jobData)
  leftCounter <- 1
  rightCounter <- n
  result <- rep("0", n)
  m <- n

  jobList <- jobData$job
  timesList <- c(jobData$m1Time, jobData$m2Time)
  for (i in 1:n){
    tempMin <- which.min(timesList)

    if (tempMin > m){ #auf M2
      result[rightCounter] <- jobList[tempMin - m]
      rightCounter <- rightCounter - 1
      timesList <- timesList[-c(tempMin, tempMin - m)]
      jobList <- jobList[-c(tempMin - m)]
    } else { #auf M1
      result[leftCounter] <- jobList[tempMin]
      leftCounter <- leftCounter + 1
      timesList <- timesList[-c(tempMin, tempMin + m)]
      jobList <- jobList[-c(tempMin)]
    }
    m <- m - 1
  }

  return(result)
}

#' @export
getJohnsonBound <- function(jobData){
  perm <- getJohnsonPermutationB(jobData)
  maxBufferSize <- sum(jobData$bufferUsage)
  result <- simulateFlowShopC(jobData, perm, perm, maxBufferSize, TRUE)
  return(result)
}


