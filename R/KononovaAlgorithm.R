#smallPathProblem <- jobData

#' @export
generateKKInstance <- function(numberOfJobs, m1Range, m2Range, bufferUsage = -1){
  jobData <- generateProblemInstance(numberOfJobs, m1Range, m2Range, bufferUsage)
  return(calculateKKData(jobData))
}

#' @export
calculateKKData <- function(jobData){
  #jobData <- generateProblemInstance(50,c(5,50),c(5,50), -1)
  johnson <- getJohnsonPermutationA(jobData)
  simData <- simulateFlowShopTotalBuffer(jobData, johnson, johnson, sum(jobData$bufferUsage))
  criticalPath <- calculateCriticalPath(jobData, johnson, sum(jobData$bufferUsage))
  print(criticalPath)

  maxBuffer <- findBufferUsageMaximum(simData, jobData)

  #neue Permutation
  #for (i in 1:10){
  johnson2 <- generateNewPermutation(criticalPath, johnson)
  simData2 <- simulateFlowShopTotalBuffer(jobData, johnson2, johnson2, sum(jobData$bufferUsage))
  if (getMakespan(simData) != getMakespan(simData2)){
    print(criticalPath)
    stop("KK: Makespan hat sich geändert!")
  }
  maxBuffer2 <- findBufferUsageMaximum(simData2, jobData)
  if (maxBuffer2 < maxBuffer){
    # Bufferänderung: dann ist das mit "auf A so spät, auf B so früh" wohl schiefgegangen
    message("KK: Buffer-Nutzung hat sich geändert.")
    maxBuffer <- min(maxBuffer, maxBuffer2)
  }
  #print(findBufferUsageMaximum(simData2, jobData))
  #}
  return(
    list("jobData" = jobData,
         "maxBuffer" = maxBuffer,
         "oneOptimalSolution" = johnson2,
         "optimalMakespan" = getMakespan(simData)
    )
  )
}


generateNewPermutation <- function(criticalPath, johnson){
  critical <- johnson[criticalPath["start"] : criticalPath["end"]]
  left <- johnson[1:(criticalPath["start"] - 1)]
  right <- johnson[(criticalPath["end"]+1) : length(johnson)]
  left <- sample(left)
  right <- sample(right)
  newJohnson <- c(left, critical, right)
  return(newJohnson)
}


findBufferUsageMaximum <- function(simData, jobData){
  bufferValues <- jobData[,c("job", "bufferUsage")]
  sub <- simData[simData$type == "buffer",]
  timeTable <- merge(sub, bufferValues, by.x = "job", by.y = "job")
  end <- max(timeTable$end)

  result <- 0
  bufferUsage <- c()
  for (i in 0:end){
    bufferUsage <- c(bufferUsage, sum(timeTable[i >= timeTable$start & i <= timeTable$end, "bufferUsage"]))
  }
  return(max(bufferUsage))
}

calculateCriticalPathOld <- function(jobData, johnson, bufferSize){
  start <- 1
  end <- length(johnson)
  optChanged <- FALSE

  tempLeftCounter <- 1
  tempRightCounter <- 2

  opt <- simulateFlowShopTotalBufferC(jobData, johnson, johnson, bufferSize)

  while(!optChanged){
    permNew <- switchJobs(johnson, start, tempRightCounter)
    tempMakespan <- simulateFlowShopTotalBufferC(jobData, permNew, johnson, bufferSize)
    #tempMakespan <- getMakespan(simulateFlowShopTotalBuffer(jobData, permNew, johnson, bufferSize))
    if (tempMakespan > opt){
      start <- tempRightCounter
      optChanged <- TRUE
    }
    tempRightCounter <- tempRightCounter + 1
    if (tempRightCounter > length(johnson) && !optChanged){
      stop("Kritischer Pfad nicht da?")
    }
  }
  tempRightCounter <- end - 1
  optChanged <- FALSE

  while(!optChanged){
    permNew <- switchJobs(johnson, end, tempRightCounter)
    tempMakespan <- simulateFlowShopTotalBufferC(jobData, johnson, permNew, bufferSize)
    #tempMakespan <- getMakespan(simulateFlowShopTotalBuffer(jobData, johnson, permNew, bufferSize))
    if (tempMakespan > opt){
      end <- tempRightCounter
      optChanged <- TRUE
    }
    tempRightCounter <- tempRightCounter - 1
    if (tempRightCounter < start && !optChanged){
      optChanged <- TRUE
      end <- start
    }
  }
  return(c("start" = start, "end" = end))
}


calculateCriticalPath <- function(jobData, johnson, bufferSize){
  start <- 1
  end <- length(johnson)
  optChanged <- FALSE

  tempLeftCounter <- 1
  tempRightCounter <- 2

  opt <- simulateFlowShopTotalBufferC(jobData, johnson, johnson, bufferSize)

  while(!optChanged){
    permNew <- switchJobs(johnson, start, tempRightCounter)
    tempMakespan <- simulateFlowShopTotalBufferC(jobData, johnson, permNew, bufferSize)
    #tempMakespan <- getMakespan(simulateFlowShopTotalBuffer(jobData, permNew, johnson, bufferSize))
    if (tempMakespan > opt){
      start <- tempRightCounter
      optChanged <- TRUE
    }
    tempRightCounter <- tempRightCounter + 1
    if (tempRightCounter > length(johnson) && !optChanged){
      stop("Kritischer Pfad nicht da?")
    }
  }
  tempRightCounter <- end - 1
  optChanged <- FALSE

  while(!optChanged){
    permNew <- switchJobs(johnson, end, tempRightCounter)
    tempMakespan <- simulateFlowShopTotalBufferC(jobData,  permNew, johnson, bufferSize)
    #tempMakespan <- getMakespan(simulateFlowShopTotalBuffer(jobData, johnson, permNew, bufferSize))
    if (tempMakespan > opt){
      end <- tempRightCounter
      optChanged <- TRUE
    }
    tempRightCounter <- tempRightCounter - 1
    if (tempRightCounter < start && !optChanged){
      optChanged <- TRUE
      end <- start
    }
  }
  return(c("start" = start, "end" = end))
}

switchJobs <- function(permutation, j1, j2){
  temp <- permutation[j1]
  permutation[j1] <- permutation[j2]
  permutation[j2] <- temp
  return(permutation)
}
