#smallPathProblem <- jobData

#' @export
generateKKInstanceIB <- function(numberOfJobs, m1Range, m2Range, bufferUsage = -1){
  jobData <- generateProblemInstance(numberOfJobs, m1Range, m2Range, bufferUsage)
  return(calculateKKDataIB(jobData))
}

#' @export
calculateKKDataIB <- function(jobData){
  johnson <- getJohnsonPermutationA(jobData)
  simData <- simulateFlowShop(jobData, johnson, johnson, sum(jobData$bufferUsage))
  criticalPath <- calculateCriticalPathIB(jobData, johnson, sum(jobData$bufferUsage))
  # print(criticalPath)

  maxBuffer <- findBufferUsageMaximum(simData, jobData)

  #neue Permutation
  #for (i in 1:10){
  johnson2 <- generateNewPermutation(criticalPath, johnson)
  simData2 <- simulateFlowShop(jobData, johnson2, johnson2, sum(jobData$bufferUsage))
  if (getMakespan(simData) != getMakespan(simData2)){
    print(criticalPath)
    stop("KKIB: Makespan hat sich geändert!")
  }
  maxBuffer2 <- findBufferUsageMaximum(simData2, jobData)
  if (maxBuffer2 < maxBuffer){
    # Bufferänderung: nochmal Paper anschauen
    message("KKIB: Buffer-Nutzung hat sich geändert.")
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
calculateCriticalPathIB <- function(jobData, johnson, bufferSize){
  start <- 1
  end <- length(johnson)
  optChanged <- FALSE

  tempLeftCounter <- 1
  tempRightCounter <- 2

  opt <- simulateFlowShopC(jobData, johnson, johnson, bufferSize)

  while(!optChanged){
    permNew <- switchJobs(johnson, start, tempRightCounter)
    tempMakespan <- simulateFlowShopC(jobData, permNew, johnson, bufferSize)
    #tempMakespan <- getMakespan(simulateFlowShop(jobData, permNew, johnson, bufferSize))
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
    tempMakespan <- simulateFlowShopC(jobData, johnson, permNew, bufferSize)
    #tempMakespan <- getMakespan(simulateFlowShop(jobData, johnson, permNew, bufferSize))
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

switchJobs <- function(permutation, j1, j2){
  temp <- permutation[j1]
  permutation[j1] <- permutation[j2]
  permutation[j2] <- temp
  return(permutation)
}

