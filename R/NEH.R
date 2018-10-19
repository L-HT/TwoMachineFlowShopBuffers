
#' @export
getNEHSolution <- function(jobData, bufferType, maxBufferSize, subType,
                           targetCriterion = "makespan"){
  # print(bufferType)
  # print(maxBufferSize)
  # print(subType)
  counter <- 0
  if (subType == 0){
    sortedJobs <- jobData[order((jobData$m1Time + jobData$m2Time), decreasing = T),"job"]
  }
  if (subType == 1){
    sortedJobs <- jobData[order((jobData$m1Time + jobData$m2Time), decreasing = F),"job"]
  }
  if (subType > 1){
    sortedJobs <- paste("j", sample(1:nrow(jobData)), sep="")
  }

  result <- sortedJobs[c(1,2)]
  bestMakespan <- getMakespanOfSimulation(jobData, result, result, maxBufferSize, bufferType,
                                          targetCriterion)
  counter <- counter + 1

  result <- sortedJobs[c(2,1)]
  currentMakespan <- getMakespanOfSimulation(jobData, result, result, maxBufferSize, bufferType,
                                             targetCriterion)
  counter <- counter + 1

  if (bestMakespan < currentMakespan){
    result <- sortedJobs[c(1,2)]
  }

  currentMakespan <- Inf
  bestMakespan <- Inf
  bestIndex <- 1

  for (job in sortedJobs[-c(1,2)]){

    for (k in 1:length(result)){
      tempSequence <- c(result[0:(k-1)], job, result[(k):length(result)])
      currentMakespan <- getMakespanOfSimulation(jobData, tempSequence, tempSequence, maxBufferSize, bufferType,
                                                 targetCriterion)
      counter <- counter + 1

      if (currentMakespan < bestMakespan){
        bestMakespan <- currentMakespan
        bestIndex <- k
      }
    }
    tempSequence <- c(result, job)
    currentMakespan <- getMakespanOfSimulation(jobData, tempSequence, tempSequence, maxBufferSize, bufferType,
                                               targetCriterion)
    counter <- counter + 1

    if (currentMakespan < bestMakespan){
      bestMakespan <- currentMakespan
      bestIndex <- length(result) + 1
    }

    if (bestIndex == length(result) + 1){
      result <- c(result, job)
    } else {
      result <- c(result[0:(bestIndex-1)], job, result[(bestIndex):length(result)])
      # result <- c(result[0:(k-1)], job, result[(k):length(result)])

    }
    # print(result)

    bestMakespan <- Inf
  }
  # hier: verwerfe das "j" am Anfang
  # (das kommt davon, wenn man seinen Code nicht so gut durchdacht hat...)
  print("Evaluationen fuer NEH:")
  print(counter)
  result <- as.integer(substring(result, 2))
  return(result)
}

#' @export
getNEHTime <- function(jobData, bufferType, maxBufferSize, subType){
  startTime <- Sys.time()
  temp <- getNEHSolution(jobData, bufferType, maxBufferSize, subType)
  endTime <- Sys.time()
  rm(temp)
  return(as.numeric(endTime-startTime))
}

#' @export
getNEHEvaluations <- function(jobData){
  numberOfJobs <- nrow(jobData)
  result <- numberOfJobs - 1 + numberOfJobs*(numberOfJobs-1) / 2;
  return(result)
}



