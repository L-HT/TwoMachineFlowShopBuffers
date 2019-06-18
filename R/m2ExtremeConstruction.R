
# grobe Laufzeitanalyse:
# im Worst-case (keine zwei Jobs passen in den Speicher)
# muss jede Teilpermutation komplett durchlaufen werden,
# also hat man 2+3+4+...+n Additionen und Vergleiche.
# ist also quadratisch, denke ich
# ist auch echt polynomiell, nicht pseudopolynomiell

#' @export
m1DominateConstructSolution <- function(jobData, maxBufferSize){
  sortedJobs <- jobData[order((jobData$m1Time + jobData$m2Time), decreasing = T),]

  # triviale Fälle wie "alle Jobs können sich gegenseitig verstecken" oder "kein Verstecken möglich"
  # sind hier weggelassen. Das zu checken, braucht quadratische Zeit?

  k_min <- 0
  compatibleJobFound <- FALSE
  while (!compatibleJobFound & k_min+1 < nrow(jobData)){
    k_min <- k_min + 1
    if (sortedJobs$bufferUsage[k_min+1] + sortedJobs$bufferUsage[k_min] <= maxBufferSize ) {
      compatibleJobFound <- TRUE
    }
  }

  if (k_min == 1 & compatibleJobFound){
    # jeder Job kann versteckt werden; dann mit dem kürzesten Job beginnen
    return(rev(sortedJobs$job))
  }

  if (!compatibleJobFound){
    # kein Job kann versteckt werden
    # dann ist jede Permutation optimal
    return(sortedJobs$job)
  }

  # sonst-Fall

  sortedJobs$fx <- ""

  k_max <- nrow(jobData)
  for (i in 1:(k_min - 1)){

    if (sortedJobs$bufferUsage[k_max] + sortedJobs$bufferUsage[i] <= maxBufferSize){

      sortedJobs$fx[i] <- sortedJobs$job[k_max]
      k_max <- k_max - 1
    }
  }

  if (k_max == k_min){
    # alle R-Jobs wurden zugewiesen
    result <- c()
    for (j in k_min:1){
      if (sortedJobs$fx[j] == ""){
        result <- c(result,sortedJobs$job[j])
      } else {
        result <- c(result, sortedJobs$fx[j], sortedJobs$job[j])
      }
    }
    return(result)
  } else {
    foundH <- FALSE
    for (h in (k_min-1):1){
      if (sortedJobs$fx[h] == ""){
        foundH <- TRUE
        break
      }
    }
    if (!foundH){
      h <- 0
    }

    tempJobSet <- sortedJobs[(k_min+1):nrow(jobData),]
    for (ll in 1:h){
      tempPos <- which(sortedJobs$fx[ll] == tempJobSet$job)
      if (length(tempPos) != 0){
        tempJobSet <- tempJobSet[-tempPos,]
      }
    }
    tempJobSet$marked <- FALSE
    #tempJobSet <- tempJobSet[!tempJobSet %in% sortedJobs$fx,]


    for (i in (k_min-1):(h+1)){

      for (r in 1:nrow(tempJobSet)){
        if (tempJobSet$bufferUsage[r] + sortedJobs$bufferUsage[i] <= maxBufferSize & !tempJobSet$marked[r]){
          sortedJobs$fx[i] <- tempJobSet$job[r]
          tempJobSet$marked[r] <- TRUE
          break
        }
      }
    }


    # smallest unassigned job in R
    k_max <- nrow(jobData)
    for (r in k_max:(k_min+1)){
      if (!sortedJobs$job[r] %in% sortedJobs$fx){
        sortedJobs$fx[k_min] <- sortedJobs$job[r]
        break
      }
    }

    # get set R
    R_jobs <- sortedJobs$job[(k_min+1):nrow(sortedJobs)]
    indicesToRemove <- c()
    for (i in 1:length(R_jobs)){
      if (R_jobs[i] %in% sortedJobs$fx){
        indicesToRemove <- c(indicesToRemove,i)
      }
    }
    R_jobs <- R_jobs[-indicesToRemove]
    result <- c(sortedJobs$fx[k_min], R_jobs, sortedJobs$job[k_min])
    for (j in (k_min-1):1){
      if (sortedJobs$fx[j] == ""){
        result <- c(result, sortedJobs$job[j])
      } else {
        result <- c(result, sortedJobs$fx[j], sortedJobs$job[j])
      }
    }
    return(result)
  }
  warning("m2ExtremeConstruction: Code is not supposed to reach this line...")
  return(rev(sortedJobs$job))
}

#' @export
getM2ExtremeOptTime <- function(jobData, maxBufferSize){
  startTime <- Sys.time()
  temp <- m1DominateConstructSolution(jobData, maxBufferSize)
  endTime <- Sys.time()
  rm(temp)
  return(as.numeric(endTime-startTime))
}

#' @export
m1DominateConstructSolution_NEH <- function(jobData, maxBufferSize){
  sortedJobs <- jobData[order((jobData$m1Time + jobData$m2Time), decreasing = T),]
  resultIndices <- c()

  for (k in 1:nrow(sortedJobs)){
    rightMostFitPos <- 0
    if (length(resultIndices) == 0){
      resultIndices <- c(k)
    } else {

      for (l in length(resultIndices):1){
        if (l == 1){
          resultIndices <- c(k, resultIndices[l:length(resultIndices)])
        } else {
          if (sortedJobs$bufferUsage[resultIndices[l]] + sortedJobs$bufferUsage[k] <= maxBufferSize &
              sortedJobs$bufferUsage[resultIndices[l]] + sortedJobs$bufferUsage[resultIndices[l-1]] > maxBufferSize){
            resultIndices <- c(resultIndices[0:(l-1)], k, resultIndices[l:length(resultIndices)])
            break
          }
        }
      }

    }

  }
  result <- sortedJobs$job[resultIndices]
  return(result)
}

#' @export
getM2ExtremeOptTime_NEH <- function(jobData, maxBufferSize){
  startTime <- Sys.time()
  temp <- m1DominateConstructSolution_NEH(jobData, maxBufferSize)
  endTime <- Sys.time()
  rm(temp)
  return(as.numeric(endTime-startTime))
}
