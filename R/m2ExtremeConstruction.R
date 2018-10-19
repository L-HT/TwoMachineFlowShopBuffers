
# grobe Laufzeitanalyse:
# im Worst-case (keine zwei Jobs passen in den Speicher)
# muss jede Teilpermutation komplett durchlaufen werden,
# also hat man 2+3+4+...+n Additionen und Vergleiche.
# ist also quadratisch, denke ich
# ist auch echt polynomiell, nicht pseudopolynomiell

#' @export
m1DominateConstructSolution <- function(jobData, maxBufferSize){
  sortedJobs <- jobData[order((jobData$m1Time + jobData$m2Time), decreasing = T),"job"]
  result <- c(sortedJobs[c(2,1)])

  for (job in sortedJobs[-c(1,2)]){
    currentM1Time <- jobData[jobData$job == job, "m1Time"]
    insertPos <- 0
    k <- length(result)
    jobFits <- FALSE

    # neue Idee: Löse den am weitesten rechts liegenden,
    # aber noch lösbaren Konflikt

    # Konflikt suchen

    k <- 0
    for (i in length(result):2){
      time1 <-jobData[jobData$job == result[i-1], "m1Time"]
      time2 <-jobData[jobData$job == result[i], "m1Time"]

      if (time1 + time2 > maxBufferSize){
        # Konflikt liegt vor
        if (currentM1Time + time1 <= maxBufferSize &&
          time2 + currentM1Time <= maxBufferSize){
          k <- i
          break
        }
      }
    }

    # while (!jobFits && k > 1){
    #   m1TimeToBeCompared <- jobData[jobData$job == result[k], "m1Time"]
    #   if (currentM1Time + m1TimeToBeCompared <= maxBufferSize){
    #     jobFits <- TRUE
    #   } else {
    #     k <- k - 1
    #   }
    # }


    if (k > 0){
      result <- c(result[0:(k-1)], job, result[(k):length(result)])
    } else {
      result <- c(job, result)
    }

  }
  return(result)
}
