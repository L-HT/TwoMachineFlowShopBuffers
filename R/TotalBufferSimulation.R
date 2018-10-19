library(stackoverflow)
library(R6)

Comment('
  Wenn M2 ewig wartet, dann auch M1.
  Wenn M2 aber kontinuierlich arbeitet, wird früher
  oder später jeder Job in den Buffer reinkönnen.
        ')

#' @export
simulateFlowShopTotalBuffer <- function(jobData, permutationM1, permutationM2, maxBufferSize){
  m1 <- Machine$new()
  m2 <- Machine$new()
  m1$name <- "m1"
  m2$name <- "m2"
  simData <- SimData$new()
  nextJobM1 <- 1
  nextJobM2 <- 1
  m1Event <- data.frame("job" = "", "state" = "")
  m2Event <- data.frame("job" = "", "state" = "")
  time <- 0
  finished <- FALSE
  m2AllowedToWork <- FALSE

  m1Wait <- 0
  m2Wait <- 0

  maxTime <- sum(jobData$m1Time) + sum(jobData$m2Time)

  jobsJustFinished <- data.frame("job" = c(), "m1Time" = c(), "m2Time" = c(), "bufferUsage" = c())
  buffer <- data.frame("job" = c(), "m1Time" = c(), "m2Time" = c(), "bufferUsage" = c())
  while(!finished){

    if (!m1$turnedOff){
      if (!m1$busy){
        # Buffer prüfen
        nextJob <- jobData[jobData$job == permutationM1[nextJobM1],]

        if (sum(buffer$bufferUsage) + nextJob$bufferUsage <= maxBufferSize){
          m1Event <- m1$give(nextJob$job, nextJob$m1Time)
          simData$writeSimData("M1", m1Event$job, "start", time)
          simData$writeSimData("buffer", m1Event$job, "start", time)
          buffer <- rbind(buffer, nextJob)
          m1Event <- m1$work()
        } else {
          m1Wait <- m1Wait + 1
          m1Event$state <- "waiting"
        }
      } else {
        m1Event <- m1$work()
      }

    }

    if (!m2$turnedOff){
      if (!m2$busy){
        nextJob <- jobData[jobData$job == permutationM2[nextJobM2],]

        if (nextJob$job %in% jobsJustFinished$job){
          m2Event <- m2$give(nextJob$job, nextJob$m2Time)
          simData$writeSimData("M2", m2Event$job, "start", time)
          m2Event <- m2$work()

          jobNumber <- which(jobsJustFinished$job == nextJob$job)

          if (length(jobNumber) > 1){
            stop(paste("Job ", nextJob$job, " wurde auf M1 zwei Mal abgeschlossen!?", sep=""))
          }
          jobsJustFinished <- jobsJustFinished[-c(jobNumber),]
        } else {
          m2Wait <- m2Wait + 1
          m2Event$state <- "waiting"
        }
      } else {
        m2Event <- m2$work()
      }
    }

    # M1 fertig, ggf. blockieren
    if (m1Event$state == "end"){
      simData$writeSimData("M1", m1Event$job, "end", time)
      nextJobM1 <- nextJobM1 + 1
      m1$busy <- FALSE
      if (nextJobM1 > length(permutationM1)){
        m1$turnedOff <- TRUE
        m1Event$state <- "turnedOff"
      }
      jobsJustFinished <- rbind(jobsJustFinished, jobData[jobData$job == m1Event$job,])
    }

    if (m2Event$state == "end"){
      simData$writeSimData("M2", m2Event$job, "end", time)
      simData$writeSimData("buffer", m2Event$job, "end", time)
      jobToRemove <- which(buffer$job == m2Event$job)
      buffer <- buffer[-c(jobToRemove),]

      m2$busy <- FALSE
      m2AllowedToWork <- FALSE
      nextJobM2 <- nextJobM2 + 1
      m2Wait <- 0

      if (nextJobM2 > length(permutationM2)){
        finished <- TRUE
        m2$turnedOff <- TRUE
        m2Event$state <- "turnedOff"
      }
    }

    if (m1Wait > maxTime){
      print(time)
      print(jobData[jobData$job == permutationM1[nextJobM1],])
      stop("M1 wartet (anscheinend ewig) auf einen Job?")

    }
    if (m2Wait > maxTime){
      print(time)
      print(jobData[jobData$job == permutationM2[nextJobM2],])
      stop("M2 wartet (anscheinend ewig) auf einen Job?")
    }

    time <- time + 1
  }
  simData$sortSimData()
  return(simData$simData)
}




