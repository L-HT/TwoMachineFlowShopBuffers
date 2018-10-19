library(stackoverflow)
library(R6)


#' @export
simulateFlowShopTFT <- function(jobData, permutationM1, permutationM2, maxBufferSize){
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
  totalBTime <-sum(jobData$m2Time) + sum(jobData$m1Time)

  m2Wait <- 0
  result <- 0

  jobsJustFinished <- data.frame("job" = c(), "m1Time" = c(), "m2Time" = c(), "bufferUsage" = c())
  buffer <- data.frame("job" = c(), "m1Time" = c(), "m2Time" = c(), "bufferUsage" = c())
  while(!finished){

    # nehmen aus Buffer
    if (!m2$busy){
      nextJob <- NULL
      if (permutationM2[nextJobM2] %in% buffer$job){
        lineNumber <- which(buffer$job == permutationM2[nextJobM2])
        nextJob <- buffer[lineNumber,]
        buffer <- buffer[-c(lineNumber),]
        simData$writeSimData("buffer", nextJob$job, "end", time-1)
        m2AllowedToWork <- TRUE
      }
    }

    # frisch aus M1 nehmen oder aus M1 in Buffer schieben
    if (nrow(jobsJustFinished) > 0){
      # Sonderfall: M1 fertig und M2 will genau das haben
      if (permutationM2[nextJobM2] == jobsJustFinished[1, "job"] && !m2$busy){
        m1$busy <- FALSE
        assertthat::assert_that(m2AllowedToWork == FALSE)
        m2AllowedToWork <- TRUE
        if (m1$blocked){
          m1$blocked <- FALSE
          simData$writeSimData("block", jobsJustFinished$job, "end", time-1)
        }
        jobsJustFinished <- jobsJustFinished[-1,]
      } else {
        currentBufferSize <- sum(buffer$bufferUsage)
        if (currentBufferSize + sum(jobsJustFinished$bufferUsage) <= maxBufferSize){
          buffer <- rbind(buffer, jobsJustFinished)
          simData$writeSimData("buffer", jobsJustFinished$job, "start", time)
          m1$busy <- FALSE
          if (m1$blocked){
            m1$blocked <- FALSE
            simData$writeSimData("block", jobsJustFinished$job, "end", time-1)
          }
          jobsJustFinished <- data.frame("job" = c(), "m1Time" = c(), "m2Time" = c(), "bufferUsage" = c())
        } else {
          # M1 muss blockiert werden
          if (!m1$blocked){
            m1$blocked <- TRUE
            m1$busy <- TRUE
            m1Event$state <- "blocked"
            simData$writeSimData("block", jobsJustFinished$job, "start", time)
          }
        }
      }
    }
    if (!m1$turnedOff){
      if (!m1$busy){
        nextJob <- jobData[jobData$job == permutationM1[nextJobM1],]
        m1Event <- m1$give(nextJob$job, nextJob$m1Time)
        simData$writeSimData("M1", m1Event$job, "start", time)
      }
      if (!m1$blocked){
        m1Event <- m1$work()
      }
    }

    if (!m2$turnedOff){
      if (!m2$busy){
        if (m2AllowedToWork){
          nextJob <- jobData[jobData$job == permutationM2[nextJobM2],]
          m2Event <- m2$give(nextJob$job, nextJob$m2Time)
          simData$writeSimData("M2", m2Event$job, "start", time)
          m2Event <- m2$work()
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
      if (nextJobM1 > length(permutationM1)){
        m1$turnedOff <- TRUE
        m1Event$state <- "turnedOff"
      }
      jobsJustFinished <- rbind(jobsJustFinished, jobData[jobData$job == m1Event$job,])
    }

    if (m2Event$state == "end"){
      simData$writeSimData("M2", m2Event$job, "end", time)
      m2$busy <- FALSE
      m2AllowedToWork <- FALSE
      nextJobM2 <- nextJobM2 + 1
      m2Wait <- 0
      # print(time+1)
      result <- result + time + 1

      if (nextJobM2 > length(permutationM2)){
        finished <- TRUE
        m2$turnedOff <- TRUE
        m2Event$state <- "turnedOff"
      }
    }

    if (m2Wait > totalBTime){
      stop("M2 wartet (anscheinend ewig) auf einen Job?")
    }
    time <- time + 1
  }

  return(result)
}



#' @export
simulateFlowShopTotalBufferTFT <- function(jobData, permutationM1, permutationM2, maxBufferSize){
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

  result <- 0

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
      # print(time+1)
      result <- result + time + 1
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

  return(result)
}




