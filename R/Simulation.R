
#         Ich brauche ein Log:
#         -wann fängt welcher Job auf welcher Maschine an?
#         -Wann hört jeder Job auf?
#         -Wann besitzt welcher Job den Buffer?
#
#         Event-Meldung:
#         -Jobnummer
#         -Ereignis (start, end, block-start, block-end, buffer-in, buffer-out)
#
#         Zeitplan:
#         -Typ: Maschine 1, Maschine 1 belegt, Maschine 2, Bufferbelegung
#         -Jobnummer
#         -Start
#         -End
#
# Fallunterscheidung mit Finished-Jobs
# M1 ist fertig, M2 blockiert:
#   -geht in den Buffer, wenn da voll, dann M1 blockiert
#
# M1 ist fertig, M2 ist frei:
#   -wenn M2 den fertigen Job nimmt, dann zu M2
#   -wenn er einen anderen Job nimmt, dann in Buffer
#     -wenn voll: M1 blockiert


#permutation: hat nur JobNamen

#' @export
simulateFlowShop <- function(jobData, permutationM1, permutationM2, maxBufferSize){
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

  simData$sortSimData()

  return(simData$simData)
}





