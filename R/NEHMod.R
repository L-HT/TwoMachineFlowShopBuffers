
#' @export
getNEHSolutionMod <- function(jobData, bufferType, maxBufferSize, subType,
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

  groupSize <- trunc(sqrt(nrow(jobData)))
  listSize <- 2*groupSize

  for (group in 1:groupSize){
    if (group != groupSize){
      jobGroup <- sortedJobs[ ((group-1) * groupSize + 1) : (group*groupSize) ]
    } else {
      jobGroup <- sortedJobs[ ((group-1) * groupSize + 1) : length(sortedJobs) ]
    }

    bestPosDf <- data.frame("index"=rep(1, 0), value=rep(9999999, 0), stringsAsFactors = F)

    # für die erste Gruppe: rechne wie NEH
    if (group == 1){
      for (job in jobGroup[-c(1,2)]){
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

        # Ende probieren
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
        }
        bestMakespan <- Inf
      }
    }

    if (group == 2 || group == groupSize){ # zweite Gruppe und Gruppenende
      for (job in jobGroup){
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

        # Ende probieren
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

        }
        bestMakespan <- Inf
      }

    }
    if (group > 2 && group != groupSize) { # für alle darauffolgenden Gruppen: Rechne nach neuer Variante

      for (j in 1:length(jobGroup)){
        job <- jobGroup[j]
        bestMakespan <- Inf

        if (j == 1){
          for (k in 1:length(result)){
            tempSequence <- c(result[0:(k-1)], job, result[(k):length(result)])
            currentMakespan <- getMakespanOfSimulation(jobData, tempSequence, tempSequence, maxBufferSize, bufferType,
                                                       targetCriterion)
            counter <- counter + 1

            bestPosDf <- insertAndReplace(bestPosDf, k, currentMakespan, listSize)

            if (currentMakespan < bestMakespan){
              bestMakespan <- currentMakespan
              bestIndex <- k
            }
          }

          # Endposition probieren
          tempSequence <- c(result, job)
          currentMakespan <- getMakespanOfSimulation(jobData, tempSequence, tempSequence, maxBufferSize, bufferType,
                                                     targetCriterion)
          bestPosDf <- insertAndReplace(bestPosDf, length(result) + 1, currentMakespan, listSize)

          counter <- counter + 1
          if (currentMakespan < bestMakespan){
            bestMakespan <- currentMakespan
            bestIndex <- length(result) + 1
          }

        } else {
          # darauffolgende Jobs in der Gruppe

          # erst die bestehende Liste durchgehen
          for (pos in bestPosDf$index) {
            if (pos != length(result) + 1){
              tempSequence <- c(result[0:(pos-1)], job, result[(pos):length(result)])
            } else {
              tempSequence <- c(result, job)
            }

            currentMakespan <- getMakespanOfSimulation(jobData, tempSequence, tempSequence, maxBufferSize, bufferType,
                                                       targetCriterion)
            bestPosDf[bestPosDf$index == pos, 2] <- currentMakespan
            counter <- counter + 1
            if (currentMakespan < bestMakespan){
              bestMakespan <- currentMakespan
              bestIndex <- pos
            }

          }

          # dann die Nachbarpositionen des vorher eingesetzten Jobs vergleichen
          # und ggf. Liste aktualisieren
          # (leftNeighborPos müsste eig. immer noch drin sein)
          for (pos in c(rightNeighborPos)){
            if (pos != length(result) + 1){
              tempSequence <- c(result[0:(pos-1)], job, result[(pos):length(result)])
            } else {
              tempSequence <- c(result, job)
            }

            currentMakespan <- getMakespanOfSimulation(jobData, tempSequence, tempSequence, maxBufferSize, bufferType,
                                                       targetCriterion)
            counter <- counter + 1
            bestPosDf <- insertAndReplace(bestPosDf, pos, currentMakespan, listSize)
            if (currentMakespan < bestMakespan){
              bestMakespan <- currentMakespan
              bestIndex <- pos
            }
          }
        }

        # Einfügung in die beste Position
        if (bestIndex == length(result) + 1){
          result <- c(result, job)

        } else {
          result <- c(result[0:(bestIndex-1)], job, result[(bestIndex):length(result)])

          # dieses Update wird laufzeittechnisch dominiert
          bestPosDf[bestPosDf$index > bestIndex, 1] <- bestPosDf[bestPosDf$index > bestIndex, 1] + 1

          # die alte Position fliegt raus in der nächsten Runde (sozusagen ist leftNeighborPos automatisch drin)
          bestPosDf[bestPosDf$index == bestIndex, 2] <- Inf
          #leftNeighborPos <- bestIndex#max(0, bestIndex-1)
          #rightNeighborPos <- bestIndex + 1#min(length(result) + 1, bestIndex + 1)
        }

        # zwei Nachbarn festlegen (noch mal überprüfen)
        leftNeighborPos <- bestIndex
        rightNeighborPos <- bestIndex + 1

      }
    }
  }

  print("Evaluationen fuer NEH-modGroup (with checks):")
  print(counter)
  result <- as.integer(substring(result, 2))
  result <- c(result, counter)
  return(result)
}






