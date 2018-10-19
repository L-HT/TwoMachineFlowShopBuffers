# Eigenschaften von NEH austesten

set.seed(20180816)
jobData <- generateProblemInstance(9, c(1,10), 5, -1)
jobData$m1Time <- c(5,5,4,4,3,3,2,2,3)
jobData$m2Time <- 3
jobData$bufferUsage <- jobData$m1Time
maxBufferSize <- 9#max(jobData$m1Time)+15
bufferType <- "totalBuffer"



# 3 2 5 4 1
perm <- paste("j",c(1,2,3),sep="")

simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, maxBufferSize = maxBufferSize)
plotSchedule(simRes, jobData, maxBufferSize, 1)

getNEHSolutionMod2Group(jobData, bufferType, maxBufferSize, 0, targetCriterion = "makespan")
getNEHSolution(jobData, bufferType, maxBufferSize, 0, "makespan")




"
Abwandlung von Mod2Group:

Behalte Liste von sqrt(n), aber ohne check, sondern
einfach nur als äquidistante Positionen auf der aktuellen Teilpermutation.
"

getNEHSolutionModSampling <- function(jobData, bufferType, maxBufferSize, subType,
                                    targetCriterion = "makespan"){

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
  listSize <- groupSize
  jobGroup <- sortedJobs[1:groupSize]

  ####### erste Gruppe
  ####### rechne wie NEH

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
    bestMakespan <- Inf
  }


  ##### zweite Gruppe
  ##### mit Bestenliste


  jobGroup <- sortedJobs[ (groupSize + 1) : length(sortedJobs)]

  for (j in 1:length(jobGroup)){
    job <- jobGroup[j]
    bestMakespan <- Inf
    positions <- trunc(seq.int(1, length(result) + 1, length.out = groupSize))

    # erst die bestehende Liste durchgehen
    for (pos in positions) {
      if (pos != length(result) + 1){
        tempSequence <- c(result[0:(pos-1)], job, result[(pos):length(result)])
      } else {
        tempSequence <- c(result, job)
      }

      currentMakespan <- getMakespanOfSimulation(jobData, tempSequence, tempSequence, maxBufferSize, bufferType,
                                                 targetCriterion)

      counter <- counter + 1
      if (currentMakespan < bestMakespan){
        bestMakespan <- currentMakespan
        bestIndex <- pos
      }

    }

    # Einfügung in die beste Position
    if (bestIndex == length(result) + 1){
      result <- c(result, job)
    } else {
      result <- c(result[0:(bestIndex-1)], job, result[(bestIndex):length(result)])
    }
  }

  print("Evaluationen fuer NEH-mod (equidistant):")
  print(counter)
  result <- as.integer(substring(result, 2))
  return(result)
}


"
Für sqrt(n) Jobs macht man normales NEH (damit die Jobs mit dem größten Einfluss
ordentlich eingeplant werden) und der Rest wird kompakter eingeplant (weil da
die Laufzeit am meisten ansteigt):
Ist gut, wenn wenige lange Jobs und viele kurze Jobs sind, man hat dann
viele ähnliche Jobs in der zweiten Gruppe.

Danach läuft man komplett durch und legt eine sqrt(n)-Liste an sowie die Einplanung von
j1.
Der nächste Kandidat j2 läuft diese Liste durch mit den Nachbarn von j1 und wird
eingeplant. Die Nachbarn von j2 kommen dann in diese sqrt(n)-Liste rein, nachdem
sie gecheckt wurden. Das wird bis zum Ende wiederholt.

"

getNEHSolutionMod2Group <- function(jobData, bufferType, maxBufferSize, subType,
                              targetCriterion = "makespan"){

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
  listSize <- groupSize
  jobGroup <- sortedJobs[1:groupSize]

  ####### erste Gruppe
  ####### rechne wie NEH

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
    bestMakespan <- Inf
  }


  ##### zweite Gruppe
  ##### mit Bestenliste


  jobGroup <- sortedJobs[ (groupSize + 1) : length(sortedJobs)]
  bestPosDf <- data.frame("index"=rep(1, 0), value=rep(9999999, 0), stringsAsFactors = F)


  leftNeighborPos <- 0
  rightNeighborPos <- 0

  for (j in 1:length(jobGroup)){
    job <- jobGroup[j]
    bestMakespan <- Inf

    if (j == 1){
      # erster Job in der Gruppe: Baut sich Liste der sqrt(n)
      # besten Einfügepositionen auf
      # zu diesem Zeitpunkt sind sqrt(n) Jobs eingeplant, deswegen
      # ist bis length(result) noch ok

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

    # print(bestIndex)
    # print(result)
    # print(bestPosDf)
    # Einfügung in die beste Position
    if (bestIndex == length(result) + 1){
      result <- c(result, job)

    } else {
      result <- c(result[0:(bestIndex-1)], job, result[(bestIndex):length(result)])
      # danach die Indizes verschieben
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

  print("Evaluationen fuer NEH-mod:")
  print(counter)
  result <- as.integer(substring(result, 2))
  return(result)
}


"
modifiziertes NEH:
Sei s = sqrt(n) (eine Abkürzung).
-man teilt Jobs in s Gruppen ein. Jede Gruppe sollte in etwa s
Elemente beinhalten.

Es wird für jede Gruppe eine Liste mit s in Frage kommenden Positionen gehalten.
Der erste Job in der Gruppe prüft alle möglichen Positionen und legt diese Liste an.
Der zweite Job j' prüft dann die Liste und wird eingefügt an einer Stelle.
Diese Position geht aus der Liste raus, aber die Nachbarpositionen von j'
gehen wieder rein, also wächst die Liste insgesamt um 1 (oder manchmal 0, falls
sich diese Positionen überdecken)
Diese überprüft man und aktualisiert gegebenenfalls die Bestenliste.

Für die erste Gruppe: Wende NEH wie üblich an, weil die möglichen Positionen under
sqrt(n) liegen, also kommen hier immer alle Positionen in Frage.

Zweite Gruppe: Da kann die Liste verwendet werden...?

Anscheinend sollte die Liste länger sein, damit der letzte Job nicht nur
eine mögliche Einfügeposition hat?

Idee:
-Die Liste der besten Kandidaten kann man in sqrt(n)-Zeit aktualisieren, indem
man durch die Liste läuft und nur den schlechtesten Kandidaten aktualisiert

-also:
erste Gruppe: Führe NEH wie normal aus.
Einfügungen erfolgen immer nach rechts verschiebend (wie bei insert)

Zweite Gruppe: Nimm die ersten sqrt(n)-Stellen und lege damit Bestenliste an.
Job j1 wird eingefügt, aus der Bestenliste gestrichen, aber die Nachbarn von
j1 kommen in die Liste rein.

J2 testet nun die Liste der Länge sqrt(n)+1. Und merkt sich dazu die sqrt(n)
besten Positionen. Also hat man die Liste wieder gestutzt. So wiederholt sich alles.

"
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
  return(result)
}

insertAndReplace <- function(myDf, index, value, listLength){
  worstIndex <- 0
  worstValue <- 0

  # die ersten Elemente einfach aufsammeln
  if (nrow(myDf) < listLength){
    #myDf <- rbind(myDf, list(index, value))
    myDf[nrow(myDf)+1, ] <- list(index, value)
  } else {
    # nach den ersten Elementen auswählen
    for (i in 1:nrow(myDf)){
      if (worstValue < myDf[i,2]){
        worstIndex <- i
        worstValue <- myDf[i,2]
      }
    }
    # Ersetze schlechtesten Wert
    if (value < worstValue){
      myDf[worstIndex, 1] <- index
      myDf[worstIndex, 2] <- value
    }
  }
  #colnames(myDf) <- c("index", "value")
  return(myDf)
}




