# F2-Buf-Generator

# Generiert Probleminstanzen für des 2-Maschinen-Flow-Shop-Problem mit Zwischenspeicher
#

# Instanz hat Menge von Jobs, jeder Job hat
#     -Laufzeit auf M1
#     -Laufzeit auf M2
#     -Bufferbelegung
# als 4-Spalten-DF

# alles als Integer-Wert möglich

# Bufferkapazität vorgegeben

# Check, ob ein Plan zulässig ist

###

# Bufferarten:
# spielt bei der Generierung keine Rolle?
#


#' Generiere Instanz von 2-Maschinen-Flow-Shop.
#'
#' Generiert eine Instanz des 2-Maschinen-Flow-Shops
#' mit Zwischenspeicher.
#' @param numberOfJobs Anzahl der Jobs.
#' @param aLimit Zweielementiger Vektor.
#' @param bLimit Zweielementiger Vektor (die Grenzen). Wenn nur eine Zahl, dann konstant diese.
#' @param bufferLimit Zweielementiger Vektor (Grenzen). Wenn
#' 1 (Skalar), dann konstant 1. Wenn negativer Skalar, dann wie bei M1.
#'
#' @export
generateProblemInstance <- function(numberOfJobs, aLimit, bLimit, bufferLimit){
  jobNames <- 1:numberOfJobs
  jobNames <- paste("j", jobNames, sep="")

  aTimes <- sample(aLimit[1]:aLimit[2] , size = numberOfJobs, replace=T)

  # jedes Mal vorgegeben oder konstant
  if (length(bLimit) == 2){
    bTimes <- sample(bLimit[1]:bLimit[2] , size = numberOfJobs, replace=T)
  } else {
    bTimes <- rep(bLimit, numberOfJobs)
  }

  # individuell, gleich Laufzeit auf M1 oder immer 1 (zählender Buffer)
  if (length(bufferLimit) == 2){
    bufferTimes <- sample(bufferLimit[1]:bufferLimit[2] , size = numberOfJobs, replace=T)
  } else {
    if (bufferLimit == 1){
      bufferTimes <- rep(1, numberOfJobs)
    }
    if (bufferLimit < 0){
      bufferTimes <- aTimes
    }
  }
  return(data.frame("job" = as.character(jobNames), "m1Time" = aTimes, "m2Time" = bTimes, "bufferUsage" = bufferTimes, stringsAsFactors = F))
}

#' Generiere Deadline-Instanz von 2-Maschinen-Flow-Shop.
#'
#' @export
appendDeadlinesToJobData <- function(jobData, bufferType, maxBufferSize){

  perm <- getNEHSolution(jobData, bufferType, maxBufferSize, 0, "makespan")
  perm <- paste("j", perm, sep="")
  if (bufferType == "totalBuffer"){
    makespan <- simulateFlowShopTotalBufferC(jobData, perm, perm, maxBufferSize)
  } else {
    makespan <- simulateFlowShopC(jobData, perm, perm, maxBufferSize)
  }
  dueTime <- sample(1:makespan, size=nrow(jobData), replace = T)
  return(cbind(jobData, dueTime))
}

#' Generiere normalverteilte Instanz von 2-Maschinen-Flow-Shop.
#'
#'
#' @param numberOfJobs Anzahl der Jobs.
#' @param aLimit Zweielementiger Vektor.
#' @param bLimit Zweielementiger Vektor (die Grenzen). Wenn nur eine Zahl, dann konstant diese.
#' @param bufferLimit Zweielementiger Vektor (Grenzen). Wenn
#' 1 (Skalar), dann konstant 1. Wenn negativer Skalar, dann wie bei M1.
#'
#' @export
generateNormalProblemInstance <- function(numberOfJobs, m, s){
  jobNames <- 1:numberOfJobs
  jobNames <- paste("j", jobNames, sep="")

  aTimes <- ceiling(rnorm(n= numberOfJobs, mean=m, sd=s ))
  aTimes[aTimes <= 0] = 1
  #aTimes <- sample(aLimit[1]:aLimit[2] , size = numberOfJobs, replace=T)


  bTimes <- ceiling(rnorm(n= numberOfJobs, mean=m, sd=s ))
  bTimes[bTimes <= 0] = 1


  # individuell, gleich Laufzeit auf M1 oder immer 1 (zählender Buffer)

  bufferTimes <- aTimes

  return(data.frame("job" = as.character(jobNames), "m1Time" = aTimes, "m2Time" = bTimes, "bufferUsage" = bufferTimes, stringsAsFactors = F))
}




