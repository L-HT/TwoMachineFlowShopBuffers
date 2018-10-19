#' @export
rCholeskySolve <- function(Phi,y){
  blatrix <<- Phi
  blector <<- y
  #print(head(Phi))
  numberOfFeatures <- ncol(Phi)

  badColumns <- apply(Phi[,-numberOfFeatures], 2, function(x){length(unique(x)) == 1})# | all(x!=0)})
  if (any(badColumns)){
    print("badColumns gefunden: ")

    badIndices <- which(badColumns)
    print(badIndices)
    Phi <- Phi[,-badIndices]
  }

  # print(head(blatrix))
  A <- t(Phi) %*% Phi
  b <- t(Phi) %*% y
  U <- chol(A)
  result <- backsolve(U, backsolve(U, b, transpose = TRUE))

  if (any(badColumns)){
    #künstliches Auffüllen des Vektors
    newResult <- rep(0, numberOfFeatures)
    tempCounter <- 1
    for (i in 1:numberOfFeatures){
      if (i %in% badIndices){
        newResult[i] <- 0
      } else {
        newResult[i] <- result[tempCounter]
        tempCounter <- tempCounter + 1
      }
    }
    return(newResult)
  }
  # attempt <- function(){
  #   U <- chol(A)
  #   return(backsolve(U, backsolve(U, b, transpose = TRUE)))
  # }
  # errorOccured <- FALSE
  # result <- tryCatch(attempt(), error = function(e){errorOccured <<- TRUE})
  #
  # if (!errorOccured){
  #   if (any(badColumns)){
  #     #künstliches Auffüllen des Vektors
  #     newResult <- rep(0, numberOfFeatures)
  #     tempCounter <- 1
  #     for (i in 1:numberOfFeatures){
  #       if (i %in% badIndices){
  #         newResult[i] <- 0
  #       } else {
  #         newResult[i] <- result[tempCounter]
  #         tempCounter <- tempCounter + 1
  #       }
  #     }
  #     return(newResult)
  #   }
  # } else {
  #   print("Matrix zu klein")
  #   result <- c(rep(0.5, numberOfFeatures - 1), 0.1)
  # }
  return(result)
}

#' @export
rQRSolve <- function(Phi,y){
  blatrix <<- Phi
  blector <<- y
  A <- t(Phi) %*% Phi
  b <- t(Phi) %*% y

  if (qr(A)$rank != ncol(A)){
    numberOfFeatures <- ncol(A)
    colNumbers <- 1:ncol(A)
    rankCheckValue <- qr(A)$rank
    importantCols <- sapply(1:ncol(A), function (x) qr(A[,-x])$rank)
    badRank <- max(importantCols)
    zeroPositions <- c()

    while(badRank == rankCheckValue){
      columnToRemove <- sample(which(importantCols == badRank),1)
      zeroPositions <- c(zeroPositions,colNumbers[columnToRemove])
      colNumbers <- colNumbers[-columnToRemove]

      b <- b - A[,columnToRemove]
      A <- A[,-columnToRemove]

      if (!is.null(ncol(A))){
        importantCols <- sapply(1:ncol(A), function (x) qr(A[,-x])$rank)
        badRank <- max(importantCols)
      } else {
        badRank <- -1
      }
    }


    #print(paste(length(zeroPositions), " Spalten entfernt", sep=""))
    x <- tryCatch(qr.solve(A,b), error = function(e) {print("solve problemed"); rep(1,numberOfFeatures)})
    result <- rep(0,numberOfFeatures)

    tempCounter <- 1
    for (i in 1:numberOfFeatures){
      if (i %in% zeroPositions){
        result[i] <- 1
      } else {
        result[i] <- x[tempCounter]
        tempCounter <- tempCounter + 1
      }
    }

  } else {
    result <- qr.solve(A,b)
  }
  return(result)
}

#' @export
parseFlowShopName <- function(fileName){
  parts <- strsplit(fileName, "-")[[1]]
  names(parts) <- c("bufferType", "numberOfJobs", "m2Type", "bufType", "constrainedness", "maxBufferSize", "instanceNumber")
  return(parts)
}


