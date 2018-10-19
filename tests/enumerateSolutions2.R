# enumerateSolutions2



enumerateAllSolutions2 <- function(jobData, bufType, bufSize){

  possibilities <- gtools::permutations(nrow(jobData),nrow(jobData),v=jobData$job)
  bestPerms <- matrix("",ncol=2*nrow(jobData), nrow=0)
  bestPerm <- c()
  bestValue <- Inf
  for (j in 1:nrow(possibilities)){
    for (i in 1:nrow(possibilities)){
      perm <- possibilities[i,]#paste("j",possibilities[i,],sep="")
      perm2 <- possibilities[j,]
      if (bufType == "totalBuffer"){
        currentValue <- simulateFlowShopTotalBufferC(jobData,perm,perm2,bufSize,FALSE)
      } else {
        currentValue <- simulateFlowShopC(jobData,perm,perm2,bufSize,FALSE)
      }
      if (currentValue == bestValue){
        bestPerms <- rbind(bestPerms,c(perm,perm2))
      }
      if (currentValue < bestValue & currentValue > 0){
        bestValue <- currentValue
        bestPerms <- matrix("",ncol=2*nrow(jobData), nrow=0)
        bestPerms <- rbind(bestPerms,c(perm,perm2))
      }

    }
  }
  return(
    list("bestValue" = bestValue,
         "bestPerms" = bestPerms,
         "nBestPerms" = nrow(bestPerms)
    )
  )
}


enumerateAllSolutions <- function(jobData, bufType, bufSize){

  possibilities <- gtools::permutations(nrow(jobData),nrow(jobData),v=jobData$job)
  bestPerms <- matrix("",ncol=nrow(jobData), nrow=0)
  bestPerm <- c()
  bestValue <- Inf
  for (i in 1:nrow(possibilities)){
    perm <- possibilities[i,]#paste("j",possibilities[i,],sep="")
    if (bufType == "totalBuffer"){
      currentValue <- simulateFlowShopTotalBufferC(jobData,perm,perm,bufSize,TRUE)
    } else {
      currentValue <- simulateFlowShopC(jobData,perm,perm,bufSize,TRUE)
    }
    if (currentValue == bestValue){
      bestPerms <- rbind(bestPerms,perm)
    }
    if (currentValue < bestValue){
      bestValue <- currentValue
      bestPerms <- matrix("",ncol=nrow(jobData), nrow=0)
      bestPerms <- rbind(bestPerms,perm)
    }
  }
  return(
    list("bestValue" = bestValue,
         "bestPerms" = bestPerms,
         "nBestPerms" = nrow(bestPerms)
    )
  )
}

permToM1Time <- function(perm, jobData){
  if (all(grepl("j", perm))){
    perm <- as.integer(substr(perm, start = 2, nchar(perm)))
  }
  return(jobData$m1Time[perm])
}

m1LookupInM1Table <- function(perm, jobData, enumRes){
  if (all(grepl("j", perm))){
    perm <- as.integer(substr(perm, start = 2, nchar(perm)))
    perm <- permToM1Time(perm, jobData)
  }

  m1Table <- apply(enumRes$bestPerms,2,function(x){permToM1Time(x,jobData)})
  if (enumRes$nBestPerms == 1){
    res <- all(m1Table==perm)
  } else {
    res <- apply(m1Table, 1, function(x){all(x==perm)})
  }
  return(any(res))
}

N = 5
foundSomething = F
# while (!foundSomething){
#
#   jobData <- generateProblemInstance(N,c(1,10),c(1,10),1)
#   bufferSize <- 2#max(jobData$m1Time) + ceiling(quantile(jobData$m1Time)["25%"])
#   enumRes <- enumerateAllSolutions2(jobData, "totalBuffer", sample(1:3,1))
#
#   checkResult <- apply(enumRes$bestPerms, 1, function(x){identical(x[1:N], x[(N+1):(2*N)])})
#   if (any(checkResult)){
#
#   } else { foundSomething = T }
#   cat(".")
# }



randomStuff <- enumRes$bestPerms[sample(1:enumRes$nBestPerms,1),]
perm1 <- randomStuff[1:N]
perm2 <- randomStuff[(N+1):(2*N)]

simRes <- simulateFlowShopTotalBuffer(jobData, perm1, perm2, bufferSize)
plotSchedule(simRes,jobData,bufferSize,1)



#Beispielinstanz bufCount, interBuf, wo Unterschied zwischen Perm-Schedules ist
jobData <- generateProblemInstance(6,c(1,10),c(1,10),1)
jobData$m1Time <- c(1,2,8,3,3,3)
jobData$m2Time <- c(7,6,4,1,1,1)
bufferSize <- 1
enumRes <- enumerateAllSolutions2(jobData, "intermediateBuffer", bufferSize)

# gleiche Instanz mit totalBuf (bufferSize=3) ergibt auch dafür eine Instanz
enumRes <- enumerateAllSolutions2(jobData, "totalBuffer", 3)
checkResult <- apply(enumRes$bestPerms, 1, function(x){identical(x[1:N], x[(N+1):(2*N)])})
print(all(!checkResult))
