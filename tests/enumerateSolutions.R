# Functions to solve Two Machine Flow Shops by complete enumeration
# (only for small instances)

# optimal permutation schedule
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



# optimal general schedule (where permutations can differ)
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


