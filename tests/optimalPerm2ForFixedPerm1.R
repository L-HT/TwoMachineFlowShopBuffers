#zu einer gegebenen Permutation das optimale pi_2 bestimmen

set.seed(20180502)
N <- 4
jobData <- generateProblemInstance(N/2,c(1,3),50,-1)
jobData <- rbind(jobData,generateProblemInstance(N/2,c(5,7),50,-1))
jobData$job <- paste("j",c(1:(N/2),(101):(100+N/2)), sep="")

jobData$m2Time <- 6#ceiling(quantile(jobData$m1Time)["25%"])
bufferSize <- max(jobData$m1Time) + ceiling(quantile(jobData$m1Time)["25%"])
perm1 <- paste("j",sample(c(2,101,1,102)),sep="")
print(perm1)
enumRes <- enumerateAllSolutionsFixedPerm(jobData, "totalBuffer",bufferSize, perm1)
print(enumRes)


randomStuff <- enumRes$bestPerms[sample(1:enumRes$nBestPerms,1),] #sample(jobData$job,size = N)
perm <- randomStuff#paste("j",c(randomStuff),sep="")
simRes <- simulateFlowShopTotalBuffer(jobData, perm1, perm, bufferSize)
plotSchedule(simRes,jobData,bufferSize,1)



enumerateAllSolutionsFixedPerm <- function(jobData, bufType, bufSize, perm1){

  possibilities <- gtools::permutations(nrow(jobData),nrow(jobData),v=jobData$job)
  bestPerms <- matrix("",ncol=nrow(jobData), nrow=0)
  bestPerm <- c()
  bestValue <- Inf
  for (i in 1:nrow(possibilities)){
    perm <- possibilities[i,]#paste("j",possibilities[i,],sep="")
    if (bufType == "totalBuffer"){
      currentValue <- simulateFlowShopTotalBufferC(jobData,perm1,perm,bufSize,FALSE)
    } else {
      currentValue <- simulateFlowShopC(jobData,perm1,perm,bufSize,FALSE)
    }
    if (currentValue == bestValue){
      bestPerms <- rbind(bestPerms,perm)
    }
    if (currentValue < bestValue & currentValue > 0){
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


N = 5

#jobData <- generateProblemInstance(N,c(10,100),c(10,100),-1)
jobData <- generateProblemInstance(N,c(1,10),c(1,10),-1)
bufferSize <- max(jobData$m1Time) + ceiling(quantile(jobData$m1Time)["25%"])
perm1 <- paste("j",sample(1:N),sep="")
# print(perm1)
enumRes <- enumerateAllSolutionsFixedPerm(jobData, "totalBuffer",bufferSize, perm1)
enumRes2 <- enumerateAllSolutions2(jobData, "totalBuffer", bufferSize)
# print(enumRes)
#jobData$job <- paste("j",c(1:(N/2),(101):(100+N/2)), sep="")

for (i in 1:enumRes$nBestPerms){
  if (identical(perm1, enumRes$bestPerms[i,])){
    print("drin")
  }
}

randomStuff <- enumRes$bestPerms[sample(1:enumRes$nBestPerms,1),]
perm <- randomStuff#paste("j",c(randomStuff),sep="")

# To-Do: Suche eine Instanz, wo enumRes nicht perm1 beinhaltet


simRes <- simulateFlowShop(jobData, perm1, perm, bufferSize)
plotSchedule(simRes,jobData,bufferSize,1)

