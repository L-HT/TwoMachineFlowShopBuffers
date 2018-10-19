# Beispiel für das Spiegeln eines Produktionsplanes

set.seed(30092018)
jobData <- generateProblemInstance(5, c(1,5), 3, -1)

bla <- jobData$m1Time
jobData$m1Time <- jobData$m2Time
jobData$m2Time <- bla

maxBufferSize <- 8
perm1 <- paste("j",c(1,2,3,4,5),sep="")
perm2 <- paste("j",c(2,1,4,3,5),sep="")

simRes <- simulateFlowShopTotalBuffer(jobData, perm1, perm2, maxBufferSize)
simRes[c(6,7,8,9,10),] <- simRes[c(10,9,8,7,6),]
plotSchedule(simRes,jobData, maxBufferSize, 1)

set.seed(30092018)
jobData <- generateProblemInstance(5, c(1,5), 3, -1)

maxBufferSize <- 8
perm1 <- paste("j",c(5,3,4,1,2),sep="")
perm2 <- paste("j",c(5,4,3,2,1),sep="")


simRes <- simulateFlowShopTotalBuffer(jobData, perm1, perm2, maxBufferSize)
simRes[c(6,7,8,9,10),] <- simRes[c(10,9,8,7,6),]
simRes[c(4,5), c(3,4)] <- simRes[c(4,5), c(3,4)] + 1
simRes[c(6,7), c(3)] <- simRes[c(6,7), c(3)] + 1
plotSchedule(simRes,jobData, maxBufferSize, 1)

