# das modifizierte NEH testen

set.seed(20180816)
jobData <- generateProblemInstance(9, c(1,10), 5, -1)
jobData$m1Time <- c(5,5,4,4,3,3,2,2,3)
jobData$m2Time <- 3
jobData$bufferUsage <- jobData$m1Time
maxBufferSize <- 9#max(jobData$m1Time)+15
bufferType <- "totalBuffer"

jobData <- generateProblemInstance(100, c(1,100), 50, -1)
maxBufferSize <- max(jobData$m1Time)+20
bufferType <- "totalBuffer"

# jd1 <- generateProblemInstance(10, c(80,100), 50, -1)
# jd2 <- generateProblemInstance(90, c(10,20), 50, -1)
# jobData <- rbind(jd1, jd2)
# jobData$job <- paste("j", 1:100, sep="")

startTime <- Sys.time()
perm1 <- getNEHSolutionModSampling(jobData, bufferType, maxBufferSize, 0, targetCriterion = "makespan")
print(Sys.time() - startTime)

startTime <- Sys.time()
perm2 <- getNEHSolution(jobData, bufferType, maxBufferSize, 0, "makespan")
print(Sys.time() - startTime)

startTime <- Sys.time()
perm3 <- getNEHSolutionMod(jobData, bufferType, maxBufferSize, 0, targetCriterion = "makespan")
print(Sys.time() - startTime)

# perm <- paste("j", perm1, sep="")
# simRes <- simulateFlowShopTotalBufferC(jobData, perm, perm, maxBufferSize = maxBufferSize)
# # simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, maxBufferSize = maxBufferSize)
# # plotSchedule(simRes, jobData, maxBufferSize, 10)
# print(paste("sampling: ",simRes,sep=""))

perm <- paste("j", perm2, sep="")
simRes <- simulateFlowShopTotalBufferC(jobData, perm, perm, maxBufferSize = maxBufferSize)
# simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, maxBufferSize = maxBufferSize)
# plotSchedule(simRes, jobData, maxBufferSize, 10)
print(paste("standard: ",simRes,sep=""))

perm <- paste("j", perm3, sep="")
simRes <- simulateFlowShopTotalBufferC(jobData, perm, perm, maxBufferSize = maxBufferSize)
# simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, maxBufferSize = maxBufferSize)
# plotSchedule(simRes, jobData, maxBufferSize, 10)
print(paste("withList: ",simRes,sep=""))

# perm <- paste("j", sample(1:nrow(jobData)), sep="")
# simRes <- simulateFlowShopTotalBufferC(jobData, perm, perm, maxBufferSize = maxBufferSize)
# print(paste("random: ",simRes,sep=""))
