set.seed(20171019)
library(Masterarbeit1)
maxBufferSize <- 2
N <- 5
jobData <- generateProblemInstance(N, c(3,5), c(5,10), c(1,1))

permutationM1 <- paste("j",sample(1:N,N), sep="")
permutationM2 <- permutationM1  #paste("j",1:10, sep="")
simulationResult <- simulateFlowShop(jobData, permutationM1, permutationM2, maxBufferSize)
simulationResult2 <- simulateFlowShopTotalBuffer(jobData, permutationM1, permutationM2, maxBufferSize)



plotSchedule(simulationResult2, jobData, maxBufferSize, maxBufferSize)
getMakespanOfSimulation(jobData, permutationM1, permutationM2, maxBufferSize)


