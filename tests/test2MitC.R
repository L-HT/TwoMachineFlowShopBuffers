set.seed(20171019)

maxBufferSize <- 50
N <- 50
jobData <- generateProblemInstance(N, c(10,50), c(10,50), c(1,80))

permutationM1 <- paste("j",1:N, sep="")
permutationM2 <- permutationM1  #paste("j",1:10, sep="")
getMakespanOfSimulation(jobData, permutationM1, permutationM2, maxBufferSize)
simulateFlowShopC(jobData, permutationM1, permutationM2, maxBufferSize)

rbenchmark::benchmark(
  getMakespanOfSimulation(jobData, permutationM1, permutationM2, maxBufferSize, "intermediateBuffer"),
  simulateFlowShopC(jobData, permutationM1, permutationM2, maxBufferSize),
  replications = 20
)
rbenchmark::benchmark(
  getMakespanOfSimulation(jobData, permutationM1, permutationM2, max(jobData$bufferUsage), "totalBuffer"),
  simulateFlowShopTotalBufferC(jobData, permutationM1, permutationM2, max(jobData$bufferUsage)),
  replications = 20
)

# Ergebnis: also 350-fache Beschleunigung oder so?


