# Examples on how to use this package

# load package
library(TwoMachineFlowShopBuffers)

# set option (or else later functions will not work)
options(stringsAsFactors = FALSE)

# generate instance with 10 jobs, aj and bj between 1 and 10 and sj=aj
jobData <- generateProblemInstance(10, c(1,10), c(1,10), -1)

# set buffer size
maxBufferSize <- 11

# set permutations by hand
perm1 <- c("j1","j2","j3","j4","j5","j6","j7","j8","j9","j10")
perm2 <- c("j2","j1","j3","j4","j5","j6","j8","j7","j9","j10")

# simulate flow shop (here with spanning buffer (=total buffer))
simulationResult <- simulateFlowShopTotalBuffer(jobData, perm1, perm2, maxBufferSize)

# plot result (setting ticks every 1 step on time axis)
plotSchedule(simulationResult, jobData, maxBufferSize, 1)

###########################################################

# call heuristic to solve this problem

startILS(jobData = jobData,
         logFileName = "testRun1",
         maxBufferSize = maxBufferSize,
         targetCriterion = "TFT",
         bufferType = "intermediateBuffer",
         runNumber = 1,
         fileSuffix = "",
         p=0.01,
         operationSequence = "23")

startILS(jobData, "logFileName", maxBufferSize)

# the resulting log file can then be found in "./output"
# the best solution calculated by this algorithm can be found in "./solutions"
