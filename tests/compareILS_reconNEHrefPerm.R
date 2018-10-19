# braucht man Referenzpermutation oder reicht Random aus
# braucht man NEH oder reicht eine Zufallslösung aus?
# braucht man Reconstruct überhaupt
# wenn ja, mit Random oder mit Ref-Permutation

library(TwoMachineFlowShopEx)
instance <- "./instances/f2total-100-m2const-bufm1-big-130-2"
instance <- "./oldInstances/instances/f2total-75-m2const-bufm1-mid-139-2"
instance <- "./instances/instances50100/f2inter-50-m2const-bufm1-small-26-1"
#instance <- "./dueTimeInstances/f2total-150-m2const-bufm1-mid-125-1"
# instance <- "./valInstances/f2inter-100-m2const-bufcount-big-1-1"
instance <- "./instances/f2total-10-m2var-bufm1-c-128-1"

instanceData <- parseFlowShopName(instance)
maxBufferSize <- as.integer(instanceData["maxBufferSize"])
jobData <- read.csv(file = instance, stringsAsFactors = F)
#jobData <- appendDeadlinesToJobData(jobData, "totalBuffer", maxBufferSize)

s
startILS(jobData, "ilsOff", maxBufferSize = maxBufferSize,
         "totalBuffer", runNumber = 1,
         lookAround = 0, p = 0.318, operationSequence = "231",
         numberOfAnts = 10, withACO = FALSE, withLS = T,
         withReconstruct = F, targetCriterion = "dueTimes",
         useRandomPermutationForLS = T,
         withNEH = T, withInitialReconstruct = F
)

startILS(jobData, "ilsOn", maxBufferSize = maxBufferSize,
         "totalBuffer", runNumber = 1,
         lookAround = 0, p = 0.318, operationSequence = "231",
         numberOfAnts = 10, withACO = FALSE, withLS = T,
         withReconstruct = F, targetCriterion = "TFT",
         useRandomPermutationForLS = F,
         withNEH = T, withInitialReconstruct =F
)

startHVNS(jobData, logFileName = "hvns", maxBufferSize = maxBufferSize,
          bufferType = "totalBuffer", targetCriterion = "TFT",
          runNumber=1)



testILSNo <- read.table("./output/ilsOff-ils-1", sep=",", header=F)
testILS <- read.table("./output/ilsOn-ils-1", sep=",", header=F)


timeAxis <- 1

minX <- 0
maxX <- max(testILSNo[,timeAxis])*1.1
maxY <- max(c(testILSNo$V4, testILS$V4))
minY <- min(c(testILSNo$V4, testILS$V4))

plot(testILSNo[,timeAxis], testILSNo[,"V4"], "l", col="black",
     xlim = c(minX, maxX),
     ylim = c(minY, maxY))
lines(testILS[,timeAxis], testILS[,"V4"], "l", col="violet")

#lines(testILS[,timeAxis], testILS[,"V3"]*100+8000, "l", col="black")
span <- (max(testILSNo[,"V4"])+min(testILSNo[,"V4"]))
mid <- span/2
lines(testILSNo[,timeAxis], testILSNo[,3]*span*0.0001+mid, "l", col="black", lty="dashed")
lines(testILS[,timeAxis], testILS[,3]*span*0.0001+mid, "l", col="violet", lty="dashed")



