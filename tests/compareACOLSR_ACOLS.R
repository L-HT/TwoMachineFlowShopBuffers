# AVI und ILS vergleichen
library(TwoMachineFlowShopEx)
instance <- "./instances/f2total-100-m2const-bufm1-big-130-2"
instance <- "./oldInstances/instances/f2total-75-m2const-bufm1-mid-139-2"
instance <- "./oldInstances/instances/f2total-50-m2const-bufm1-big-142-2"
instance <- "./instances/instances50100/f2total-50-m2const-bufm1-small-124-1"
instance <- "./instances/f2total-10-m2var-bufm1-c-128-1"

instanceData <- parseFlowShopName(instance)
maxBufferSize <- as.integer(instanceData["maxBufferSize"])
jobData <- read.csv(file = instance, stringsAsFactors = F)

# Rcpp-Konstruktor explizit aufrufen, um die Objekte zu erzeugen?
startDABC(jobData, "dabc", maxBufferSize, "totalBuffer", "dueTimes", 1, "")
startHVNS(jobData, "hvns", maxBufferSize, "totalBuffer", "TFT", 1,"")

startILS(jobData, "ils", maxBufferSize = maxBufferSize,
           "totalBuffer", runNumber = 100,
           lookAround = 0, p = 0.9, operationSequence = "2",
           numberOfAnts = 10, withACO = FALSE, withLS = T,
          withReconstruct = F, targetCriterion = "TFT",
         useRandomPermutationForLS = T
)
startILS(jobData, "ils", maxBufferSize = maxBufferSize,
         "totalBuffer", runNumber = 101,
         lookAround = 0, p = 0.9, operationSequence = "2",
         numberOfAnts = 10, withACO = FALSE, withLS = T,
         withReconstruct = F, targetCriterion = "TFT",
         useRandomPermutationForLS = T
)


startILS(jobData, "ils", maxBufferSize = maxBufferSize,
         "totalBuffer", runNumber = 101,
         lookAround = 0, p = 0.9, operationSequence = "2",
         numberOfAnts = 10, withACO = FALSE, withLS = T,
         withReconstruct = T, useRandomPermutationForReconstruct = T,
         targetCriterion = "TFT"
)

startACOLS(jobData, "dt", maxBufferSize = maxBufferSize,
           "totalBuffer", runNumber = 1, withReconstruct = F,
           targetCriterion = "dueTimes"
           )

startILS(jobData, "ilsNo", maxBufferSize = maxBufferSize,
            "totalBuffer", runNumber = 100,
           lookAround = 0, p = 0.9, operationSequence = "2",
           numberOfAnts = 10,evaporation = 0.8, withReconstruct = F,
         targetCriterion = "dueTimes")


testACOLS <- read.table("./output/hvns-hvns-1", sep=",", header=F)
testILSNo <- read.table("./output/ilsOff-ils-1", sep=",", header=F)
testILS <- read.table("./output/ilsOn-ils-1", sep=",", header=F)

perm <- paste("j",1:50, sep="")
simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, maxBufferSize)
sum(simRes[simRes$type=="M2", "end"] + 1)
simulateFlowShopTotalBufferC_TFT(jobData, perm, perm, maxBufferSize)
# Plotten und so

timeAxis <- 2

plot(testILS[,timeAxis], testILS[,"V4"], "l", col="black", xlim = c(0,max(testILSNo[,timeAxis])*1.1))
lines(testILSNo[,timeAxis], testILSNo[,"V4"], "l", col="violet")
lines(testACOLS[,timeAxis], testACOLS[,"V4"], "l", col="red")

#lines(testILS[,timeAxis], testILS[,"V3"]*100+8000, "l", col="black")
span <- (max(testILSNo[,"V4"])+min(testILSNo[,"V4"]))
mid <- span/2
lines(testILSNo[,timeAxis], testILSNo[,3]*span*0.001+mid, "l", col="black", lty="dashed")
lines(testILS[,timeAxis], testILS[,3]*span*0.001+mid, "l", col="violet", lty="dashed")
lines(testACOLS[,timeAxis], testACOLS[,3]*span*0.001+mid, "l", col="red", lty="dashed")


#nach einer Iteration ist ACO schon ganz gut, aber Pheromonmatrix
#hat noch zu wenige Info, macht also wieder schlechte Lösung
#und braucht Zeit, sie zu verbessern
