# AVI und ILS vergleichen
library(TwoMachineFlowShopEx)
instance <- "./instances/f2total-100-m2const-bufm1-mid-130-1"
instance <- "./instances/f2total-75-m2const-bufm1-big-133-3"
instance <- "./instances/f2total-50-m2const-bufm1-mid-131-1"

instanceData <- parseFlowShopName(instance)

jobData <- read.csv(file = instance, stringsAsFactors = F)

# Rcpp-Konstruktor explizit aufrufen, um die Objekte zu erzeugen?

# startILS(jobData, "ils", maxBufferSize = as.integer(instanceData["maxBufferSize"]),
         # "totalBuffer", runNumber = 100,
         # lookAround = 0, p = 0.1, operationSequence = "2")


startACOLS(jobData, "acolsNo", maxBufferSize = as.integer(instanceData["maxBufferSize"]),
           "totalBuffer", runNumber = 100,
           lookAround = 0, p = 0.9, operationSequence = "2",
           numberOfAnts = 10, withACO = FALSE, withReconstruct = FALSE
           )

startACOLS(jobData, "acols", maxBufferSize = as.integer(instanceData["maxBufferSize"]),
           "totalBuffer", runNumber = 100,
           lookAround = 0, p = 0.9, operationSequence = "2",
           numberOfAnts = 10,evaporation = 0.8)


testACOLSNo <- read.table("./output/acolsNo-acols-100", sep=",", header=F)
testACOLS <- read.table("./output/acols-acols-100", sep=",", header=F)
testILS <- read.table("./output/ils-ils-100", sep=",", header=F)

# Plotten und so

timeAxis <- 1

plot(testILS[,timeAxis], testILS[,"V4"], "l", col="black")
lines(testACOLSNo[,timeAxis], testACOLSNo[,"V4"], "l", col="violet")
lines(testACOLS[,timeAxis], testACOLS[,"V4"], "l", col="red")

#lines(testILS[,timeAxis], testILS[,"V3"]*100+8000, "l", col="black")
span <- (max(testILS[,"V4"])+min(testILS[,"V4"]))
mid <- span/2
lines(testILS[,timeAxis], testILS[,3]*span*0.001+mid, "l", col="black", lty="dashed")
lines(testACOLSNo[,timeAxis], testACOLSNo[,3]*span*0.001+mid, "l", col="violet", lty="dashed")
lines(testACOLS[,timeAxis], testACOLS[,3]*span*0.001+mid, "l", col="red", lty="dashed")


#nach einer Iteration ist ACO schon ganz gut, aber Pheromonmatrix
#hat noch zu wenige Info, macht also wieder schlechte Lösung
#und braucht Zeit, sie zu verbessern
