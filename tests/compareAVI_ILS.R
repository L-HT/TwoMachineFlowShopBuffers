# AVI und ILS vergleichen

instance <- "./oldInstances/instances/f2total-100-m2const-bufm1-mid-130-1"
instance <- "./oldInstances/instances/f2total-75-m2const-bufm1-big-133-3"
instance <- "./oldInstances/instances/f2total-50-m2const-bufm1-mid-131-1"


instanceData <- parseFlowShopName(instance)

jobData <- read.csv(file = instance, stringsAsFactors = F)
jobData <- generateProblemInstance(250, c(1,100),58,-1)

startAVI(jobData, "avi", maxBufferSize = as.integer(instanceData["maxBufferSize"]),
         "totalBuffer", runNumber = 1, timeGradient = 0.6,
         lookAround = 1, p = 0.69, useRL = F)

startHVNS(jobData, "hvns", maxBufferSize = as.integer(instanceData["maxBufferSize"]),
          "totalBuffer", runNumber = 1)

startILS(jobData, "ils", maxBufferSize = as.integer(instanceData["maxBufferSize"]),
         "totalBuffer", runNumber = 10,
         lookAround = 0, p = 0.982, operationSequence = "213")

testAVI <- read.table("./output/avi-avi-1", sep=",", header=F)
testHVNS <- read.table("./output/hvns-hvns-1", sep=",", header=F)
testILS <- read.table("./output/ils-ils-10", sep=",", header=F)

# Plotten und so

timeAxis <- 1

plot(testILS[,timeAxis], testILS[,"V4"], "l", col="green")
lines(testHVNS[,timeAxis], testHVNS[,"V4"], "l", col="violet")
lines(testAVI[,timeAxis], testAVI[,"V4"], "l", col="brown")
#lines(testILS[,timeAxis], testILS[,"V3"]*100+8000, "l", col="black")
lines(testILS[,timeAxis], testILS[,3]*30+8000, "l")

#############
s <- "j34,j59,j40,j54,j94,j29,j58,j41,j17,j43,j12,j75,j44,j47,j26,j22,j27,j21,j14,j73,j74,j36,j30,j67,j2,j95,j4,j15,j20,j97,j32,j83,j6,j78,j76,j87,j1,j64,j70,j62,j79,j35,j66,j81,j57,j91,j82,j60,j33,j100,j25,j80,j71,j38,j39,j88,j85,j18,j68,j24,j61,j84,j28,j11,j7,j65,j31,j16,j45,j92,j37,j46,j52,j3,j10,j56,j63,j55,j9,j99,j49,j77,j72,j86,j69,j96,j51,j5,j13,j90,j93,j53,j23,j19,j42,j89,j8,j98,j48,j50"
perm <- strsplit(s, ",")[[1]]

s <- "j3,j6,j5,j2,j8,j7,j4,j9,j10,j1"
perm2 <- strsplit(s, ",")[[1]]
perm2 <- perm
simulateFlowShopTotalBufferC(jobData, perm, perm2, as.integer(instanceData["maxBufferSize"]))

simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm2, as.integer(instanceData["maxBufferSize"]))
plotSchedule(simRes, jobData, as.integer(instanceData["maxBufferSize"]), 100)


##############

s <- "22,40,65,74,10,29,59,30,15,75,5,61,6,58,66,18,55,21,49,44,20,17,35,8,26,9,51,45,63,69,60,52,13,62,56,68,38,48,72,57,2,47,54,39,23,14,3,24,37,36,41,12,70,7,11,32,27,43,28,46,71,53,19,33,4,16,25,34,50,64,67,73,31,42,1"
splitt <- strsplit(s, ",")[[1]]
perm <- paste("j",splitt, sep="")
simulateFlowShopTotalBufferC(jobData, perm, perm, as.integer(instanceData["maxBufferSize"]))

