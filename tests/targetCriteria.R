# mal die neuen Zielkriterien probieren

set.seed(313131)
N <- 5
jobData <- generateProblemInstance(N, c(1,10), c(1,10), -1)
jobData <- appendDeadlinesToJobData(jobData, "totalBuffer", 12)

perm <- paste("j",1:N, sep="")

simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, 12)
plotSchedule(simRes, jobData, 12, 1)

simulateFlowShopTotalBufferC_DueTime(jobData, perm, perm, 12)

set.seed(4)
N <- 5
jobData <- generateProblemInstance(N, c(1,10), c(1,10), -1)
jobData <- appendDeadlinesToJobData(jobData, "intermediateBuffer", 3)

perm <- paste("j",1:N, sep="")

simRes <- simulateFlowShop(jobData, perm, perm, 12)
plotSchedule(simRes, jobData, 3, 1)

simulateFlowShopC_DueTime(jobData, perm, perm, 12)


##############################

N <- 25
jobData <- generateProblemInstance(N, c(1,100), c(1,100), -1)
jobData <- appendDeadlinesToJobData(jobData, "intermediateBuffer", 50)

perm <- paste("j",1:N, sep="")

simRes <- simulateFlowShop(jobData, perm, perm, 12)
plotSchedule(simRes, jobData, 3, 1)

simulateFlowShopC_DueTime(jobData, perm, perm, 12)

startILS(jobData, "duedate", 50, "intermediateBuffer", "dueTimes",1,"")
startACOLS(jobData, "duedate", 50, "intermediateBuffer", "dueTimes",1,"",withReconstruct = F)


# Plotten und so

testACOLS <- read.table("./output/duedate-acols-1", sep=",", header=F)
testILS <- read.table("./output/duedate-ils-1", sep=",", header=F)

timeAxis <- 1

plot(testILS[,timeAxis], testILS[,"V4"], "l", col="aquamarine")
lines(testACOLS[,timeAxis], testACOLS[,"V4"], "l", col="red")

#lines(testILS[,timeAxis], testILS[,"V3"]*100+8000, "l", col="black")
span <- (max(testILS[,"V4"])+min(testILS[,"V4"]))
mid <- span/2
lines(testILS[,timeAxis], testILS[,3]*span*0.01+mid, "l", col="black", lty="dashed")
lines(testACOLS[,timeAxis], testACOLS[,3]*span*0.01+mid, "l", col="red", lty="dashed")
