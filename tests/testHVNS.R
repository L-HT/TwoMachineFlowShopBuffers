jobData <- generateProblemInstance(25, c(1,100), c(1,100), c(1,100))
print(jobData)

startHVNS(jobData, "sorcerehvns",1,"50b")
startHVNS(jobData, logFileName = "iterationTest", maxBufferSize = 130, runNumber = 1)
#####################################


test <- read.csv("./output/sorcerehvns-hvns-1-50b", header=F,sep=",")
test <- read.csv("./output/sorcerehvns-hvns-1-100", header=F,sep=",")
test$V2 <- trunc(test$V2)
nta <- as.integer(table(test$V2))
mean(nta)

P25 <- 10
P50 <- 7
P75 <- 5
P100 <- 3
sizeInkB <- 60*(P25*5 + P50*10 + P75*15 + P100*20) * 10800
sizeInGB <- sizeInkB / 1000000
