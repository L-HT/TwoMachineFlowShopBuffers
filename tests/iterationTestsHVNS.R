jobData <- read.csv(file = "./instances/f2total-100-m2const-bufm1-mid-130-1", stringsAsFactors = F)
jobData <- read.csv(file = "./instances/f2total-75-m2const-bufm1-big-133-3", stringsAsFactors = F)

jobData <- read.csv(file = "./instances/f2total-50-m2const-bufm1-mid-131-1", stringsAsFactors = F)

startHVNS(jobData, logFileName = "iterationTest100", maxBufferSize = 130, bufferType = "totalBuffer",
          runNumber = 5)

# 3: normal
# 4: edgeInsertionSA vor insertionSA
# 5: LS zu erst

test <- read.table("./output/iterationTest100-hvns-3", header=F, sep=",")
#plot(test$V1,log(test$V3,base = 1),"l")

timeAxis <- 1
plot(test[,timeAxis],test[,3],"l",xlim=c(0,50000))

#skaliere Makespan
test$V4 <- min(test[,3])+(test$V4-min(test$V4))*(max(test[,3])-min(test[,3]))/(max(test$V4)-min(test$V4))
lines(test[,timeAxis],test$V4, col="red","l")


abline(v=test[min(which(test$V3 == -1)),timeAxis])
abline(v=test[min(which(test$V3 == -2)),timeAxis])

test2 <- read.table("./output/iterationTest100-hvns-4", header=F, sep=",")
test2$V4 <- min(test[,3])+(test2$V4-min(test2$V4))*(max(test[,3])-min(test[,3]))/(max(test2$V4)-min(test2$V4))
lines(test2[,timeAxis],test2$V4, col="blue","l")

test3 <- read.table("./output/iterationTest100-hvns-5", header=F, sep=",")
test3$V4 <- min(test[,3])+(test3$V4-min(test3$V4))*(max(test[,3])-min(test[,3]))/(max(test3$V4)-min(test3$V4))
lines(test3[,timeAxis],test3$V4, col="green","l")
