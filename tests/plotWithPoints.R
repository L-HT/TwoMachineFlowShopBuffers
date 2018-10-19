bufferSize <- 7
N <- 2

appendSimResRow <- function(simRes, name, job, start, end){
  simRes[nrow(simRes) + 1,] <- list(name, job, start, end)
  return(simRes)
}

jobData <- generateProblemInstance(N,c(1,5),c(1,5),-1)
perm1 <- paste("j",1:N,sep="")
perm2 <- paste("j",1:N,sep="")

simRes <- simulateFlowShopTotalBuffer(jobData, perm1, perm2, bufferSize <- 5)
simRes <- simRes[0,]

d <- 1
simRes <- appendSimResRow(simRes, "M1", "i", 5, 9)
simRes <- appendSimResRow(simRes, "M1", "j", 15, 18)
simRes <- appendSimResRow(simRes, "M2", "j", 24-d, 28-d)
simRes <- appendSimResRow(simRes, "M2", "i", 34-d, 38-d)

simRes <- appendSimResRow(simRes, "dotsM1", "", 0, 4)
simRes <- appendSimResRow(simRes, "dotsM1", "", 10, 14)
simRes <- appendSimResRow(simRes, "dotsM1", "", 19, 23)
simRes <- appendSimResRow(simRes, "dotsM2", "", 19-d, 23-d)
simRes <- appendSimResRow(simRes, "dotsM2", "", 29-d, 33-d)
simRes <- appendSimResRow(simRes, "dotsM2", "", 39-d, 43-d)
#
# simRes <- appendSimResRow(simRes, "dotsM1", "", 1, 3)
# simRes <- appendSimResRow(simRes, "M1", "i", 4, 5)
# simRes <- appendSimResRow(simRes, "dotsM1", "", 6, 8)
# simRes <- appendSimResRow(simRes, "M1", "j", 9, 10)
# simRes <- appendSimResRow(simRes, "dotsM2", "", 11, 13)
# simRes <- appendSimResRow(simRes, "M2", "j", 14, 15)
# simRes <- appendSimResRow(simRes, "dotsM2", "", 15, 17)
# simRes <- appendSimResRow(simRes, "M2", "i", 18, 19)
# simRes <- appendSimResRow(simRes, "dotsM2", "", 20, 22)

plotScheduleNoTime(simRes, jobData, bufferSize, -1)

###########################
###########################

MYWIDTH = 700
MYHEIGHT = 373
MYPOINTSIZE = 17

################### Beispiel Seite 27

jobData <- generateProblemInstance(6, c(1,10),sample(1:10,1),-1)
jobData$m1Time <- c(18,3,3,4,6,12)
jobData$m2Time <- 5
bufferSize <- 24
jobData$bufferUsage <- jobData$m1Time
perm1 <- paste("j",c(1,2,3,4,5,6), sep="")
perm2 <- paste("j",c(1,4,2,3,5,6), sep="")


simRes <- simulateFlowShopTotalBuffer(jobData, perm1, perm2, bufferSize)

simRes[c(1,2,3,4,5,6),] <- simRes[c(1,3,5,6,4,2),]
#simRes[c(1,2,3,4,5,6),] <- simRes[c(6,5,4,3,2,1),]

simRes[c(7:9),1] <- "M1g"
simRes[c(13,15,16),1] <- "M2g"


labelDf1 <- data.frame(pos=c(5,10,12),text=c("A","B/2","c"))
labelDf2 <- data.frame(pos=c(30,35,45),text=c("x","Y","z"))
labelDf <- list(labelDf1, labelDf2)
tickPos <- list(c(0,7,16), c(25,35,47))

png("p84a.png", width=MYWIDTH, height=MYHEIGHT, pointsize=MYPOINTSIZE)
plotScheduleTheory(simRes,jobData, bufferSize,bai = 1, tickPos, labelDf)
dev.off()
