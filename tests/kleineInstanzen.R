set.seed(101)
bla <- generateProblemInstance(3, c(1,5), c(1,5), c(1,5))
bla$job <- paste("J", 1:3,sep="")
# sortiert:
sort <- c("j1", "j3", "j2")

rl <- 20
setPerm(c("j1"))
neh <- paste("j", getNEHSolution(bla, "intermediateBuffer", 999, 0), sep="")
simulateFlowShopC(bla, neh, neh, 999)

simRes <- simulateFlowShop(bla, perm1, perm2, 999)
plotScheduleNoBuffer(simRes, bla, bai=1, until = rl)

setPerm(c("J3", "J1"))
simRes <- simulateFlowShop(bla, perm1, perm2, 999)
plotScheduleNoBuffer(simRes, bla, bai=1, until = rl)

setPerm(c("J1", "J3"))
simRes <- simulateFlowShop(bla, perm1, perm2, 999)
plotScheduleNoBuffer(simRes, bla, bai=1, until = rl)

setPerm(c("J2", "J1", "J3"))
simRes <- simulateFlowShop(bla, perm1, perm2, 999)
plotScheduleNoBuffer(simRes, bla, bai=1, until = rl)

setPerm(c("J1", "J2", "J3"))
simRes <- simulateFlowShop(bla, perm1, perm2, 999)
plotScheduleNoBuffer(simRes, bla, bai=1, until = rl)

setPerm(c("J1", "J3", "J2"))
simRes <- simulateFlowShop(bla, perm1, perm2, 10)
plotScheduleNoBuffer(simRes, bla, bai=1, until = rl)


#############

setPerm <- function(str){
  perm1 <<- str
  perm2 <<- perm1
}


plotScheduleNoBuffer <- function(simRes, jobData, bai, until){
  sizeMachines <- 15
  rightLimit <- max(simRes$end)+1
  if (until > 1){
    rightLimit <- until
  }
  plot(c(0, rightLimit), c(0,-100), type = "n", xlab = "", ylab = "",
       yaxt = "n", xaxt = "n", bty="n")


  axis(3, lwd=2, line=NA, at = pretty(0:(rightLimit)))
  axis(3, lwd=1, line=NA, labels = F, at = seq(0, (rightLimit), bai))
  axis(2, at = c(16, -0.5*sizeMachines, -1.5*sizeMachines, -68),
       labels = c("Zeit     ", "M1     ", "M2     ", "Speicher"),
       las=1,tick=F, xpd=TRUE,  line=-1)

  bufferAreaTop <- -2*sizeMachines - 10
  bufferAreaBottom <- -100
  bufferAreaSize <- abs(bufferAreaTop - bufferAreaBottom)
  bufferRectangles <- data.frame("left" = c(), "top" = c(), "right" = c(), "bottom" = c())

  rect(0, bufferAreaTop, max(simRes$end)+1, bufferAreaBottom,
       col="gray95")#density=10)

  for (type in unique(simRes$type)){
    sub <- simRes[simRes$type == type,]
    if(type == "M1"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines)
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -0.5*sizeMachines, labels = sub[k,"job"])
      }
    }
    if(type == "block"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines,density = 20)
      }
    }
    if(type == "M2"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-2*sizeMachines)
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, labels = sub[k,"job"])

      }
    }
    #Buffer hat Platz von
    if(type == "buffer"){
      sub <- sub[order(sub$start),]
      for (k in 1:nrow(sub)){
        bufferUsage <- (jobData[jobData$job == sub[k, "job"], "bufferUsage"]
                        / bufferSize) * bufferAreaSize
        left <- sub[k, "start"]
        right <- sub[k, "end"]+1
        top <- bufferAreaTop
        bottom <- bufferAreaTop - bufferUsage

        checking <- TRUE
        while(checking){
          checkResult <- apply(bufferRectangles, MARGIN = 1,
                               FUN = function(r){checkOverlap(r, left, top, right, bottom)})
          if (any(checkResult)){
            top <- max(bufferRectangles[checkResult,"bottom"])
            bottom <- top - bufferUsage
          } else {
            checking <- FALSE
          }
        }
        bufferRectangles <- rbind(bufferRectangles,
                                  data.frame("left" = left, "top" = top, "right" = right,
                                             "bottom" = bottom))

        bottomFixed <- FALSE
        # Randkorrektur
        if (bottom < bufferAreaBottom){
          bottom <- bufferAreaBottom
          bottomFixed <- TRUE
        }
        mid <- c((left+right)/2, (top+bottom)/2)
        rect(left, top, right, bottom, col="white")
        if (bottomFixed){
          for (l in seq(left, right, by=bai)){
            lines(x=c(l,l), y=c(bufferAreaBottom, bufferAreaBottom+2))
          }
        }
        text(x=mid[1], y=mid[2], labels = sub[k, "job"])

      }
    }
  }
}

###############
###############

set.seed(100)
bla <- generateProblemInstance(5, c(1,5), c(1,5), -1)
bla$job <- paste("J", 1:5, sep="")
# sortiert:

perm1 <- c("J1", "J2", "J3", "J4", "J5")
perm2 <- c("J2", "J1", "J3", "J5", "J4")
simRes <- simulateFlowShop(bla, perm1, perm2, 5)
plotSchedule(simRes, bla, bai=1, 5)


#set.seed(105)
#bla <- generateProblemInstance(5, c(1,5), c(1,5), c(1,5))
#bla$job <- paste("J", 1:5, sep="")
#setPerm(paste("J",1:5,sep=""))
#perm1 <- c("J1", "J2", "J3", "J4", "J5")
#perm2 <- c("J1", "J3", "J2", "J4", "J5")
simRes2 <- simulateFlowShopTotalBuffer(bla, perm1, perm2, 8)
simRes2[c(7,8,9),] <- simRes2[c(9,8,7),]
plotSchedule(simRes2, bla, bai=1, 8)

bla <- generateProblemInstance(5, c(1,5), c(1,5), c(1,5))
bla$job <- paste("J", 1:5, sep="")
setPerm(paste("J",1:5,sep=""))
perm1 <- c("J1", "J2", "J3", "J4", "J5")
perm2 <- c("J1", "J3", "J2", "J4", "J5")
simRes2 <- simulateFlowShop(bla, perm1, perm2, 4)
simRes2[c(7,8,9),] <- simRes2[c(9,8,7),]
plotSchedule(simRes2, bla, bai=1, 4)

########

# Blocking
set.seed(131)
bla <- generateProblemInstance(5, c(2,4), c(3,6), c(1,5))
bla$job <- paste("J", 1:5, sep="")

perm1 <- c("J1", "J2", "J3", "J4", "J5")
perm2 <- perm1
simRes <- simulateFlowShop(bla, perm1, perm2, 0)
plotSchedule(simRes, bla, bai=1, 0)


#No Wait
set.seed(131)
bla <- generateProblemInstance(5, c(2,4), c(3,6), c(1,5))
bla$job <- paste("J", 1:5, sep="")
# sortiert:

perm1 <- c("J1", "J2", "J3", "J4", "J5")
perm2 <- perm1
simRes2 <- simulateFlowShop(bla, perm1, perm2, 0)

simRes2 <- simRes2[1:10,]
simRes2[simRes2$job=="J2" & simRes2$type=="M1",c("start", "end")] <- c(5,6)
simRes2[simRes2$job=="J3" & simRes2$type=="M1",c("start", "end")] <- c(10,11)
plotSchedule(simRes2, bla, bai=1, 0)
