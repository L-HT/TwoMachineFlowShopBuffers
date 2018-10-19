require(grDevices)
## set up the plot region:

checkOverlap <- function(row, left, top, right, bottom){
  # x-Ueberlappung
  #print(row)
  xOverlap <- (row["left"] < right && left < row["right"]) ||
    (left < row["right"] && row["left"] < right)
  yOverlap <- (row["top"] > bottom && top > row["bottom"]) ||
    (top > row["bottom"] && row["top"] > bottom)
  return(xOverlap & yOverlap)
}
calculateMidPoints <- function(values){
  result <- c()
  for (i in 1:(length(values)-1)){
    result <- c(result, (values[i]+values[i+1])/2)
  }
  return(result)
}

#' @export
plotSchedule <- function(simRes, jobData, bufferSize, bai){
  sizeMachines <- 15
  plot(c(0, max(simRes$end)+1), c(0,-100), type = "n", xlab = "", ylab = "",
       yaxt = "n", xaxt = "n", bty="n")


  axis(3, lwd=2, line=NA, at = pretty(0:(max(simRes$end)+1)))
  axis(3, lwd=1, line=NA, labels = F, at = seq(0, (max(simRes$end)+1), bai))
  axis(2, at = c(16, -0.5*sizeMachines, -1.5*sizeMachines, -68),
       labels = c("Time     ", "M1     ", "M2     ", "Buffer"),
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
    if(type == "M1g"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines, col="grey88")
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
    if(type == "M2g"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-2*sizeMachines, col="grey88")
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, labels = sub[k,"job"])

      }
    }
    if (type == "dotsM1"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"], length.out = 5)
        points(x[-c(1,5)],rep(-0.5*sizeMachines,3), pch = 20)
      }
    }
    if (type == "dotsM2"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"], length.out = 5)
        points(x[-c(1,5)],rep(-1.5*sizeMachines,3), pch = 20)
        #rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-sizeMachines,density = 20)
      }
    }
    #Buffer hat Platz von
    if(type == "buffer"){
      #sub <- sub[order(sub$start),]
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


#' @export
plotScheduleNoTime <- function(simRes, jobData, bufferSize, bai, noBuffer=T,
                               fontScaling=1.0, rightLim=-1,
                               leftLabels = c("        ", "M1     ", "M2     ", "Buffer")){
  sizeMachines <- 15
  if (rightLim < max(simRes$end)+1){
    rightLim <- max(simRes$end)+1
  }
  plot(c(0, rightLim), c(0,-100), type = "n", xlab = "", ylab = "",
       yaxt = "n", xaxt = "n", bty="n")

  if (bai > 0){
    axis(3, lwd=2, line=NA, at = pretty(0:(max(simRes$end)+1)),labels=F, lwd.tick=0)
    axis(3, lwd=1, line=NA, labels = F, at = seq(0, (max(simRes$end)+1), bai))
  }
  # leftLabels <- c("        ", "M1     ", "M2     ", "Buffer")
  # if (noBuffer){
  #   leftLabels <- c("        ", "M1     ", "M2     ", "")
  # }
  axis(2, at = c(16, -0.5*sizeMachines, -1.5*sizeMachines, -68),
       labels = leftLabels,
       las=1,tick=F, xpd=TRUE,  line=-1, cex.axis=fontScaling)
  bufferAreaTop <- -2*sizeMachines - 10
  bufferAreaBottom <- -100
  bufferAreaSize <- abs(bufferAreaTop - bufferAreaBottom)
  bufferRectangles <- data.frame("left" = c(), "top" = c(), "right" = c(), "bottom" = c())

  if (!noBuffer){
  rect(0, bufferAreaTop, max(simRes$end)+1, bufferAreaBottom,
       col="gray95")#density=10)
  }

  for (type in unique(simRes$type)){
    sub <- simRes[simRes$type == type,]
    if(type == "M1"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines)
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -0.5*sizeMachines, labels = sub[k,"job"], cex=fontScaling)
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
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, cex=fontScaling,labels = sub[k,"job"])

      }
    }
    if (type == "dotsM1"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"]+1, length.out = 5)
        points(x[-c(1,5)],rep(-0.5*sizeMachines,3), pch = 20)
      }
    }
    if (type == "dotsM2"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"]+1, length.out = 5)
        points(x[-c(1,5)],rep(-1.5*sizeMachines,3), pch = 20)
        #rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-sizeMachines,density = 20)
      }
    }
    # if(type == "M2"){
    #   for (k in 1:nrow(sub)){
    #     rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-2*sizeMachines)
    #     text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, cex=fontScaling, labels = sub[k,"job"])
    #
    #   }
    # }
    if(type == "M2g"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-2*sizeMachines, col="grey88")
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, cex=fontScaling, labels = sub[k,"job"])

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


#' @export
plotScheduleTheory <- function(simRes, jobData, bufferSize, bai,
                               tickPos, labelDf, lowerTickPos=NULL, lowerLabelDf = NULL,
                               fontScaling=1, noBuffer=FALSE){
  sizeMachines <- 15
  plot(c(0, max(simRes$end)+1), c(0,-100), type = "n", xlab = "", ylab = "",
       yaxt = "n", xaxt = "n", bty="n")

  if (is.null(tickPos)){
    tickPos <- pretty(0:(max(simRes$end)+1))
  }

  if (class(tickPos) == "list" & class(labelDf) == "list"){
    for (i in 1:length(tickPos)){
      tempTickPos <- tickPos[[i]]
      if (i == 1){
        tempTickPos <- c(0,tempTickPos)
      }
      if (i == length(tickPos)){
        tempTickPos <- c(tempTickPos,max(simRes$end)+1)
      }

      axis(3, lwd=2, line=NA, at = tempTickPos, labels = F)
      axis(3, lwd=0, line=NA, labels = labelDf[[i]]$text,
           at = calculateMidPoints(tempTickPos),
           pos = 1, cex.axis=fontScaling)
    }
  } else {
    axis(3, lwd=2, line=NA, at = c(0,tickPos,max(simRes$end)+1), labels = F)
    axis(3, lwd=0, line=NA, labels = labelDf$txt, at = labelDf$pos,
         pos = -1)
  }


  if(!is.null(lowerTickPos)){
    lowerAxisPos = -2*sizeMachines - 5
    bufferAreaTop <- -2*sizeMachines - 15
    axis(1, lwd=2, line=NA, at = c(0,lowerTickPos,max(simRes$end)+1),
         labels = F, pos=lowerAxisPos)
    axis(1, lwd=0, line=NA, labels = lowerLabelDf$txt,
         at = lowerLabelDf$pos, pos = lowerAxisPos+4)
  } else {
    bufferAreaTop <- -2*sizeMachines - 10
  }

  leftLabels <- c("        ", "M1     ", "M2     ", "Buffer")
  if (noBuffer){
    leftLabels <- c("        ", "M1     ", "M2     ", "")
  }
  axis(2, at = c(16, -0.5*sizeMachines, -1.5*sizeMachines, -68),
       labels = leftLabels,
       las=1,tick=F, xpd=TRUE,  line=-1, cex.axis=fontScaling)

  bufferAreaTop <- -2*sizeMachines - 10
  bufferAreaBottom <- -100
  bufferAreaSize <- abs(bufferAreaTop - bufferAreaBottom)
  bufferRectangles <- data.frame("left" = c(), "top" = c(), "right" = c(), "bottom" = c())

  if (!noBuffer){
  rect(0, bufferAreaTop, max(simRes$end)+1, bufferAreaBottom,
       col="gray95")#density=10)
  }
  for (type in unique(simRes$type)){
    sub <- simRes[simRes$type == type,]
    if(type == "M1"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines)
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -0.5*sizeMachines, labels = sub[k,"job"],
             cex=fontScaling)
      }
    }
    if(type == "M1g"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines, col="grey88")
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
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, labels = sub[k,"job"],
             cex=fontScaling)

      }
    }
    if(type == "M2g"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-2*sizeMachines, col="grey88")
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, labels = sub[k,"job"])

      }
    }
    if (type == "dotsM1"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"], length.out = 5)
        points(x[-c(1,5)],rep(-0.5*sizeMachines,3), pch = 20,cex=fontScaling)
      }
    }
    if (type == "dotsM2"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"], length.out = 5)
        points(x[-c(1,5)],rep(-1.5*sizeMachines,3), pch = 20,cex=fontScaling)
        #rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-sizeMachines,density = 20)
      }
    }
    #Buffer hat Platz von
    if(type == "buffer"){
      #sub <- sub[order(sub$start),]
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


#' @export
plotScheduleTheory2 <- function(simRes, jobData, bufferSize, bai,
                               tickPos, labelDf, lowerTickPos=NULL, lowerLabelDf = NULL,
                               fontScaling=1, noBuffer=FALSE){
  sizeMachines <- 15
  plot(c(0, max(simRes$end)+1), c(0,-100), type = "n", xlab = "", ylab = "",
       yaxt = "n", xaxt = "n", bty="n")

  if (is.null(tickPos)){
    tickPos <- pretty(0:(max(simRes$end)+1))
  }

  if (class(tickPos) == "list" & class(labelDf) == "list"){
    for (i in 1:length(tickPos)){
      tempTickPos <- tickPos[[i]]
      if (i == 1){
        tempTickPos <- c(0,tempTickPos)
      }
      if (i == length(tickPos)){
        tempTickPos <- c(tempTickPos,max(simRes$end)+1)
      }

      axis(3, lwd=2, line=NA, at = tempTickPos, labels = F)
      axis(3, lwd=0, line=NA, labels = parse(text=labelDf[[i]]$text),
           at = calculateMidPoints(tempTickPos),
           pos = 1, cex.axis=fontScaling)
    }
  } else {
    axis(3, lwd=2, line=NA, at = c(0,tickPos,max(simRes$end)+1), labels = F)
    axis(3, lwd=0, line=NA, labels = labelDf$txt, at = labelDf$pos,
         pos = -1)
  }


  if(!is.null(lowerTickPos)){
    lowerAxisPos = -2*sizeMachines - 5
    bufferAreaTop <- -2*sizeMachines - 15
    axis(1, lwd=2, line=NA, at = c(0,lowerTickPos,max(simRes$end)+1),
         labels = F, pos=lowerAxisPos)
    axis(1, lwd=0, line=NA, labels = lowerLabelDf$txt,
         at = lowerLabelDf$pos, pos = lowerAxisPos+4)
  } else {
    bufferAreaTop <- -2*sizeMachines - 10
  }

  leftLabels <- c("        ", "M1     ", "M2     ", "Buffer")
  if (noBuffer){
    leftLabels <- c("        ", "M1     ", "M2     ", "")
  }
  axis(2, at = c(16, -0.5*sizeMachines, -1.5*sizeMachines, -68),
       labels = leftLabels,
       las=1,tick=F, xpd=TRUE,  line=-1, cex.axis=fontScaling)

  bufferAreaTop <- -2*sizeMachines - 10
  bufferAreaBottom <- -100
  bufferAreaSize <- abs(bufferAreaTop - bufferAreaBottom)
  bufferRectangles <- data.frame("left" = c(), "top" = c(), "right" = c(), "bottom" = c())

  if (!noBuffer){
    rect(0, bufferAreaTop, max(simRes$end)+1, bufferAreaBottom,
         col="gray95")#density=10)
  }
  for (type in unique(simRes$type)){
    sub <- simRes[simRes$type == type,]
    if(type == "M1"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines)
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -0.5*sizeMachines, labels = sub[k,"job"],
             cex=fontScaling)
      }
    }
    if(type == "M1g"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -0, sub[k, "end"]+1,-sizeMachines, col="grey88")
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
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, labels = sub[k,"job"],
             cex=fontScaling)

      }
    }
    if(type == "M2g"){
      for (k in 1:nrow(sub)){
        rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-2*sizeMachines, col="grey88")
        text(x=(sub[k, "start"]+sub[k, "end"]+1)/2, y= -1.5*sizeMachines, labels = sub[k,"job"])

      }
    }
    if (type == "dotsM1"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"], length.out = 5)
        points(x[-c(1,5)],rep(-0.5*sizeMachines,3), pch = 20,cex=fontScaling)
      }
    }
    if (type == "dotsM2"){
      for (k in 1:nrow(sub)){
        x <- seq(from=sub[k, "start"], to=sub[k, "end"], length.out = 5)
        points(x[-c(1,5)],rep(-1.5*sizeMachines,3), pch = 20,cex=fontScaling)
        #rect(sub[k, "start"], -sizeMachines, sub[k, "end"]+1,-sizeMachines,density = 20)
      }
    }
    #Buffer hat Platz von
    if(type == "buffer"){
      #sub <- sub[order(sub$start),]
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


