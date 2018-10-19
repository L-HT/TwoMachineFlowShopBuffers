MYWIDTH = 700
MYHEIGHT = 373
MYPOINTSIZE = 17
options(stringsAsFactors=F)
library(tikzDevice)

mathify <- function(s){
  return(paste("$",s,"$",sep=""))
}
appendSimResRow <- function(simRes, name, job, start, end){
  simRes[nrow(simRes) + 1,] <- list(name, job, start, end)
  return(simRes)
}

################### m2Dominate

scaleFactor <- 4
x <- c(11,7,7,10,9,6) * scaleFactor
B <- 25 * scaleFactor
m <- 2

jobData <- generateProblemInstance(6, c(1,10),sample(1:10,1),-1)

jobData$m1Time <- c(2,3,7,4,6,4)
jobData$m2Time <- c(7,7,7,7,7,7)
bufferSize <- 0
jobData$bufferUsage <- 1
perm1 <- paste("j",c(1:6), sep="")
perm2 <- paste("j",c(1:6), sep="")
simRes <- simulateFlowShop(jobData, perm1, perm2, bufferSize)

simRes <- simRes[simRes$type != "block", ]
rownames(simRes) <- 1:nrow(simRes)
simRes[2,c(3,4)] <- simRes[2,c(3,4)] + 4
simRes[4,c(3,4)] <- simRes[4,c(3,4)] + 3

simRes[c(5,6),c(3,4)] <- simRes[c(5,6),c(3,4)] + 4
simRes[c(10:12),c(3,4)] <- simRes[c(10:12),c(3,4)] + 4
simRes[c(5),c(3,4)] <- simRes[c(5),c(3,4)] + 1
simRes[c(6),c(3,4)] <- simRes[c(6),c(3,4)] + 3

simRes <- appendSimResRow(simRes, "dotsM1","", 23,28)
simRes <- appendSimResRow(simRes, "dotsM2","", 23,27)

#plotSchedule(simRes, jobData, 0, bai=1)


labelDf1 <- data.frame(pos=-1,text=mathify(c("a_1", "c", "c", "c")))
labelDf2 <- data.frame(pos=-1,text=mathify(c("c", "c", "c")))
labelDf <- list(labelDf1, labelDf2)
tickPos <- list(c(2,9,16,23), c(27,34,41))

simRes$job <- mathify(c("j_1", "j_2", "j_3", "j_4", "j_{n-1}", "j_n",
                "j_1", "j_2", "j_3", "j_{n-2}", "j_{n-1}", "j_n", "", "")
              )

scaleFactor <- 4
x <- c(11,7,7,10,9,6) * scaleFactor
B <- 25 * scaleFactor
m <- 2

tikz("figures/m2Dominate.tex", standAlone = TRUE, width=1.7*5, height=1*5)
plotScheduleTheory(simRes,jobData, bufferSize,bai = 1, tickPos, labelDf, fontScaling = 1.2, noBuffer = T)
dev.off()

setwd("./figures")
tools::texi2dvi('m2Dominate.tex',pdf=T, clean = T)
system("pdfcrop m2Dominate.pdf m2Dominate.pdf")
system(paste(getOption('pdfviewer'),'m2Dominate.pdf'))
setwd("..")

################### m1Dominate

scaleFactor <- 4
x <- c(11,7,7,10,9,6) * scaleFactor
B <- 25 * scaleFactor
m <- 2

jobData <- generateProblemInstance(6, c(1,10),sample(1:10,1),-1)

jobData$m1Time <- c(5,4,3,6,5,5)
jobData$m2Time <- 3#c(2,2,2,2,2,2)
bufferSize <- 0
jobData$bufferUsage <- 1
perm1 <- paste("j",c(1:6), sep="")
perm2 <- paste("j",c(1:6), sep="")
simRes <- simulateFlowShop(jobData, perm1, perm2, bufferSize)


simRes[c(5,6),c(3,4)] <- simRes[c(5,6),c(3,4)] + 4
simRes[c(10:12),c(3,4)] <- simRes[c(10:12),c(3,4)] + 4

simRes <- appendSimResRow(simRes, "dotsM1","", 18, 22)
simRes <- appendSimResRow(simRes, "dotsM2","", 16,21)

#plotSchedule(simRes, jobData, 0, bai=1)


labelDf1 <- data.frame(pos=-1,text=mathify(c("\\sum_j a_j")))
labelDf2 <- data.frame(pos=-1,text=mathify(c("c")))
labelDf <- list(labelDf1, labelDf2)
tickPos <- list(c(32), c(32))


simRes$job <- mathify(c("j_1", "j_2", "j_3", "j_4", "j_{n-1}", "j_n",
                        "j_1", "j_2", "j_3", "j_{n-2}", "j_{n-1}", "j_n", "", "")
)

scaleFactor <- 4
x <- c(11,7,7,10,9,6) * scaleFactor
B <- 25 * scaleFactor
m <- 2

tikz("figures/m1Dominate.tex", standAlone = TRUE, width=1.7*5, height=1*5)
plotScheduleTheory(simRes,jobData, bufferSize,bai = 1, tickPos, labelDf, fontScaling = 1.3, noBuffer = T)
dev.off()

setwd("./figures")
tools::texi2dvi('m1Dominate.tex',pdf=T, clean = T)
system("pdfcrop m1Dominate.pdf m1Dominate.pdf")
system(paste(getOption('pdfviewer'),'m1Dominate.pdf'))
setwd("..")

################### Beispiele zur Anwendung

# Grenze ist zwischen Summe der zwei größten
# und Summe der zwei kleinsten

jobData <- generateProblemInstance(5, c(30,80), 20, -1)
bufferType <- "totalBuffer"
maxBufferSize <- 20#sample(80:110,1)
maxBufferSize
jobData$m1Time <- c(5,10,9,11,16)
jobData$bufferUsage <- jobData$m1Time
jobData
enumerateAllSolutions(jobData, bufferType, maxBufferSize)
enumRes <- enumerateAllSolutions(jobData, bufferType, maxBufferSize)
apply(enumRes$bestPerms,2,function(x){permToM1Time(x,jobData)})
m1LookupInM1Table(c(36,35,34,40,31,49,73), jobData, enumRes )

permToM1Time(perm, jobData)

perm <- m1DominateConstructSolution(jobData, maxBufferSize)
permToM1Time(perm, jobData)
m1LookupInM1Table(perm, jobData, enumRes)

perm <- enumRes$bestPerms[1,]#
perm <- paste("j", c(2,4,3,1,6,5), sep="")

simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, maxBufferSize)
simulateFlowShopTotalBufferC(jobData, perm, perm, maxBufferSize)


plotSchedule(simRes, jobData, maxBufferSize, 1)

"
Init:
Sortiere a_j aufsteigend. Nimm kleinstes a_j und
suche dazu zwei Jobs links und rechts, die dazu passen.

Expand-Right: Füge rechts immer so lange große Jobs
hinzu, bis nichts mehr passt.

Expand-Left: Füge links immer so lange große Jobs hinzu,
bis nichts mehr passt.

Vielleicht nur expand-Left, damit links alles kürzer
wird?

Wenn expand fertig: Wieder init und mit neuem Kern
anfangen.



"




########################

# Ist NEH auch immer optimal?
omg <- TRUE
while (omg){
  jobData <- generateProblemInstance(8, c(3,12), 15, -1)
  bufferType <- "totalBuffer"
  maxBufferSize <- sample(13:20,1)

  nehSolution <- getNEHSolution(jobData, bufferType, maxBufferSize, 0)
  nehSolution <- paste("j",nehSolution,sep="")
  enumRes <- enumerateAllSolutions(jobData, bufferType, maxBufferSize)

  omg <- any(apply(enumRes$bestPerms, 1, function(x){identical(x, nehSolution)}))
  cat(".")
}

perm <- nehSolution
simRes <- simulateFlowShopTotalBuffer(jobData, perm, perm, maxBufferSize)
plotSchedule(simRes, jobData, maxBufferSize, 1)

# bisher ja...


#######################

# Test der Idee für m1DominateConstruction
temp <- TRUE
while(temp){
  jobData <- generateProblemInstance(6, c(20,100), 110, -1)
  bufferType <- "totalBuffer"
  maxBufferSize <- sample(100:120,1)

  enumRes <- enumerateAllSolutions(jobData, bufferType, maxBufferSize)
  perm <- m1DominateConstructSolution(jobData, maxBufferSize)
  temp <- m1LookupInM1Table(perm, jobData, enumRes )
  cat(".")
}

#################

# interessanter Fall mit 1 optimalen Lösung

# > jobData
# job m1Time m2Time bufferUsage
# 1  j1     53    110          53
# 2  j2     46    110          46
# 3  j3     40    110          40
# 4  j4     70    110          70
# 5  j5     56    110          56
# 6  j6     75    110          75

# G = 120, optMakespan = 712

#permToM1Time(perm, jobData)

