jobData <- generateProblemInstance(25, c(1,100), c(1,100), -1)

startDABC(jobData, "sorceresss", 1, "25")

getNEHSolution(jobData, "intermediateBuffer", maxBufferSize = 3, 0)


#################################

perm <- readLines("./solutions/sorceresss-dabc-1-50c")
perm <- strsplit(perm, split=",")[[1]]

simulateFlowShopC(jobData, perm, perm, 10)

test <- read.csv("./output/sorceresss-dabc-1-50c", header=F)

#####################


# Frage: ist es bei genau zwei Jobs immer besser, zu erst den
# mit kürzerer Zeit auf A zu starten?
# for (k in 1:100){
#   bla <- generateProblemInstance(2, c(1,100), c(1,100), c(1,100))
#   minIndex <- which.min(bla$m1Time)
#   mySequence <- c(bla$job[minIndex], bla$job[3-minIndex])
#   alternative <- c(bla$job[3-minIndex], bla$job[minIndex])
#   maxBufferSize <- sample(1:100,1)
#
#   simRes1 <- simulateFlowShop(bla, mySequence, mySequence, maxBufferSize)
#   simRes2 <- simulateFlowShop(bla, alternative, alternative, maxBufferSize)
#
#   value1 <- getMakespan(simRes1)
#   value2 <- getMakespan(simRes2)
#   if (value1 > value2){
#     print("eeeeh?")
#     print(bla)
#     print(mySequence)
#     print(alternative)
#     print(maxBufferSize)
#     print(simRes1)
#     print(simRes2)
#     stop("End")
#   }
# }
