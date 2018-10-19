# Algorithmen aufrufen

fileName <- "f2inter-50-m2const-bufcount-c-2-2"
filePath <- paste("./instances/", fileName, sep="")

jobData <- read.csv(filePath)

##############################

runNumber <- 1

# DABC+IG
startDABC(jobData, fileName, runNumber)

# VNS+SA
startHVNS(jobData, fileName, runNumber)

# SPPBO
init_rJava()
startSPPBO(jobData, fileName, runNumber, "ccc")


