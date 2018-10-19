# Algorithmen aufrufen

files <- list.files("./instances/")
if (".gitignore" %in% files){
  files <- files[files != ".gitignore"]
}

startTime <- Sys.time()
for (fileName in files){
  fullPath <- paste("./instances/", fileName, sep="")
  jobData <- read.csv(fullPath, sep=",", header=T, stringsAsFactors=F)
  for (runNumber in 3:3){ # bis 7
    fileNameToCheck <- paste("./output/", fileName, "-","hvns","-",runNumber, sep="")

    if (file.exists(fileNameToCheck)){
      print(paste(fileNameToCheck, "ist schon da"))
    } else {
      startHVNS(jobData, fileName, runNumber)
    }
  }
}
endTime <- Sys.time()
print(endTime - startTime)
# VNS+SA
2668 f2total-50-m2var-bufm1-c-130-2-hvns-1
2995 f2inter-50-m2const-bufm1-c-48-3-hvns-1

##############################



# DABC+IG
startDABC(jobData, fileName, runNumber)


# SPPBO
init_rJava()
startSPPBO(jobData, fileName, runNumber)


