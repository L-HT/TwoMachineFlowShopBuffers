stackoverflow::Comment('
-Fall: t2 konstant

   -Job-Größen: 50, 75, 100

   Fälle:
   -Zwischenspeicher, sj=t1
   -Zwischenspeicher, sj=1
   -Gesamtspeicher, sj=t1
   -Gesamtspeicher, sj=1

   -Buffer-Hardness: so schwer wie möglich:
   -zählend: Zwischen 1, Gesamt 3,
   -Zwischen: 1. Quartil
   -Gesamt: max sj

   -Konstante: Median und zwei Quartile

   4 Typen * 3 Konstanten * 3 Roh-Instanzen: 36 Instanzen pro Größe

   36*(5+10+15)*10=10800 Min: Gesamtzeit für einen Algo

   Mit den Algos: HVNS, SPPBO-MMD, SPPBO, SPPBO-BUM

   -> 43200 Min oder 30 Tage

-normalverteilte (gerundete) Laufzeiten mit mu=50, sigma=10.

                       ')


C <- 1
m2Types <- c("m2const")
bufTypes <- c("bufcount", "bufm1")
types <- c("intermediateBuffer", "totalBuffer")
m2Constants<- c("small", "mid", "big")

#{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv
generateAndSaveInstance <- function(jobDataRaw, number){
  for (mt in m2Types){
    for (bt in bufTypes){
      for (ty in types){
        for (c in m2Constants){

          jobData <- jobDataRaw

          N <- nrow(jobData)

          quantiles <- quantile(jobData$m1Time)
          if (c == "small"){
            jobData$m2Time <- ceiling(quantiles["25%"])
          }
          if (c == "mid"){
            jobData$m2Time <- ceiling(quantiles["50%"])
          }
          if (c == "big"){
            jobData$m2Time <- ceiling(quantiles["75%"])
          }

          ##############################
          # buffer size
          ##############################


          if (bt == "bufcount"){
            jobData$bufferUsage <- 1

            if (ty == "totalBuffer"){
              bufferSize <- 3
            } else {
              bufferSize <- 1
            }
          } else {
            if (ty == "totalBuffer"){
              bufferSize <- max(jobData$bufferUsage) + ceiling(quantiles["25%"])
            } else {
              bufferSize <- ceiling(quantiles["25%"])
            }
          }



          if (ty == "totalBuffer"){
            fileName <- paste("./instances/f2total", N, mt, bt, c, bufferSize, number, sep="-")
            # fileName <- paste("./valInstances/f2total", N, mt, bt, c, bufferSize, number, sep="-")

          } else {
            fileName <- paste("./instances/f2inter", N, mt, bt, c, bufferSize, number, sep="-")
            # fileName <- paste("./valInstances/f2inter", N, mt, bt, c, bufferSize, number, sep="-")
          }
          write.csv(jobData, fileName, quote=F, row.names=F)
        }
      }
    }
  }
}

################################
################################

set.seed(20180430)

#erzeuge 1600 (alt: 1760) Instanzen
#{f2inter, f2total}-N-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv

numberOfInstances <- 3
jobNumbers <- c(50,100,150)

for (i in 1:numberOfInstances){

  for (jn in jobNumbers){
    jobDataRaw <- generateProblemInstance(jn, c(1,100), c(1,100), -1)
      #generateNormalProblemInstance(jn, 50, 20)

    generateAndSaveInstance(jobDataRaw, i)
    cat(".")
  }

}


####################
####################
####################

C <- 1
m2Types <- c("m2const")
bufTypes <- c("bufcount", "bufm1")
types <- c("intermediateBuffer", "totalBuffer")
m2Constants<- c("small", "mid", "big")

#{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv
generateAndSaveInstance <- function(jobDataRaw, number){
  for (mt in m2Types){
    for (bt in bufTypes){
      for (ty in types){
        for (c in m2Constants){

          jobData <- jobDataRaw

          N <- nrow(jobData)

          quantiles <- quantile(jobData$m1Time)
          if (c == "small"){
            jobData$m2Time <- ceiling(quantiles["25%"])
          }
          if (c == "mid"){
            jobData$m2Time <- ceiling(quantiles["50%"])
          }
          if (c == "big"){
            jobData$m2Time <- ceiling(quantiles["75%"])
          }

          ##############################
          # buffer size
          ##############################


          if (bt == "bufcount"){
            jobData$bufferUsage <- 1

            if (ty == "totalBuffer"){
              bufferSize <- 3
            } else {
              bufferSize <- 1
            }
          } else {
            if (ty == "totalBuffer"){
              bufferSize <- max(jobData$bufferUsage) + ceiling(quantiles["25%"])
            } else {
              bufferSize <- ceiling(quantiles["25%"])
            }
          }



          if (ty == "totalBuffer"){
            # fileName <- paste("./instances/f2total", N, mt, bt, c, bufferSize, number, sep="-")
            fileName <- paste("./valInstances/f2total", N, mt, bt, c, bufferSize, number, sep="-")

          } else {
            # fileName <- paste("./instances/f2inter", N, mt, bt, c, bufferSize, number, sep="-")
            fileName <- paste("./valInstances/f2inter", N, mt, bt, c, bufferSize, number, sep="-")
          }
          write.csv(jobData, fileName, quote=F, row.names=F)
        }
      }
    }
  }
}

set.seed(20180501)

#erzeuge 1600 (alt: 1760) Instanzen
#{f2inter, f2total}-N-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv

numberOfInstances <- 2
jobNumbers <- c(100)

for (i in 1:numberOfInstances){

  for (jn in jobNumbers){
    jobDataRaw <- generateProblemInstance(jn, c(1,100), c(1,100), -1)
      #generateNormalProblemInstance(jn, 50, 20)

    generateAndSaveInstance(jobDataRaw, i)
    cat(".")
  }

}
