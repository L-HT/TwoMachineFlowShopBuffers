stackoverflow::Comment('

Aufgabe: Probleminstanzen generieren und speichern; zum 3. Mal...

	- m2Const, bufM1, Zwischenspeicher, constrained
 - m2Const, bufCount, Zwischenspeicher, 2
 - m2Const, bufM1, Gesamtspeicher, constrained
 - m2Const, bufCount, Gesamtspeicher, 2
 - m2Var, bufM1, Gesamtspeicher, constrained (Kononova, Lin)
 - m2Var, bufCount, Zwischenspeicher, constrained (andere Literatur)

 - m2Var, bufM1, Zwischenspeicher, heavily constrained
 - m2Var, bufM1, Zwischenspeicher, constrained
 - m2Var, bufM1, Zwischenspeicher, light constrained
 - m2Var, bufM1, Zwischenspeicher, KK
 - m2Var, bufM1, Gesamtspeicher, heavily constrained
 %- m2Var, bufM1, Gesamtspeicher, constrained %schon oben drin
 - m2Var, bufM1, Gesamtspeicher, light constrained
 - m2Var, bufM1, Gesamtspeicher, KK


-Namensformat:
{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l}-bufferSize-N.csv


')


C <- 1
m2Types <- c("m2var", "m2const")
bufTypes <- c("bufcount", "bufm1")
types <- c("intermediateBuffer", "totalBuffer")
constraints <- c("h", "c", "l", "kk")

#{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv
generateAndSaveInstance <- function(jobDataRaw, number){
  for (mt in m2Types){
    for (bt in bufTypes){
      for (ty in types){
        c <- "c"

        jobData <- jobDataRaw

        N <- nrow(jobData)

        if (mt == "m2const"){
          jobData$m2Time <- ceiling(median(jobData$m1Time))
        }

        if (bt == "bufcount"){
          jobData$bufferUsage <- 1
        }

        #############################
        # Ausschliessen von Fällen
        #############################

        if (mt == "m2var" && bt == "bufcount" && ty == "totalBuffer"){
          next
        }
        if (mt == "m2var" && bt == "bufm1" && ty == "intermediateBuffer" && N != 50){
          next
        }
        ##############################
        # buffer size
        ##############################

        bufferSize <- 0
        if (bt == "bufcount"){
          bufferSize <- 2
        } else {
          if (ty == "totalBuffer"){
            bufferSize <- max(jobData$bufferUsage) + ceiling(quantile(jobData$bufferUsage)["25%"])
          } else {
            bufferSize <- ceiling(median(jobData$bufferUsage))
          }
        }

        if (N != 50 || bt == "bufcount" || mt == "m2const"){
          if (ty == "totalBuffer"){
            fileName <- paste("./instances/f2total", N, mt, bt, c, bufferSize, number, sep="-")
          } else {
            fileName <- paste("./instances/f2inter", N, mt, bt, c, bufferSize, number, sep="-")
          }
          write.csv(jobData, fileName, quote=F, row.names=F)
        } else {

          #############################
          # Bufferconstraints ermitteln
          quantiles <- quantile(jobData$bufferUsage)
          for (c in constraints){
            # heavily constrained
            if (c == "h"){
              if (ty == "totalBuffer"){
                bufferSize <- max(jobData$bufferUsage)
              } else {
                bufferSize <- ceiling(quantiles["25%"])
              }
            }

            # constrained
            if (c == "c"){
              if (ty == "totalBuffer"){
                bufferSize <- ceiling(max(jobData$bufferUsage) + quantiles["25%"])
              } else {
                bufferSize <- ceiling(quantiles["50%"])
              }
            }

            # lightly constrained
            if (c == "l"){

              if (ty == "totalBuffer"){
                bufferSize <- ceiling(max(jobData$bufferUsage) + quantiles["50%"])
              } else {
                bufferSize <- ceiling(quantiles["75%"])
              }


            }

            # KK-Algorithmus
            if (c == "kk"){
              if (ty == "totalBuffer"){
                kkData <- calculateKKData(jobData)
              } else {
                kkData <- calculateKKDataIB(jobData)
              }
              bufferSize <- kkData$maxBuffer
            }
            if (ty == "totalBuffer"){
              fileName <- paste("./instances/f2total", N, mt, bt, c, bufferSize, number, sep="-")
            } else {
              fileName <- paste("./instances/f2inter", N, mt, bt, c, bufferSize, number, sep="-")
            }
            write.csv(jobData, fileName, quote=F, row.names=F)
          }
        }
      }
    }
  }
}

################################
################################

set.seed(20171123)

#erzeuge 1600 (alt: 1760) Instanzen
#{f2inter, f2total}-N-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv

numberOfInstances <- 3
jobNumbers <- c(50, 75, 100)

for (i in 1:numberOfInstances){

  for (jn in jobNumbers){
    jobDataRaw <- generateProblemInstance(jn, c(1,100), c(1,100), -1)

    generateAndSaveInstance(jobDataRaw, i)
    cat(".")
  }

}
