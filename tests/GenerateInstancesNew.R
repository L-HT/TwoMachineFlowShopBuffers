stackoverflow::Comment('

Aufgabe: Probleminstanzen generieren und speichern

25, 50, 75, 50 -> für jedes 5 Instanzen

- 4 Gesertypen: m2Const
  -Gesamtspeicher: bufCount
  -Gesamtspeicher: bufM1
  -Zwischenspeicher: bufCount
  -Zwischenspeicher: bufM1

- zu einer gegebenen Instanz (vollkommen zufällig) kann ich Folgendes machen:
  -tu nichts (bufFree, m2Var)
  -bufCount, m2var (normale Literatur zu BufFS)
  -bufCount, m2Const (Gesertyp 1 und 3)
  -bufM1, m2Const (Gesertyp 2 und 4)
  -bufM1, m2var (Lin und Kononova)

  -nicht interessant:
    -m2Const, bufFree

  -alles dann light, con, heavy, KK; dann Zwischen/Gesamtspeicher
  -also: 5 Maßnahmen * 4 Constraint-Types * 2 Speicherarten = 40 neue Instanzen

  -4 Problemgrößen, jeweils 10 Instanzen -> 40
  -40*40 sind dann 1600 Tests


-Namensformat:
{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l}-bufferSize-N.csv


')


C <- 1
m2Types <- c("m2var", "m2const")
bufTypes <- c("bufvar", "bufcount", "bufm1")
types <- c("intermediateBuffer", "totalBuffer")
constraints <- c("h", "c", "l", "kk")

#{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv
generateAndSaveInstance <- function(jobDataRaw, number){
  for (mt in m2Types){
    for (bt in bufTypes){
      for (ty in types){
          for (c in constraints){

          jobData <- jobDataRaw

          N <- nrow(jobData)

          if (mt == "m2const"){
            jobData$m2Time <- ceiling(median(jobData$m1Time))
            if (bt == "bufvar"){
              next
            }
          }

          if (bt == "bufcount"){
            jobData$bufferUsage <- 1
          }
          if (bt == "bufm1"){
            jobData$bufferUsage <- jobData$m1Time
          }

          ##############################
          # buffer size
          ##############################

          bufferSize <- 0
          quantiles <- quantile(jobData$bufferUsage)

          # heavily constrained
          if (c == "h"){
            if (bt == "bufcount"){
              bufferSize <- 1
            } else {
              if (ty == "totalBuffer"){
                bufferSize <- max(jobData$bufferUsage)
              } else {
                bufferSize <- ceiling(quantiles["25%"])
              }

            }
          }

          # constrained
          if (c == "c"){
            if (bt == "bufcount"){
              bufferSize <- 2
            } else {
              if (ty == "totalBuffer"){
                bufferSize <- ceiling(max(jobData$bufferUsage) + quantiles["25%"])
              } else {
                bufferSize <- ceiling(quantiles["50%"])
              }

            }
          }

          # lightly constrained
          if (c == "l"){
            if (bt == "bufcount"){
              bufferSize <- 3
            } else {
              if (ty == "totalBuffer"){
                bufferSize <- ceiling(max(jobData$bufferUsage) + quantiles["50%"])
              } else {
                bufferSize <- ceiling(quantiles["75%"])
              }

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

################################
################################

set.seed(20171123)

#erzeuge 1600 (alt: 1760) Instanzen
#{f2inter, f2total}-N-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv

C <- 1
numberOfInstances <- 10
m2Types <- c("m2var", "m2const")
bufTypes <- c("bufvar", "bufcount", "bufm1")
types <- c("intermediateBuffer", "totalBuffer")
jobNumbers <- c(25, 50, 75, 100)

for (i in 1:numberOfInstances){

  for (jn in jobNumbers){
    jobDataRaw <- generateProblemInstance(jn, c(1,100), c(1,100), c(1,100))

    generateAndSaveInstance(jobDataRaw, i)
    cat(".")
  }

}
