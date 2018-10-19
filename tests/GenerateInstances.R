stackoverflow::Comment('

Aufgabe: Probleminstanzen generieren und speichern

-Frage:
  -wie sehr wirkt sich Platzbeschränkung aus? -> constrained-ness

  -Buffer-Arten
    -zählender Buffer: Vergleichbar mit anderen Algorithmen, die auch zählende Buffer verwenden
    -m1Time-Buffer: andere Literatur
    -willkürlicher Buffer: besonders schwer

  -2mFS so in verschiedenen Varianten betrachten

  -Leistung über Zeit -> nochmal Benchmarking TSP anschauen

-zwei Speicherarten, da auch bei Geser so betrachtet

-zu speichern in ./instances/

-Problemtypen:
  -Zwischenspeicher: Buffergröße beliebig
    -nutze Median, damit die Teil der Jobs passt und beim Rest Blocking ausgelöst wird
    -very heavily constrained: 0.25-Quantil (75% der Jobs lösen Blocking aus)
    -heavily constrained: 0.5-Quantil
    -constrained: 0.75%-Quantil
    -lightly constrained: maxBuf

  -Gesamtspeicher: Buffergröße mindestens M=max(jobData$bufferUsage)
    -setze Buffergröße auf M (heaviest)
    -constrained: M + 0.25-Quantil
    -lightly constrained: M + 0.5-Quantil


  -M2 konst/nicht konst
  -Buffer willkürlich, 1, m1Time
  -Zwischenspeicher/Gesamtspeicher
-> 2*3*2 = 12 Typen

-für jedes 10 Instanzen -> 120 Instanzen

-Namensformat:
{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l}-bufferSize-N.csv


Beispiel:
f2inter-m2const-bufvar-bufm1-30-1.csv
f2inter-m2const-bufvar-bufm1-30-2.csv
f2total-m2var-bufm2-100-3.csv

-Kononova-Instanzen:
  -m2konst/nichtkonst
  -Zwischenspeicher/Gesamtspeicher
  -von jedem 5 Instanzen

-Jobanzahl: 25, 50, 75, 100 -> Skalierbarkeit testen; von jedem 5 Stück -> 20

Insgesamt also:

4*5*12 + 4*5 = 260 Instanzen


###################### noch mal neu überlegen...

Ranking am Ende nach Weise
-aber auch Methodik von anderen beachten

-weniger Instanzen: einmal völlig zufällig generieren und darauf dann die
Einschränkungen anwenden?

-wie sehr weichen die von den 3 Verfahren generierten Elite-Lösungen ab?
  -Abstandsmaß?
')

#{f2inter, f2total}-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv

generateAndSaveInstance <- function(N, m1Times, m2Times, bufferUsage, type="intermediateBuffer",
                                    m2Type = "m2var", bufType = "bufvar",  number){

  jobData <- generateProblemInstance(N, m1Times, m2Times, bufferUsage)

  if (type == "intermediateBuffer"){
    myQuantiles <- quantile(jobData$bufferUsage)
    # very heavily constrained
    fileName <- paste("./instances/f2inter", N, m2Type, bufType, "vh", ceiling(myQuantiles["25%"]), number, sep="-")
    write.csv(jobData, fileName, quote=F, row.names=F)

    # heavily constrained
    fileName <- paste("./instances/f2inter", N, m2Type, bufType, "h", ceiling(myQuantiles["50%"]), number, sep="-")
    write.csv(jobData, fileName, quote=F, row.names=F)

    # constrained
    fileName <- paste("./instances/f2inter", N, m2Type, bufType, "c", ceiling(myQuantiles["75%"]), number, sep="-")
    write.csv(jobData, fileName, quote=F, row.names=F)

    # lightly constrained
    fileName <- paste("./instances/f2inter", N, m2Type, bufType, "l", max(jobData$bufferUsage), number, sep="-")
    write.csv(jobData, fileName, quote=F, row.names=F)
  }

  if (type == "totalBuffer"){
    myQuantiles <- quantile(jobData$bufferUsage)
    # lightly constrained
    fileName <- paste("./instances/f2total", N, m2Type, bufType, "h", max(jobData$bufferUsage), number, sep="-")
    write.csv(jobData, fileName, quote=F, row.names=F)

    # heavily constrained
    fileName <- paste("./instances/f2total", N, m2Type, bufType, "c", max(jobData$bufferUsage)+ceiling(myQuantiles["25%"]), number, sep="-")
    write.csv(jobData, fileName, quote=F, row.names=F)

    # constrained
    fileName <- paste("./instances/f2total", N, m2Type, bufType, "l", max(jobData$bufferUsage)+ceiling(myQuantiles["50%"]), number, sep="-")
    write.csv(jobData, fileName, quote=F, row.names=F)
  }
}

stackoverflow::Comment('
  Hier die KK-Instanzen:
  -Kononova-Instanzen:
    -m2konst/nichtkonst
    -Zwischenspeicher/Gesamtspeicher
    -von jedem 5 Instanzen

     -Jobanzahl: 25, 50, 75, 100 -> Skalierbarkeit testen; von jedem 5 Stück -> 20
')

generateAndSaveKKInstance <- function(N, m1Times, m2Times, type="intermediateBuffer",
                                    m2Type = "m2var", number){

  if (type == "intermediateBuffer"){
    kkList <- generateKKInstanceIB(N, m1Times, m2Times)
    fileName <- paste("./instances/f2inter", N, m2Type, bufType, "kk", kkList$maxBuffer, number, sep="-")
    write.csv(kkList$jobData, fileName, quote=F, row.names=F)
  }

  if (type == "totalBuffer"){
    kkList <- generateKKInstance(N, m1Times, m2Times)
    fileName <- paste("./instances/f2total", N, m2Type, bufType, "kk", kkList$maxBuffer, number, sep="-")
    write.csv(kkList$jobData, fileName, quote=F, row.names=F)
  }

  # Optimum speichern
  optFileName <- paste(fileName, "Opt", sep="-")
  tempString <- paste(c(kkList$oneOptimalSolution, as.character(kkList$optimalMakespan)), collapse=",")
  write(tempString, optFileName)
}

################################
################################

set.seed(20171123)

#erzeuge 840 Instanzen:
#2*3*4*4*5 + 2*3*4*3*5
#{f2inter, f2total}-N-{m2const,m2var}-{bufvar, bufcount, bufm1}-{vh, h, c, l, kk}-bufferSize-N.csv

C <- 1
m2Types <- c("m2var", "m2const")
bufTypes <- c("bufvar", "bufcount", "bufm1")
types <- c("intermediateBuffer", "totalBuffer")
jobNumbers <- c(25, 50, 75, 100)

for (i in 1:5){
  for (mt in m2Types){
    for (bt in bufTypes){
      for (ty in types){
        for (jn in jobNumbers){

          if (mt == "m2const"){
            m2Times <- C
          } else {
            m2Times <- c(1,100)
          }

          if (bt == "bufvar"){
            bufferUsage <- c(1,100)
          }
          if (bt == "bufcount"){
            bufferUsage <- 1
          }
          if (bt == "bufm1"){
            bufferUsage <- -1
          }
          generateAndSaveInstance(jn, c(1,100), m2Times, bufferUsage,
                                  type = ty, m2Type = mt, bufType = bt,
                                  number = i)
        }
      }
    }
  }
}

#######################
######################

# KK-Instanzen
stackoverflow::Comment('
  Hier die KK-Instanzen:
   -m2konst/nichtkonst
   -Zwischenspeicher/Gesamtspeicher
   -von jedem 5 Instanzen

   -Jobanzahl: 25, 50, 75, 100 -> Skalierbarkeit testen; von jedem 5 Stück -> 20

To-Do:
  -KK-Instanz-Generierung sollte die C-Simulation verwenden
')

C <- 1
m2Types <- c("m2var", "m2const")
types <- c("intermediateBuffer", "totalBuffer")
jobNumbers <- c(25, 50, 75, 100)

for (i in 1:5){
  for (mt in m2Types){
    for (t in types){
      for (jn in jobNumbers){

        if (mt == "m2const"){
          m2Times <- C
        } else {
          m2Times <- c(1,100)
        }

        generateAndSaveKKInstance(jn, c(1,100), m2Times,
                                type = t, m2Type = mt,
                                number = i)
      }
    }
  }
}
