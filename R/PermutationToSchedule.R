# Permutation zu Startzeit
# Ergebnis:
#     -(N, 2)-Matrix mit Startzeit und (N, 2)-Matrix mit Endzeit für jeden Job
#     -oder (N,2)-Matrix mit Start- und Endzeit für M1 und das Gleiche für M2? eher das hier,
#      da dann Maschinen besser getrennt sind, bessere Analyse der Bufferbelegung
"
Job 1 läuft durch. Geht auf Maschine 2.
Job 2 läuft durch

wenn J2 auf M1 fertig:
  -wenn J1 noch auf M2 (durch konkretes Berechnen der Zeiten):

      -J2 in Buffer


Nee... Vielleicht ist ein simulationsbasierter Ansatz besser.
D.h. ich programmiere, wie Maschinen laufen und lasse eine schrittweise Simulation für
jeden Zeitschritt laufen.
Dabei wird quasi aufgezeichnet, wann wer was macht.

Dann bringt man nur eine Permutation der Jobs rein.

Wenn Überholen erlaubt: Permutation für M1 und M2, jedoch muss jeder Job bei M2 später
anfangen als bei M1.

Bei diesem Simulationsansatz ist vielleicht sogar RL möglich...??

"
permutationToSchedule <- function(permutation, jobData, bufferSize, bufferType){
  bufferUsed <- 0
  m1Schedule <- data.frame("job" = permutation, "start" = 0, "end" = 0)
  m2Schedule <- data.frame("job" = permutation, "start" = 0, "end" = 0)

  m1Schedule[1, "start"] <- 0
  m1Schedule[1, "end"] <- jobData[jobData$job == permutation[1], "m1Times"]
  m2Schedule[1, "start"] <- m1Schedule[1, "end"]
  m2Schedule[1, "end"] <- m1Schedule[1, "end"] + jobData[jobData$job == permutation[1], "m2Times"]

  # erst mal mit Zwischenspeicher, später mit Gesamtspeicher
  # wenn Buffer genug
  m1Schedule[2, "start"] <- m1Schedule[1, "end"]
  m1Schedule[2, "end"] <- m1Schedule[2, "start"] + jobData[jobData$job == permutation[2], "m1Times"]

  # wenn M1 zu früh fertig, bevor M2 fertig, muss gelagert werden
  if (m1Schedule[2, "end"] < m2Schedule[1, "end"]){
    bufferUsed <- bufferUsed + jobData[jobData$job == permutation[1], "bufferUsage"]
  }
  return(list(m1Schedule, m2Schedule))
}
