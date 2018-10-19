# Auswertung

stackoverflow::Comment('

-To-Do:
  -einzelne Testinstanzen in Skripte aufteilen
  -Nick Wiesecke eine E-Mail schreiben
  -Seminar vorbereiten (30 Minuten)
  -Evaluationsdatensatz:
    -12 Instanzen (2 für jeden Untertyp) bei 50 Jobs
    -auf jeder Instanz 3 Mal laufen
  -wie sollten die Params für die Verfahren gewählt werden?
  -lesen, wie SPPBO funktioniert
  -vielleicht schon erste Tests durchführen
  -Quade-Test: auf den Mittelwerten, und auf den Einzelwerten

-schätzen, wie groß die Gesamtlogs werden
  -1 Sek: 500 Messungen in HVNS -> 8 kB
  -20 Min -> 60*20 Sek =  1200 Sek -> 8*1200 = 9,6 MB
  -43200 Tests:

  -100 Jobs
  -5 Minuten -> 500 kB -> 1.67 kB pro Sekunde
  -20 Min: 2 MB

------------------
  -100 Jobs -> 150 Zeilen, 3 kB pro Sekunde
  -75 Jobs -> 250 Zeilen pro Sek,  5 kB pro Sekunde
  -50 Jobs -> 400 Zeilen pro Sek, 7 kB pro Sekunde
  -25 Jobs -> 600 Zeilen, 10 kB pro Sekunde
------------------
  -43200 Tests: 86.4 GB
  -geloggt bei 25/50/75/100 Jobs pro Sekunde?
  N25, N50, N75, N100

  -60*(N25*5 + N50*10 + N75*15 + N100*20) * 10800


-Zeitaufwand schätzen


-------------------------------------------



normalizedRuntime wird in Auswertung gemacht

-Problem: Wann terminieren die Algorithmen?
  -nach 5 Minuten?
  -nach Moslehi-Formel:
    -bei 2mFS: 10*N*M ms = maximal nach 10*100*2 = 2 Sekunden
    -bei BlockFS: 100*N*M ms = 20 Sekunden
  -hier 5/10/15/20


-Was muss ich loggen?

-RPD: Qualität am Ende des Verfahrens für jede Replikation

-Zeitmessung: absolute Zeit, jede Funktionsevaluation, normalisierte Laufzeit
  -Performance über Zeit (immer aktuellen Min-Wert speichern)
    -Performance-AT, Performance-NT, Performance-FE

  -Zeitgitter: immer nextGridPointTime speichern, wenn Zeit darüber, dann
    -logge aktuell besten Wert
    -für Runtime: alle 10 Sekunden vielleicht? -> auch bei getSolutionQuality gleich mitloggen
    -für FE: bei Aufruf von getSolutionQuality kann ich das gleich loggen

-Qualitätsschranken:
  -Johnson-Schranke und dann in Promille hochzählen?
    -also 100.1%, 100.2%, ... bis 150% -> 500 Schrankenpunkte
  -grobschrittig: schlecht auswertbar
  -kleinschrittig: viel Daten, aber besser das als zu wenig...?

  -für jedes Verfahren die Zeit messen bis zum Ende?


  -wenn Funktionsaufruf für getSolutionQuality:
    -wie lange nach Start wurde es aufgerufen?
    -gelieferter Wert

  -Progresscurve:
    -Performance über Zeit: es kann sein, dass ein Algo erst spät gut wird, quantitativ nicht
      so gut interpretierbar laut ErtMaybe (man kann nicht viel sagen, wenn zu einem
        Zeitpunkt ein Algo über dem anderen liegt, weil es oft nicht im voraus bekannt ist,
        ob das viel/schwer/bedeutsam ist);
      -wenn f transformiert wird, ist neue Auswertung nötig, da zum gleichen Zeitpunkt nun
       andere f-Werte vorliegen können

    -Zeit für bestimmte Performance: kompatibel über verschiedene Algos
      -interpretierbar: für das Erreichen der Schranke ist ein Algo besser als der andere
      -robuster bei Trafos auf f (d.h. Ergebnis ändert sich nicht)


  -ERT (Zeit-Fehlerschranke-Diagramm): wie berechne ich das?
    -siehe ErtMaybe: hat Vorteil, dass Skala zwischen 0 und 1 (Prozent), gut interpretierbar, einfach
    -nimm Fehlerschranke, summiere für jeden Testlauf (auch die nicht erfolgreichen)
      die vergangene Zeit, bis sie erreicht
      wurde und teile durch die Anzahl der Läufe, die das geschafft haben
    -es kann nicht sein, dass in einem Testlauf es geschafft wird, im nächsten mit dem
      gleichen Algo aber nicht;
      man muss theoretisch so lange warten, bis es auch der langsamere schafft
      -wirklich unendlich wird es nur, wenn es unmöglich ist?

  -ECDF: welche Qualitätsgrenze nehme ich, die zu erreichen ist?
    -für Arbeit: johnson geht nicht, da nicht erreichbar; johnson*p geht auch nicht, da stark
      von Buffergröße abhängig; konkret von Buffergröße modellieren ist schwer
      -deswegen Idee: man nimmt definitive, untere Grenze (Johnson) und definitive obere Grenze
        (NEH), die immer deterministisch bestimmt ist und legt Schranke anhand des Abstandes
        fest
    -> johnson+(neh-johnson)*0.01? in neh steckt dann schon die Buffergröße drin

-obere Grenze sei NEH, untere sei Johnson
-vlt. johnson+(neh-johnson)*0.01?

-Darstellung in Log-Skala, damit Verhalten für geringe t sichtbar (meist springt das schnell hoch)



Fragen:
  - wie skaliert das Verfahren bei steigendem Speicherbedarf? Wer kommt da besser zurecht als andere mit weniger/mehr Speicher? separat für die zwei Speichertypen
  - durchschnittliche Abweichung vom KK-Optimum
  - Laufzeit: Wie skaliert sie, wenn man hochgeht mit der Job-Anzahl? (wie unterschiedliche Typen betrachten?) Auf gleichem Computer (absolute Zeit) oder eher über Anzahl der Abfragen
  - Gesamtspeicher / Zwischenspeicher: Macht es da einen Unterschied?
  - Auswertung nach den anderen Arbeiten
  - wie sehr weichen die von den 3 Verfahren generierten Elite-Lösungen ab (Abstandsmaß dafür nötig)
  - bufCount: Wie gut mit anderen Algos, die das auch benutzen
  - m1Time: andere Literatur (Kononova)
  - willkürlicher Buffer: macht das die Verfahren noch schwerer?
  - m2Const: wie wirkt sich das aus?
  - initialisiere Lösung mit Johnson-Algorithmus. Ist das gut?
  - Kombination mit lokaler Suche gut? (aufwändig, im Paper keine Details dazu): ABC ist schon mit lokaler Suche drin. VNS ist mit SA kombiniert in LS-Manier. Wie sieht SPPBO aus?
  - steigt Makespan linear mit Jobanzahl? ich glaube ja (obere Schranke)

jedes Verfahren läuft auf Instanz mehrmals und die geloggten Daten sind auszuwerten
  -> nicht zu viele Instanzen

Verfahren:
  - Tukey-Test (in R schon implementiert als Funktion TukeyHSD) nach Moslehi
  - RPD: dazu muss man beste Makespan immer speichern aus jedem Lauf
')


# kkTest <- generateKKInstance(50, c(1,100), c(1,100), c(1,100))
# jBound <- kkTest$optimalMakespan
#
# p <- 0.01
# totalBufSize <- ( kkTest$maxBuffer - max(kkTest$jobData$bufferUsage) ) * p + max(kkTest$jobData$bufferUsage)
# totalBufSize <- ceiling(totalBufSize)
#
# neh <- getNEHSolution(kkTest$jobData, "totalBuffer", totalBufSize, 0)
# neh <- paste("j", neh, sep="")
# nehTime <- simulateFlowShopTotalBufferC(kkTest$jobData, neh, neh, totalBufSize)
# nehTime/jBound
#
# jBound + (nehTime-jBound)*0.01
