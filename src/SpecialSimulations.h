#ifndef SPECIALSIMULATIONS
#define SPECIALSIMULATIONS

#include <Rcpp.h>
#include <vector>
//#include "Machine.cpp"

/*
 * Simulationsfunktionen für den Sonderfall, dass Laufzeiten
 * auf der zweiten Maschine konstant sind. Denn da kann man bestimmte
 * Regeln verwenden, für die Auswahl der Jobs auf M2
 */

//' @export
// [[Rcpp::export]]
int simulateSpecial(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                      int maxBufferSize, bool abortOnInvalidity = true,
                      bool getMakespan = true);

//' @export
// [[Rcpp::export]]
int simulateSpecialTotalBuffer(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                      int maxBufferSize, bool abortOnInvalidity = true,
                      bool getMakespan = true);

#endif
