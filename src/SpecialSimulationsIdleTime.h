#ifndef SPECIALSIMULATIONS_IDLETIME
#define SPECIALSIMULATIONS_IDLETIME

#include <Rcpp.h>
#include <vector>
//#include "Machine.cpp"


//' @export
// [[Rcpp::export]]
int simulateSpecialIdleTime(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                            int maxBufferSize,
                            bool abortOnInvalidity);

//' @export
// [[Rcpp::export]]
int simulateSpecialIdleTimeTotalBuffer(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                                       int maxBufferSize,
                                       bool abortOnInvalidity);

#endif
