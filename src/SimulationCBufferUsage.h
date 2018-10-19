#ifndef SIMULATIONC_BUFFERUSAGE
#define SIMULATIONC_BUFFERUSAGE

#include <Rcpp.h>
#include <vector>

//' @export
// [[Rcpp::export]]
int simulateBufferUsage(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                      std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

//' @export
// [[Rcpp::export]]
int simulateBufferUsageTotalBuffer(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                                 std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

#endif
