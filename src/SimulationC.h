#ifndef SIMULATIONC
#define SIMULATIONC

#include <Rcpp.h>
#include <vector>

//' @export
// [[Rcpp::export]]
int simulateFlowShopC(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                      std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

//' @export
// [[Rcpp::export]]
int simulateFlowShopTotalBufferC(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                                 std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

//' @export
// [[Rcpp::export]]
int simulateFlowShopC_TFT(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                      std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

//' @export
// [[Rcpp::export]]
int simulateFlowShopTotalBufferC_TFT(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                                 std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

//' @export
// [[Rcpp::export]]
int simulateFlowShopC_DueTime(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                          std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

//' @export
// [[Rcpp::export]]
int simulateFlowShopTotalBufferC_DueTime(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1,
                                     std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity = true);

#endif
