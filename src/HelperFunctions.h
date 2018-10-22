#ifndef HELPERFUNCTIONS_H
#define HELPERFUNCTIONS_H

#include <Rcpp.h>
#include <vector>
#include <list>
#include <string>
#include <algorithm>
#include <chrono>
#include <random>

#define MYPACKAGE "package:TwoMachineFlowShopBuffers"
#define GETQUALITY "getMakespanOfSimulation"
#define SOLVE_LINEAR_EQUATION_SYSTEM "rQRSolve"
#define SOLVE_LINEAR_EQUATION_SYSTEM_CHOL "rCholeskySolve"
#define LOGINTERVAL 10

#define OP_INSERT 1
#define OP_EDGEINSERT 2
#define OP_SWAP 3
// #define DEBUG_ENABLED false

std::vector<std::string> vectorToPermutation(std::vector<int> x);

int getAndLogSolutionQuality(Rcpp::DataFrame jobData,
                             const std::vector<int>& permutationM1,
                             const std::vector<int>& permutationM2,
                             int maxBufferSize,
                             std::string bufferType,
                             std::string targetCriterion,
                             std::ofstream& logFile,
                             std::chrono::high_resolution_clock::time_point startTime,
                             long& numberOfEvaluation,
                             int& iterationNumber,
                             int& currentBest,
                             std::vector<int>& currentBestSolution,
                             std::vector<int>& currentBestSolutionM2,
                             bool forceLogging = false,
                             bool abortOnInvalidity = true,
                             bool m2Special = false);


int getSolutionQuality(Rcpp::DataFrame jobData,
                       const std::vector<int>& permutationM1,
                       const std::vector<int>& permutationM2,
                       int maxBufferSize,
                       std::string bufferType,
                       std::string targetCriterion,
                       bool abortOnInvalidity = true,
                       bool m2Special = false);

template <typename T>
void printVector (const std::vector<T>& v) {
  Rcpp::Rcout << ">>";
  for (typename std::vector<T>::const_iterator it = v.begin(); it != v.end(); it++){
    Rcpp::Rcout << *it << ",";
  }
  Rcpp::Rcout << "<<" << std::endl;
}

template <typename T>
void printList (const std::list<T>& v) {
  Rcpp::Rcout << "]]";
  for (typename std::list<T>::const_iterator it = v.begin(); it != v.end(); it++){
    Rcpp::Rcout << *it << ",";
  }
  Rcpp::Rcout << "[[" << std::endl;
}
template <typename T, typename C>
bool contains (const C& v, T value) {
  return (std::find(v.begin(), v.end(), value) != v.end());
}

void insertOperator(std::vector<int>& permutation, int takePosition, int insertPosition);
void swapOperator(std::vector<int>& permutation, int position1, int position2);
void edgeInsertOperator(std::vector<int>& permutation, int takePosition, int insertPosition);
int replaceOperator(std::vector<int>& permutation, int pos, std::size_t numberOfJobs, int forbiddenJob = -1);

void initRandom();
int getRandomNumber(int min, int max);
std::vector<int> getRandomPermutation(std::size_t numberOfJobs, std::mt19937& rng);

void waitForInput(std::string msg, bool enabled);

int getAndLogBufferUsage(Rcpp::DataFrame jobData,
                         const std::vector<int>& permutationM1,
                         const std::vector<int>& permutationM2,
                         int maxBufferSize,
                         std::string bufferType,
                         std::ofstream& logFile,
                         std::chrono::high_resolution_clock::time_point startTime,
                         long& numberOfEvaluation,
                         int& iterationNumber,
                         int& currentBest,
                         bool abortOnInvalidity,
                         bool m2Special = false);

#endif
