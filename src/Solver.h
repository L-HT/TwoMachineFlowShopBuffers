#ifndef SOLVER_H
#define SOLVER_H

#include <Rcpp.h>
#include <fstream>
#include <chrono>

struct Solver{

  const Rcpp::DataFrame jobData_;
  const std::string logFileName_;
  const std::string algorithmName_;
  const unsigned int runNumber_;

  std::ofstream logFile_;

  Rcpp::Environment rEnvironment_;
  Rcpp::Function getSolutionQuality_;

  std::string fileSuffix_;

  const int maxBufferSize_;
  const std::string bufferType_;
  std::string targetCriterion_;
  const std::size_t numberOfJobs_;

  std::vector<int> bestSolution_;
  std::vector<int> bestSolutionM2_;

  std::chrono::high_resolution_clock::time_point startTime_;
  double timeLimit_; // in Sekunden
  long numberOfEvaluation_;
  long maxEvaluation_;
  int bestSolutionQuality_;

  
  //int bestMeasuredMakespan_;

  int iterationNumber_;


  Solver(Rcpp::DataFrame jobData,
         std::string logFileName,
         unsigned int runNumber,
         Rcpp::Environment rEnvironment,
         int maxBufferSize,
         std::string bufferType,
         std::string targetCriterion,
         std::string algorithmName,
         std::string fileSuffix);

  virtual int evaluateSolution(Rcpp::DataFrame jobData,
                   const std::vector<int>& permutationM1,
                   const std::vector<int>& permutationM2,
                   int maxBufferSize,
                   std::string bufferType,
                   bool forceLogging = false,
                   bool abortOnInvalidity = true,
                   bool m2Special = false);

  int getCurrentBufferUsage(Rcpp::DataFrame jobData,
                    const std::vector<int>& permutationM1,
                    const std::vector<int>& permutationM2,
                    int maxBufferSize,
                    std::string bufferType,
                    bool abortOnInvalidity,
                    bool m2Special = false);

  std::vector<int> getNEHSolution(int type = 0);
  long getNEHEvaluations();

  long getMaxEvaluation(std::size_t numberOfJobs);
  virtual bool terminationCriterionSatisfied();
  virtual void writeBestSolution();
  ~Solver();
};

#endif
