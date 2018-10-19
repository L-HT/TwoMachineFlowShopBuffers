
#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <sstream>
//#include <ctime>
#include <limits>
#include <chrono>

#include "HelperFunctions.h"
#include "Solver.h"

Solver::Solver(Rcpp::DataFrame jobData,
         std::string logFileName,
         unsigned int runNumber,
         Rcpp::Environment rEnvironment,
         int maxBufferSize,
         std::string bufferType,
         std::string targetCriterion,
         std::string algorithmName,
         std::string fileSuffix)
    : jobData_(jobData), logFileName_(logFileName),
      algorithmName_(algorithmName),
      runNumber_(runNumber),
      rEnvironment_(rEnvironment),
      getSolutionQuality_(rEnvironment[GETQUALITY]),
      fileSuffix_(fileSuffix),
      maxBufferSize_(maxBufferSize), bufferType_(bufferType),
      targetCriterion_(targetCriterion),
      numberOfJobs_(jobData.nrows()), numberOfEvaluation_(1),
      bestSolutionQuality_(std::numeric_limits<int>::max()), iterationNumber_(1){

    std::stringstream ss;
    ss << "./output/" << logFileName_ << "-" << algorithmName_ << "-" << runNumber_;
    if (fileSuffix != ""){
      ss << "-" << fileSuffix;
    }

    //Logfile vorbereiten
    logFile_.open (ss.str(), std::ios_base::trunc); //app für append
    //logFile_ << "n,time,best" << std::endl;


    startTime_ = std::chrono::high_resolution_clock::now();

// # SPPBO 100: 34000
// # SPPBO 75: 130000
// # SPPBO 50: 90000
//
// # HVNS 100: 260000
// # HVNS 75: 220000
// # HVNS 50: 160000
//
// # DABC 100: 40000
// # DABC 75: 140000
// # DABC 50: 140000
//
// # API 100: 50000 (?)
// # API 75: 215000
// # API 50: 160000
//
// # AVI 100: 75000
// # AVI 75: 150000
// # AVI 50: 160000
//
    switch(numberOfJobs_){
    case(10):
      timeLimit_ = 10;
      Rcpp::Rcout << "10 Jobs (Test)" << std::endl;
      break;
    case(25):
      timeLimit_ = 15;
      Rcpp::Rcout << "25 Jobs" << std::endl;
      break;
    case(50):
      timeLimit_ = 60;//300;
      Rcpp::Rcout << "50 Jobs" << std::endl;
      break;
    case(75):
      timeLimit_ = 45;
      Rcpp::Rcout << "75 Jobs" << std::endl;
      break;
    case(100):
      timeLimit_ = 60; //600
      Rcpp::Rcout << "100 Jobs" << std::endl;
      break;
    case(150):
      timeLimit_ = 90; //900
      Rcpp::Rcout << "150 Jobs" << std::endl;
      break;
    default:
      timeLimit_ = 30;
      Rcpp::Rcout << "Sonst-Fall: 1/2 Minute" << std::endl;
      break;
    }
    maxEvaluation_ = getMaxEvaluation(numberOfJobs_);
}

std::vector<int> Solver::getNEHSolution(int type){
  Rcpp::Function getNEHSolution = rEnvironment_["getNEHSolution"];
  std::vector<int> nehSolution_ = Rcpp::as<std::vector<int>>(getNEHSolution(jobData_, bufferType_, maxBufferSize_, type, targetCriterion_));
  numberOfEvaluation_ += getNEHEvaluations();
  return nehSolution_;
}

long Solver::getNEHEvaluations(){
  double d = numberOfJobs_ - 1 + numberOfJobs_*(numberOfJobs_-1) / 2;
  return (long) d;
}

long Solver::getMaxEvaluation(std::size_t n){
  switch(n){
  case(10):
    return 20000;
    break;
  case(25):
    return 100000;
    break;
  case(50):
    return 160000;
    break;
  case(75):
    return 215000;
    break;
  case(100):
    return 50000;
    break;
  default:
    return 10000;
  break;
  }
}
int Solver::evaluateSolution(Rcpp::DataFrame jobData,
                     const std::vector<int>& permutationM1,
                     const std::vector<int>& permutationM2,
                     int maxBufferSize,
                     std::string bufferType,
                     bool forceLogging,
                     bool abortOnInvalidity,
                     bool m2Special){

  return getAndLogSolutionQuality(jobData, permutationM1, permutationM2, maxBufferSize, bufferType,
                                  targetCriterion_,
                                  logFile_, startTime_, numberOfEvaluation_, iterationNumber_,
                                  bestSolutionQuality_, bestSolution_, bestSolutionM2_,
                                  forceLogging, abortOnInvalidity, m2Special);
}

int Solver::getCurrentBufferUsage(Rcpp::DataFrame jobData,
                     const std::vector<int>& permutationM1,
                     const std::vector<int>& permutationM2,
                     int maxBufferSize,
                     std::string bufferType,
                     bool abortOnInvalidity,
                     bool m2Special){

  return getAndLogBufferUsage(jobData, permutationM1, permutationM2, maxBufferSize, bufferType,
                                  logFile_, startTime_, numberOfEvaluation_, iterationNumber_,
                                  bestSolutionQuality_, abortOnInvalidity, m2Special);
}

bool Solver::terminationCriterionSatisfied(){

  std::chrono::high_resolution_clock::time_point currentTime = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> timeSpan = std::chrono::duration_cast<std::chrono::duration<double>>(currentTime - startTime_);
  double timePassed = timeSpan.count();

  return timePassed >= timeLimit_;
  // return numberOfEvaluation_ >= maxEvaluation_;
}
Solver::~Solver(){
  logFile_.close();
}

void Solver::writeBestSolution(){
  std::ofstream myfile;
  std::stringstream ss;
  ss << "./solutions/" << logFileName_ << "-" << algorithmName_ << "-" << runNumber_;
  if (fileSuffix_ != ""){
    ss << "-" << fileSuffix_;
  }
  myfile.open (ss.str(), std::ios_base::trunc);

  for (std::size_t i = 0; i < numberOfJobs_; i++){
    myfile << "j" << bestSolution_[i];
    if (i < numberOfJobs_ - 1){
      myfile << ",";
    }
  }
  myfile << std::endl;
  for (std::size_t i = 0; i < numberOfJobs_; i++){
    myfile << "j" << bestSolutionM2_[i];
    if (i < numberOfJobs_ - 1){
      myfile << ",";
    }
  }
  myfile << std::endl;

  // wenn nicht-Permutationsplan gefunden wurde
  if (bestSolutionM2_ != bestSolution_){
    myfile << "!";
  }
  myfile.close();
  // printVector(bestSolution_);
  // printVector(bestSolutionM2_);
}

