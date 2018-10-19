#include <Rcpp.h>
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <cstdlib>
#include <chrono>
#include <fstream>
#include <random>

#include "HelperFunctions.h"
#include "SimulationC.h"
#include "SimulationCBufferUsage.h"
#include "SpecialSimulations.h"
#include "SpecialSimulationsIdleTime.h"

std::vector<std::string> vectorToPermutation(std::vector<int> x){
  std::vector<std::string> result;
  result.reserve(x.size());
  for (int i : x){
    result.push_back("j" + std::to_string(i));
  }
  return result;
}

int getSolutionQuality(Rcpp::DataFrame jobData, const std::vector<int>& permutationM1, const std::vector<int>& permutationM2,
                       int maxBufferSize, std::string bufferType,
                       std::string targetCriterion,
                       bool abortOnInvalidity, bool m2Special){
  std::vector<std::string> perm1AsString = vectorToPermutation(permutationM1);
  std::vector<std::string> perm2AsString = vectorToPermutation(permutationM2);

;
  if (targetCriterion == "TFT"){
    if (bufferType == "intermediateBuffer"){
      return simulateFlowShopC_TFT(jobData, perm1AsString, perm2AsString, maxBufferSize, abortOnInvalidity);
    }
    if (bufferType == "totalBuffer"){
      return simulateFlowShopTotalBufferC_TFT(jobData, perm1AsString, perm2AsString, maxBufferSize, abortOnInvalidity);
    }
  }
  if (targetCriterion == "dueTimes"){
    if (bufferType == "intermediateBuffer"){
      return simulateFlowShopC_DueTime(jobData, perm1AsString, perm2AsString, maxBufferSize, abortOnInvalidity);
    }
    if (bufferType == "totalBuffer"){
      return simulateFlowShopTotalBufferC_TFT(jobData, perm1AsString, perm2AsString, maxBufferSize, abortOnInvalidity);
    }
  }
  if (targetCriterion == "makespan"){
    if (m2Special){
      // Rcpp::Rcout << "quali: ";
      // printVector(perm1AsString);

      if (bufferType == "intermediateBuffer"){
        return simulateSpecial(jobData, perm1AsString, maxBufferSize, abortOnInvalidity, true);
      }
      if (bufferType == "totalBuffer"){
        return simulateSpecialTotalBuffer(jobData, perm1AsString, maxBufferSize, abortOnInvalidity, true);
      }
    } else {
      if (bufferType == "intermediateBuffer"){
        return simulateFlowShopC(jobData, perm1AsString, perm2AsString,  maxBufferSize, abortOnInvalidity);
      }
      if (bufferType == "totalBuffer"){
        return simulateFlowShopTotalBufferC(jobData, perm1AsString, perm2AsString,  maxBufferSize, abortOnInvalidity);
      }
    }
  }


  Rcpp::stop("getSolutionQuality: kein passende Simulation gefunden");
  return 0;
}
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
                             bool forceLogging,
                             bool abortOnInvalidity,
                             bool m2Special){

  // Rcpp::Rcout << m2Special << std::endl;

  // if (!m2Special){
  //   Rcpp::stop("blablabla");
  // }
  std::chrono::high_resolution_clock::time_point currentTime = std::chrono::high_resolution_clock::now();

  std::chrono::duration<double> timeSpan = std::chrono::duration_cast<std::chrono::duration<double>>(currentTime - startTime);

  double timePassed = timeSpan.count();
  int result = getSolutionQuality(jobData, permutationM1, permutationM2, maxBufferSize,
                                  bufferType, targetCriterion,
                                  abortOnInvalidity, m2Special);

  std::size_t numberOfJobs = jobData.nrows();

  // waitForInput("eval", true);
  // Rcpp::Rcout << "BufSize: " << maxBufferSize << std::endl;
  // Rcpp::Rcout << currentBest << " -- " << result << ": " << m2Special << std::endl;
  // printVector(permutationM1);
  // Rcpp::Rcout << "Jobnummer: " << numberOfJobs << std::endl;

  //wenn besser und zulässige Lösung
  if (result < currentBest && permutationM1.size() == numberOfJobs && permutationM2.size() == numberOfJobs && result > 0){
    // Rcpp::Rcout << "BufSize: " << maxBufferSize << std::endl;
    // Rcpp::Rcout << currentBest << " -- " << result << ": " << m2Special << std::endl;
    // printVector(permutationM1);
    // Rcpp::Rcout << "Jobnummer: " << numberOfJobs << std::endl;

    currentBest = result;
    currentBestSolution.assign(permutationM1.begin(), permutationM1.end());
    currentBestSolutionM2.assign(permutationM2.begin(), permutationM2.end());
    // printVector(currentBestSolution);
    // printVector(currentBestSolutionM2);
  }

  if (numberOfEvaluation % LOGINTERVAL == 0 || forceLogging){
    logFile << numberOfEvaluation << "," << timePassed << ",";
    logFile << iterationNumber << "," << currentBest;
    logFile << std::endl;

    // vielleicht noch ein Flush?
  }
  numberOfEvaluation++;


  return result;
}
// nimm das Element an takePosition und pack es auf insertPosition
// Rest wird dann angemessen verschoben
void insertOperator(std::vector<int>& permutation, int takePosition, int insertPosition){
  /*
  * Wenn Zielposition rechts von Startposition: alles ab Start verschieben
  */
  int valueToInsert = permutation[takePosition];
  if (takePosition < insertPosition){
    for (int i = takePosition; i <= (insertPosition - 1); i++){
      permutation[i] = permutation[i+1];
    }
    permutation[insertPosition] = valueToInsert;
  }
  if (insertPosition < takePosition){
    for (int i = takePosition; i >= (insertPosition + 1); i--){
      permutation[i] = permutation[i-1];
    }
    permutation[insertPosition] = valueToInsert;
  }
}

void swapOperator(std::vector<int>& permutation, int position1, int position2){
  int temp = permutation[position1];
  permutation[position1] = permutation[position2];
  permutation[position2] = temp;
}
void edgeInsertOperator(std::vector<int>& permutation, int takePosition, int insertPosition){
  int value1ToInsert = permutation[takePosition];
  int value2ToInsert = permutation[takePosition + 1];
  if (takePosition < insertPosition){
    for (int i = takePosition; i <= (insertPosition - 1); i++){
      permutation[i] = permutation[i+2];
    }
    permutation[insertPosition] = value1ToInsert;
    permutation[insertPosition+1] = value2ToInsert;
  }
  if (insertPosition < takePosition){
    for (int i = takePosition+1; i >= (insertPosition + 2); i--){
      permutation[i] = permutation[i-2];
    }
    permutation[insertPosition] = value1ToInsert;
    permutation[insertPosition+1] = value2ToInsert;
  }
}

// ersetze einen Job an pos durch einen zufällig ungeplanten Job
int replaceOperator(std::vector<int>& permutation, int pos, std::size_t numberOfJobs,
                    int forbiddenJob){
  if (permutation.size() == numberOfJobs){
    return -1;
  }

  std::vector<int> unplannedJobs;
  for (int job = 1; job <= (int) numberOfJobs; job++){
    //der nächste Job darf nicht verplant werden
    if (std::find(permutation.begin(), permutation.end(), job) == permutation.end()
          && job != forbiddenJob){
      unplannedJobs.push_back(job);
    }
  }
  int chosenIndex = getRandomNumber(0, unplannedJobs.size() - 1);
  // Rcpp::Rcout << pos << "." << unplannedJobs[chosenIndex] << std::endl;
  // printVector(permutation);

  permutation[pos] = unplannedJobs[chosenIndex];
  return unplannedJobs[chosenIndex];
}

void initRandom(){
  srand(171115);
}

//Grenzen sind mit drin
int getRandomNumber(int min, int max){
  int result = min + (rand() % static_cast<int>(max - min + 1));
  //Rcpp::Rcout << result << " ";
  return result;
}

//
// std::vector<int> permutationToVector(std::vector<std::string> x){
//   std::vector<int> result;
//   result.reserve(x.size());
//
//   for (std::string s : x){
//     result.push_back(s.);
//   }
//
//   for (std::string s : result){
//     std::cout << s;
//   }
//   return result;
// }


std::vector<int> getRandomPermutation(std::size_t numberOfJobs, std::mt19937& rng){
  std::vector<int> result;
  for (std::size_t i = 1; i <= numberOfJobs; i++){
    result.push_back(i);
  }
  std::shuffle(result.begin(), result.end(), rng);
  return result;
}

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
                             bool m2Special){

  std::chrono::high_resolution_clock::time_point currentTime = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> timeSpan = std::chrono::duration_cast<std::chrono::duration<double>>(currentTime - startTime);
  double timePassed = timeSpan.count();

  std::vector<std::string> perm1AsString = vectorToPermutation(permutationM1);
  std::vector<std::string> perm2AsString = vectorToPermutation(permutationM2);
  int result = 0;

  // Rcpp::Rcout << "buf: ";
  // printVector(perm1AsString);
  // printVector(perm2AsString);
  if (m2Special){
    if (bufferType == "intermediateBuffer"){
      result = simulateSpecial(jobData, perm1AsString, maxBufferSize, abortOnInvalidity, false);
    }
    if (bufferType == "totalBuffer"){
      result = simulateSpecialTotalBuffer(jobData, perm1AsString, maxBufferSize, abortOnInvalidity, false);
    }
  } else {
    if (bufferType == "intermediateBuffer"){
      result = simulateBufferUsage(jobData, perm1AsString, perm2AsString,  maxBufferSize, abortOnInvalidity);
    }
    if (bufferType == "totalBuffer"){
      result = simulateBufferUsageTotalBuffer(jobData, perm1AsString, perm2AsString,  maxBufferSize, abortOnInvalidity);
    }
  }

  if (numberOfEvaluation % LOGINTERVAL == 0){
    logFile << numberOfEvaluation << "," << timePassed << ",";
    logFile << iterationNumber << "," << currentBest;
    logFile << std::endl;
    // vielleicht noch ein Flush?
  }
  numberOfEvaluation++;


  return result;
}

void waitForInput(std::string msg, bool enabled){
  if (enabled){
    Rcpp::Environment baseEnvironment = Rcpp::Environment("package:base");
    Rcpp::Function readLineR = baseEnvironment["readline"];
    Rcpp::Rcout << msg << " (press Enter)" << std::endl;
    readLineR("");
  }
}

