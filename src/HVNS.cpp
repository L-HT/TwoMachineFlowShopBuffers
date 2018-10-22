#include <Rcpp.h>
#include <algorithm>
#include <limits>
#include <iostream>
#include <fstream>
#include <random>
#include <cmath>

#include "HelperFunctions.h"
#include "Solver.h"

struct HVNSSolver : public Solver{
  //int iterationNumber_ = 1;
  // int maxIterations_ = 2;

  double currentTemperature_;
  double initTemperature_;
  double endTemperature_;
  double beta_;
  int nIter_;

  /*
   * localSearchEndsWithEquality = false: Ist zu Ende, wenn wahrscheinlich eine
   * Verbesserung erreicht wird
   * = true: endet, wenn keine Verbesserung erreicht werden kann (ist das nicht
   * eher lokale Suche?)
   */
  bool localSearchEndsWithEquality = false; //im Algo: true, im Text: false
  bool useFallback = true; //im Algo: false, im Text: true

  std::vector<int> currentSolution_;
  int bestSolutionQualityBeforeIteration_;
  std::vector<int> myCopy_;

  std::random_device rd;
  std::mt19937 gen;
  std::uniform_real_distribution<double> randomNumber; //im Intervall [0,1)

  HVNSSolver(Rcpp::DataFrame jobData, std::string logFileName, unsigned int runNumber,
             Rcpp::Environment rEnvironment,
             int maxBufferSize, std::string bufferType,
             std::string targetCriterion, std::string fileSuffix)
    : Solver(jobData, logFileName, runNumber, rEnvironment, maxBufferSize, bufferType,
            targetCriterion,
           "hvns", fileSuffix), initTemperature_(0.0), endTemperature_(0.0), nIter_(180000){

    Rcpp::IntegerVector m1Times = jobData["m1Time"];
    Rcpp::IntegerVector m2Times = jobData["m2Time"];
    initTemperature_ = std::accumulate(m1Times.begin(), m1Times.end(), 0.0);
    initTemperature_ = std::accumulate(m2Times.begin(), m2Times.end(), initTemperature_);
    initTemperature_ /= 5.0*2.0* (double) numberOfJobs_;

    endTemperature_ = initTemperature_ / 10.0;
    // Rcpp::Rcout << "initT: " << initTemperature_ << std::endl;
    // Rcpp::Rcout << "endT: " << endTemperature_ << std::endl;

    beta_ = (initTemperature_ - endTemperature_) / (((double) nIter_ - 1.0)*initTemperature_*endTemperature_);

    currentTemperature_ = initTemperature_;
    myCopy_.reserve(numberOfJobs_);

    //RNG
    gen = std::mt19937(rd());
    randomNumber = std::uniform_real_distribution<double>(0.0, 1.0); //Intervall [0,1)
  }

  void run(){
    setInitialSolution();
    int currentNeighborhood;
    while(!terminationCriterionSatisfied()){
      // Rcpp::Rcout << "Starte Iteration " << iterationNumber_ << std::endl;
      currentNeighborhood = 1;
      do{

        // Rcpp::Rcout << "Nachbarschaftssuche mit " << currentNeighborhood << "-ten Nachbarschaft" << std::endl;
        neighborhoodSearch(currentSolution_, currentNeighborhood);
        // int tempQuality = evaluateSolution(jobData_, currentSolution_, currentSolution_, maxBufferSize_, bufferType_);
        // if (tempQuality < bestSolutionQuality_){
        //   bestSolution_.assign(currentSolution_.begin(), currentSolution_.end());
        //   bestSolutionQuality_ = tempQuality;
        //   if (bestSolutionQuality_ != bestMeasuredMakespan_){
        //     // std::cerr << "bestMeasured ist ungleich bestQuality?1?" << std::endl;
        //   }
        //   Rcpp::Rcout << "Iteration " << iterationNumber_ << ": Verbesserung" << std::endl;
        // }
        // Rcpp::Rcout << "bestKnown1: " << bestSolutionQuality_;
        // printVector(bestSolution_);
        Rcpp::Rcout << "Nachbarschaftssuche fertig" << std::endl;

        //lokale Suche
        std::vector<int> tempVector;
        tempVector.reserve(numberOfJobs_);

        iterationNumber_ *= -1;
        if (localSearchEndsWithEquality){
          do{
            /*
             * Problem: kommt bei der SA-Suche nicht immer eine andere Lösung raus?
             * Man hat doch gerade verboten, dass take gleich insertPosition ist.
             * Im Paper steht außerdem drin, dass die Abbruch bedingung nicht die GLeichheit, sondern
             * gerade die Ungleichheit ist, was von dem angegebenen ALgo abweicht.
             */
            insertionSearchSA(currentSolution_);
            tempVector.assign(currentSolution_.begin(), currentSolution_.end());
            iterationNumber_--;
            edgeInsertionSearchSA(currentSolution_);
             // printVector(tempVector);
             // printVector(currentSolution_);
            Rcpp::checkUserInterrupt();
          } while (tempVector != currentSolution_ && !terminationCriterionSatisfied());
        } else {
          do{
            insertionSearchSA(currentSolution_);
            iterationNumber_--;
            tempVector.assign(currentSolution_.begin(), currentSolution_.end());
            edgeInsertionSearchSA(currentSolution_);
            // printVector(tempVector);
            // printVector(currentSolution_);
            Rcpp::Rcout << std::endl;
            Rcpp::checkUserInterrupt();
          } while (tempVector == currentSolution_ && !terminationCriterionSatisfied());
        }
        // Rcpp::Rcout << "lokale Suche fertig" << std::endl;

        // Rcpp::Rcout << "bestKnown2: " << bestSolutionQuality_;
        // printVector(bestSolution_);
        if (bestSolutionQuality_ < bestSolutionQualityBeforeIteration_){
          currentNeighborhood = 1;
          bestSolutionQualityBeforeIteration_ = bestSolutionQuality_;
        } else {
          currentNeighborhood++;
          // Rcpp::Rcout << "aktualisiere Nachbarschaft" << std::endl;
        }
        Rcpp::checkUserInterrupt();
        iterationNumber_ *= -1;
        iterationNumber_++;
      } while (currentNeighborhood <= 3 && !terminationCriterionSatisfied());
      //logResults();
      iterationNumber_ += 10;
    }

    writeBestSolution();
  }

  void setInitialSolution(){
    // Rcpp::Function getNEHSolution = rEnvironment_["getNEHSolution"];
    // std::vector<int> tempSolution = Rcpp::as<std::vector<int>>(getNEHSolution(jobData_, bufferType_, maxBufferSize_, 1));
    std::vector<int> tempSolution = getNEHSolution(1);
    bestSolution_.assign(tempSolution.begin(), tempSolution.end());
    currentSolution_.assign(tempSolution.begin(), tempSolution.end());
    bestSolutionQuality_ = evaluateSolution(jobData_, bestSolution_, bestSolution_, maxBufferSize_,bufferType_, true);

    //bestMeasuredMakespan_ = bestSolutionQuality_;
    bestSolutionQualityBeforeIteration_ = bestSolutionQuality_;
  }

  ///////////////////
  ///////// lokale Suche (auf SA basierend)
  ///////////////////

  void insertionSearchSA(std::vector<int>& permutation){
    std::size_t counter = 0;
    std::size_t fallbackCounter = 0;
    int bestMakespan = std::numeric_limits<int>::max();
    int tempMakespan = std::numeric_limits<int>::max();
    //std::size_t bestTakePosition = 0;
    std::size_t bestInsertPosition = 1;
    double delta = 0;
    bool permutationHasChanged = false;

    //Zufallspermutation generieren

    std::vector<int> randomPermutation = Rcpp::as<std::vector<int>>(Rcpp::sample(numberOfJobs_, numberOfJobs_));

    // Rcpp::Rcout << "Permutation: ";
    // printVector(permutation);
    // Rcpp::Rcout << "RP: ";
    // printVector(randomPermutation);

    int currentMakespan = evaluateSolution(jobData_, permutation, permutation, maxBufferSize_, bufferType_);
    // Zufallspermutation aus 1:numberOfJobs_
    // Rcpp::Rcout << "current: " << currentMakespan << std::endl;
    do{
      // finde den Job
      std::size_t takePosition = 0;
      for (std::size_t i = 0; i < numberOfJobs_; i++){
        if (permutation[i] == randomPermutation[counter]){
          takePosition = i;
        }
      }
      // Rcpp::Rcout << "takePosition: " << takePosition << std::endl;
      bestMakespan = std::numeric_limits<int>::max();

      //finde beste Insert-Position
      for (std::size_t insertPosition = 0; insertPosition < numberOfJobs_; insertPosition++){
        if (takePosition != insertPosition){
          myCopy_.assign(permutation.begin(), permutation.end());
          insertOperator(myCopy_, takePosition, insertPosition);
          tempMakespan = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
          if (tempMakespan < bestMakespan){
            bestMakespan = tempMakespan;
            //bestTakePosition = takePosition;
            bestInsertPosition = insertPosition;
          }
        }
      }

      delta = bestMakespan - currentMakespan;
      // Rcpp::Rcout << "best: " << bestMakespan << std::endl;
      // Rcpp::Rcout << takePosition << " - " << bestInsertPosition << " - " << delta << std::endl;
      double myRandomNumber = randomNumber(gen);
      // Rcpp::Rcout << myRandomNumber << " - " << std::exp(-delta / currentTemperature_) << std::endl;
      if (myRandomNumber < std::exp(-delta / currentTemperature_)){
        insertOperator(permutation, takePosition, bestInsertPosition);
        currentMakespan = bestMakespan;
        randomPermutation = Rcpp::as<std::vector<int>>(Rcpp::sample(numberOfJobs_, numberOfJobs_));
        counter = 0;
        permutationHasChanged = true;
        // Rcpp::Rcout << "Permutation: ";
        // printVector(permutation);
        // Rcpp::Rcout << "RP: ";
        // printVector(randomPermutation);

      } else {
        counter++;
      }
      // if (bestMakespan < bestSolutionQuality_){
      //   bestSolution_.assign(permutation.begin(), permutation.end());
      //   bestSolutionQuality_ = bestMakespan;
      //   if (bestSolutionQuality_ != bestMeasuredMakespan_){
      //     // std::cerr << "bestMeasured ist ungleich bestQuality?3?" << std::endl;
      //   }
      // }
      currentTemperature_ = currentTemperature_ / (1 + beta_*currentTemperature_);
      //Rcpp::Rcout << currentTemperature_ << " - " << counter << std::endl << std::endl;
      if (permutationHasChanged && useFallback){
        fallbackCounter++;

      }
    } while (counter < numberOfJobs_ && fallbackCounter < numberOfJobs_);
    if (counter >= numberOfJobs_){
      // Rcpp::Rcout << "Fallback not used??" << std::endl;
    }
  }

  void edgeInsertionSearchSA(std::vector<int>& permutation){
    std::size_t counter = 0;

    int bestMakespan = std::numeric_limits<int>::max();
    int tempMakespan = std::numeric_limits<int>::max();
    std::size_t bestTakePosition = 0;
    std::size_t bestInsertPosition = 1;
    int delta = 0;

    std::size_t fallbackCounter = 0;
    bool permutationHasChanged = false;

    std::random_device rd;  //Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<double> randomNumber(0.0, 1.0);
    std::vector<int> randomPermutation = Rcpp::as<std::vector<int>>(Rcpp::sample(numberOfJobs_, numberOfJobs_));

    int currentMakespan = evaluateSolution(jobData_, permutation, permutation, maxBufferSize_, bufferType_);
    // Zufallspermutation aus 1:numberOfJobs_
    do{
      // finde den Job
      std::size_t takePosition = 0;
      for (std::size_t i = 0; i < numberOfJobs_; i++){
        if (permutation[i] == randomPermutation[counter]){
          takePosition = i;
        }
      }

      if (takePosition < numberOfJobs_ - 1){
        //finde beste Insert-Position
        for (std::size_t insertPosition = 0; insertPosition < numberOfJobs_ - 1; insertPosition++){
          if (takePosition != insertPosition){
            myCopy_.assign(permutation.begin(), permutation.end());
            edgeInsertOperator(myCopy_, takePosition, insertPosition);
            tempMakespan = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
            if (tempMakespan < bestMakespan){
              bestMakespan = tempMakespan;
              bestTakePosition = takePosition;
              bestInsertPosition = insertPosition;
            }
          }
        }

        delta = bestMakespan - currentMakespan;
        if (randomNumber(gen) < std::exp(-delta / currentTemperature_)){
          insertOperator(permutation, bestTakePosition, bestInsertPosition);
          currentMakespan = bestMakespan;
          randomPermutation = Rcpp::as<std::vector<int>>(Rcpp::sample(numberOfJobs_, numberOfJobs_));
          counter = 0;
          permutationHasChanged = true;
        } else {
          counter++;
        }
        // if (bestMakespan < bestSolutionQuality_){
        //   bestSolution_.assign(permutation.begin(), permutation.end());
        //   bestSolutionQuality_ = bestMakespan;
        //   if (bestSolutionQuality_ != bestMeasuredMakespan_){
        //     // std::cerr << "bestMeasured ist ungleich bestQuality?4?" << std::endl;
        //   }
        // }
        currentTemperature_ = currentTemperature_ / (1 + beta_*currentTemperature_);
      } else { // wenn takePosition ganz hinten ist
        counter++;
      }
      if (permutationHasChanged && useFallback){
        fallbackCounter++;
      }
    } while (counter < numberOfJobs_ && fallbackCounter < numberOfJobs_);
    if (counter >= numberOfJobs_){
      // Rcpp::Rcout << "Fallback not used??" << std::endl;
    }
  }

  ////////////////////////////
  ///////// Nachbarschaftssuche
  ////////////////////////////


  void neighborhoodSearch(std::vector<int>& permutation, int currentNeighborhood){
    switch(currentNeighborhood){
    case(1):
      bestInsertionShaking(permutation);
      break;
    case(2):
      bestEdgeInsertionShaking(permutation);
      break;
    case(3):
      bestSwapShaking(permutation);
      break;
    }
  }
  void bestInsertionShaking(std::vector<int>& permutation){
    int bestMakespan = std::numeric_limits<int>::max();
    int tempMakespan = std::numeric_limits<int>::max();
    std::size_t bestTakePosition = 0;
    std::size_t bestInsertPosition = 1;
    for (std::size_t i = 0; i < numberOfJobs_; i++){
      for (std::size_t j = 0; j < numberOfJobs_; j++){
        if (i != j){
          myCopy_.assign(permutation.begin(), permutation.end());
          insertOperator(myCopy_, i, j);
          tempMakespan = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
          if (tempMakespan < bestMakespan){
            bestMakespan = tempMakespan;
            bestTakePosition = i;
            bestInsertPosition = j;
          }
        }
      }
    }
    // Rcpp::Rcout << bestTakePosition << " " << bestInsertPosition << std::endl;
    insertOperator(permutation, bestTakePosition, bestInsertPosition);
  }

  // nimm das Element an takePosition und pack es auf insertPosition
  // Rest wird dann angemessen verschoben
  // void edgeInsertOperator(std::vector<int>& permutation, int takePosition, int insertPosition){
  //   int value1ToInsert = permutation[takePosition];
  //   int value2ToInsert = permutation[takePosition + 1];
  //   if (takePosition < insertPosition){
  //     for (int i = takePosition; i <= (insertPosition - 1); i++){
  //       permutation[i] = permutation[i+2];
  //     }
  //     permutation[insertPosition] = value1ToInsert;
  //     permutation[insertPosition+1] = value2ToInsert;
  //   }
  //   if (insertPosition < takePosition){
  //     for (int i = takePosition+1; i >= (insertPosition + 2); i--){
  //       permutation[i] = permutation[i-2];
  //     }
  //     permutation[insertPosition] = value1ToInsert;
  //     permutation[insertPosition+1] = value2ToInsert;
  //   }
  // }
  void bestEdgeInsertionShaking(std::vector<int>& permutation){
    int bestMakespan = std::numeric_limits<int>::max();
    int tempMakespan = std::numeric_limits<int>::max();
    std::size_t bestTakePosition = 0;
    std::size_t bestInsertPosition = 1;
    for (std::size_t i = 0; i < numberOfJobs_ - 1; i++){
      for (std::size_t j = 0; j < numberOfJobs_ - 1; j++){
        if (i != j){
          myCopy_.assign(permutation.begin(), permutation.end());
          edgeInsertOperator(myCopy_, i, j);
          tempMakespan = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
          if (tempMakespan < bestMakespan){
            bestMakespan = tempMakespan;
            bestTakePosition = i;
            bestInsertPosition = j;
          }
        }
      }
    }
    // Rcpp::Rcout << bestTakePosition << " " << bestInsertPosition << std::endl;
    edgeInsertOperator(permutation, bestTakePosition, bestInsertPosition);
  }

  void bestSwapShaking(std::vector<int>& permutation){
    int bestMakespan = std::numeric_limits<int>::max();
    int tempMakespan = std::numeric_limits<int>::max();
    std::size_t bestPosition1 = 0;
    std::size_t bestPosition2 = 1;
    for (std::size_t i = 0; i < numberOfJobs_; i++){
      for (std::size_t j = i + 1; j < numberOfJobs_; j++){
        if (i != j){
          myCopy_.assign(permutation.begin(), permutation.end());
          swapOperator(myCopy_, i, j);
          tempMakespan = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
          if (tempMakespan < bestMakespan){
            bestMakespan = tempMakespan;
            bestPosition1 = i;
            bestPosition2 = j;
          }
        }
      }
    }
    // Rcpp::Rcout << bestPosition1 << " " << bestPosition2 << std::endl;
    swapOperator(permutation, bestPosition1, bestPosition2);
  }

  // # HVNS 100: 260000
  // # HVNS 75: 220000
  // # HVNS 50: 160000
  long getMaxEvaluation(std::size_t n){
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
      return 220000;
      break;
    case(100):
      return 260000;
      break;
    default:
      return 10000;
      break;
    }
  }
  // bool terminationCriterionSatisfied(){
  //   return iterationNumber_ > maxIterations_;
  // }

  // void logResults(){
  //   logFile_ << iterationNumber_ << "," << bestSolutionQuality_ << std::endl;
  // }
  // void writeBestSolution(){
  //   std::ofstream myfile;
  //   std::stringstream ss;
  //   ss << "./solutions/" << logFileName_ << "-" << "hvns" << "-" << runNumber_;
  //   if (fileSuffix_ != ""){
  //     ss << "-" << fileSuffix_;
  //   }
  //   myfile.open (ss.str(), std::ios_base::trunc);
  //
  //   for (std::size_t i = 0; i < numberOfJobs_; i++){
  //     myfile << "j" << bestSolution_[i];
  //     if (i < numberOfJobs_ - 1){
  //       myfile << ",";
  //     }
  //   }
  //   myfile << std::endl;
  //   printVector(bestSolution_);
  // }
};


//' @export
// [[Rcpp::export]]
void startHVNS(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType = "intermediateBuffer",
               std::string targetCriterion = "makespan",
               unsigned int runNumber = 1, std::string fileSuffix = ""){
  // Rcpp::Rcout << "Start HVNS" << std::endl;
  Rcpp::Environment myEnvironment(MYPACKAGE);
  HVNSSolver hvnss(jobData, logFileName, runNumber, myEnvironment, maxBufferSize, bufferType, targetCriterion, fileSuffix);

  ///////Bodeih/////////

  // Rcpp::Rcout << "run aufgerufen" << std::endl;
  hvnss.run();
  // Rcpp::Rcout << "Feddich";
}

