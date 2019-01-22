#include <Rcpp.h>
#include <algorithm>
#include <limits>
#include <iostream>
#include <fstream>

#include "HelperFunctions.h"
#include "Solver.h"
/*
 * Der diskrete ABC speziell für 2mFSBuf.
 * Sppbo initialisiert alles und läuft durch.
 * Dabei loggt er die Ergebnisse.
 */


struct DABCSolver : public Solver{
  // const Rcpp::DataFrame jobData_;
  // const std::string logFileName_;
  // const unsigned int runNumber_;
  //
  // Rcpp::Environment rEnvironment_;
  // Rcpp::Function evaluateSolution_;
  //
  // const int maxBufferSize_;
  // const std::string bufferType_;
  // const std::size_t numberOfJobs_;
  //int iterationNumber_ = 1;
  int maxIterations_ = 2;

  int populationSize_ = 50; //50
  int perturbationStrength_ = 3; //3
  int destructionSize1_ = 5; //5
  int destructionSize2_ = 6; //6
  int numberOfInserts_ = 10; //n (wird unten so gesetzt)
  int localSearchAttempts_ = 10; //n^2 (wird unten so gesetzt)

  std::vector<std::vector<int>> population_;
  std::vector<int> populationQualities_;
  std::vector<int> myCopy_;

  DABCSolver(Rcpp::DataFrame jobData, std::string logFileName, unsigned int runNumber, Rcpp::Environment rEnvironment,
         int maxBufferSize, std::string bufferType,
         std::string targetCriterion, std::string fileSuffix)

      : Solver(jobData, logFileName, runNumber, rEnvironment, maxBufferSize, bufferType,
        targetCriterion,
        "dabc", fileSuffix){

    numberOfInserts_ = numberOfJobs_;
    localSearchAttempts_ = numberOfJobs_ * numberOfJobs_;
    population_.reserve(populationSize_);
    myCopy_.reserve(numberOfJobs_);
  }

  // Konstruktor mit Parametern
  DABCSolver(Rcpp::DataFrame jobData, std::string logFileName, unsigned int runNumber, Rcpp::Environment rEnvironment,
             int maxBufferSize, std::string bufferType,
             std::string targetCriterion, std::string fileSuffix,
             int populationSize,
             int perturbationStrength,
             int destructionSize1,
             int destructionSize2,
             int numberOfInserts,
             int localSearchAttempts)

    : Solver(jobData, logFileName, runNumber, rEnvironment, maxBufferSize, bufferType,
      targetCriterion,
      "dabc", fileSuffix),
      populationSize_(populationSize),
      perturbationStrength_(perturbationStrength),
      destructionSize1_(destructionSize1),
      destructionSize2_(destructionSize2),
      numberOfInserts_(numberOfInserts),
      localSearchAttempts_(localSearchAttempts)
      {

    population_.reserve(populationSize_);
    myCopy_.reserve(numberOfJobs_);
  }

  void run(){
    initializePopulation();
    iterationNumber_ += 10;
    Rcpp::Rcout << "Population initialisiert" << std::endl;

    while(!terminationCriterionSatisfied()){
      //Employed-Bees variieren
      for (int i = 0; i < populationSize_; i++){
        tryToUpdate(population_[i], i);
        // Lokale Suche
        int r = getRandomNumber(1,100);
        if (r == 42){ // eine Zahl aus 100 ist 1%
          localSearch(population_[i], i);
        }
      }
      iterationNumber_++;
      Rcpp::Rcout << "E-Bienen fertig (Iteration " << iterationNumber_ << ")" << std::endl;

      //Onlooker-Bees arbeiten
      for (int i = 0; i < 2*populationSize_; i++){

        int winner = tournamentSelectionSize2(true);
        optimizedInsert(winner);
        // Rcpp::Rcout << i << std::endl;
      }
      Rcpp::Rcout << "O-Bienen fertig (Iteration " << iterationNumber_ << ")" << std::endl;
      iterationNumber_++;
      Rcpp::checkUserInterrupt();

      //Scout-Bees arbeiten
      for (int i = 0; i < (int) populationSize_/10; i++){
        int loser = tournamentSelectionSize2(false);
        myCopy_.assign(population_[loser].begin(), population_[loser].end());
        destructConstructOperator(myCopy_, 4);
        int newMakespan = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
        if (newMakespan < populationQualities_[loser]){
          populationQualities_[loser] = newMakespan;
          population_[loser].assign(myCopy_.begin(), myCopy_.end());
        }
      }
      iterationNumber_++;
      Rcpp::Rcout << "S-Bienen fertig (Iteration " << iterationNumber_ << ")" << std::endl;
      //logResults();
      Rcpp::checkUserInterrupt();
      iterationNumber_ += 10;
    }
    writeBestSolution();
  }


  void initializePopulation(){
    //Population erzeugen

    // Rcpp::Function getNEHSolution = rEnvironment_["getNEHSolution"];
    for (int i = 0; i < populationSize_; i++){
      //Rcpp::Rcout << "Try Neh";
      std::vector<int> tempSolution = getNEHSolution(i);
      //printVector(tempSolution);

      population_.push_back(tempSolution);

      numberOfEvaluation_ += getNEHEvaluations();
      int tempQuality = evaluateSolution(jobData_, tempSolution, tempSolution, maxBufferSize_, bufferType_, true, true, false);
      populationQualities_.push_back(tempQuality);
      //Rcpp::Rcout << "no: " << numberOfEvaluation_ << std::endl;
      if (terminationCriterionSatisfied()){
        writeBestSolution();
        Rcpp::stop("Zeit abgelaufen");
      }
    }


    //bestMeasuredMakespan_ = *(std::min_element(populationQualities_.begin(), populationQualities_.end()));
  }

  // bool terminationCriterionSatisfied(){
  //   return iterationNumber_ > 3;
  // }

  // void logResults(){
  //   int bestSolutionIndex = getBestIndividual();
  //   logFile_ << iterationNumber_ << "," << populationQualities_[bestSolutionIndex] << std::endl;
  // }

  int getBestIndividual(){
    std::vector<int>::iterator result = std::min_element(populationQualities_.begin(), populationQualities_.end());
    return std::distance(populationQualities_.begin(), result);
  }


  // void writeBestSolution(){
  //   std::ofstream myfile;
  //
  //   std::stringstream ss;
  //   ss << "./solutions/" << logFileName_ << "-" << "dabc" << "-" << runNumber_;
  //   if (fileSuffix_ != ""){
  //     ss << "-" << fileSuffix_;
  //   }
  //   myfile.open (ss.str(), std::ios_base::trunc); //app für append
  //
  //   int bestSolutionIndex = getBestIndividual();
  //   for (std::size_t i = 0; i < numberOfJobs_; i++){
  //     myfile << "j" << population_[bestSolutionIndex][i];
  //     if (i < numberOfJobs_ - 1){
  //       myfile << ",";
  //     }
  //   }
  //
  //   myfile << std::endl;
  // }

  ///////////// Funktion für die Onlooker-Bees

  void optimizedInsert(int winner){
    int jobToMove = getRandomNumber(0, numberOfJobs_ - 1);
    int insertPosition = 0;
    int bestInsertPosition = 0;
    int bestMakespan = std::numeric_limits<int>::max();

    for (int j = 0; j < numberOfInserts_; j++){
      myCopy_.assign(population_[winner].begin(), population_[winner].end());
      do{
        insertPosition = getRandomNumber(0, numberOfJobs_ - 1);
      } while (insertPosition == jobToMove && !terminationCriterionSatisfied());
      //Rcpp::Rcout << "From " << jobToMove << " to " << insertPosition << std::endl;
      insertOperator(myCopy_, jobToMove, insertPosition);
      //printVector(population_[winner]);
      //printVector(myCopy_);

      int currentMakespan = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
      if (currentMakespan < bestMakespan){
        bestInsertPosition = insertPosition;
        bestMakespan = currentMakespan;
      }
    }

    if (bestMakespan < populationQualities_[winner]){
      insertOperator(population_[winner], jobToMove, bestInsertPosition);
      populationQualities_[winner] = bestMakespan;
    }

  }

  int tournamentSelectionSize2(bool wantWinner){
    int contender1 = getRandomNumber(0, populationSize_ - 1);
    int contender2 = getRandomNumber(0, populationSize_ - 1);
    int result = 0;
    bool contender1Wins = (populationQualities_[contender1] < populationQualities_[contender2]);
    if (wantWinner){
      result = (contender1Wins) ? contender1 : contender2;
    } else {
      result = (contender1Wins) ? contender2 : contender1;
    }
    return result;
  }

  ///////////// Funktionen für die Employer-Bees

  void tryToUpdate(std::vector<int>& permutation, int index){
    myCopy_.assign(permutation.begin(), permutation.end());
    int selectedStrategy = getRandomNumber(1,8);
    switch(selectedStrategy){
      case 1:
        s1(myCopy_);
        break;
      case 2:
        s2(myCopy_);
        break;
      case 3:
        s3(myCopy_);
        break;
      case 4:
        s4(myCopy_);
        break;
      case 5:
        s5(myCopy_);
        break;
      case 6:
        s6(myCopy_);
        break;
      case 7:
        s7(myCopy_);
        break;
      case 8:
        s8(myCopy_);
        break;
    }
    int newSolutionQuality = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
    if (newSolutionQuality < populationQualities_[index]){
      populationQualities_[index] = newSolutionQuality;
      permutation.assign(myCopy_.begin(), myCopy_.end());
    }
  }


  void localSearch(std::vector<int>& permutation, int index){
    int l = 1;
    int counter = 1;

    // Rcpp::Rcout << "lokale Suche aufgerufen" << std::endl;

    while (l < localSearchAttempts_ && !terminationCriterionSatisfied()){

      //versuche Insert
      myCopy_.assign(permutation.begin(), permutation.end());
      insertOperator(myCopy_, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
      int newSolutionQuality = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
      if (newSolutionQuality < populationQualities_[index]){
        populationQualities_[index] = newSolutionQuality;
        permutation.assign(myCopy_.begin(), myCopy_.end());
        counter = 1;
      } else {
        counter++;
      }

      //versuche Swap
      if (counter == 2){
        myCopy_.assign(permutation.begin(), permutation.end());
        swapOperator(myCopy_, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
        newSolutionQuality = evaluateSolution(jobData_, myCopy_, myCopy_, maxBufferSize_, bufferType_);
        if (newSolutionQuality < populationQualities_[index]){
          populationQualities_[index] = newSolutionQuality;
          permutation.assign(myCopy_.begin(), myCopy_.end());
          counter = 1;
        } else {
          counter++;
        }
      }

      //schlägt fehl
      if (counter >= 3){
        counter = 1;
        l++;
      }
    }
  }



  void destructConstructOperator(std::vector<int>& permutation, int destructionSize){
    std::list<std::size_t> indicesToRemove;
    std::list<int> removed, remain;
    //verschiedene Zufallszahlen ermitteln
    int randomNumbersFound = 0;

    // ausreichend Zufallsindizes suchen
    while (randomNumbersFound < destructionSize){
      std::size_t tempValue = (std::size_t) getRandomNumber(0, permutation.size()-1);
      if (!contains(indicesToRemove, tempValue)){
        indicesToRemove.push_back(tempValue);
        randomNumbersFound++;
      }
    }

    // Destruct
    for (std::size_t i = 0; i < permutation.size(); i++){
      if (contains(indicesToRemove, i)){
        removed.push_back(permutation[i]);
      } else {
        remain.push_back(permutation[i]);
      }
    }

    //printList(remain);
    //printList(removed);

    // Construct
    int bestMakespan;
    int currentMakespan;
    std::vector<int> tempVector;
    tempVector.reserve(numberOfJobs_);

    std::size_t currentIndex = 0;
    std::size_t bestIndex = 0;

    for (std::list<int>::const_iterator it = removed.begin(); it != removed.end(); it++){
      bestMakespan = std::numeric_limits<int>::max();
      currentMakespan = std::numeric_limits<int>::max();

      //std::list<int>::iterator subIt = remain.begin();
      //for (std::size_t j = 0; j < remain.size() + 1; j++){
      for (std::list<int>::iterator subIt = remain.begin(); subIt != remain.end(); subIt++){
        remain.insert(subIt, *it); //1 zurückgehen, damit der Iterator auf neu eingefügtes Element zeigt

        // zu std::vector machen
        tempVector.clear();
        std::copy(std::begin(remain), std::end(remain), std::back_inserter(tempVector));

        //printVector(tempVector);
        //Rcpp::Rcout << "-" << *subIt << "-" << std::endl;

        currentMakespan = evaluateSolution(jobData_, tempVector, tempVector, maxBufferSize_, bufferType_);
        if (currentMakespan < bestMakespan){
          bestMakespan = currentMakespan;
          bestIndex = currentIndex;
        }
        currentIndex++;

        //neu angefügtes Element wieder entfernen
        std::list<int>::iterator tempIt = subIt;
        tempIt--;
        remain.erase(tempIt);

        //printList(remain);
        //Rcpp::Rcout << "-" << *subIt << "-" << std::endl;
      }

      //noch Einfügen am Ende prüfen
      tempVector.clear();
      std::copy(std::begin(remain), std::end(remain), std::back_inserter(tempVector));
      tempVector.push_back(*it);

      //printVector(tempVector);
      currentMakespan = evaluateSolution(jobData_, tempVector, tempVector, maxBufferSize_, bufferType_);
      if (currentMakespan < bestMakespan){
        bestMakespan = currentMakespan;
        bestIndex = currentIndex;
      }

      ///////////////////////////////////
      // so, jetzt ist der innere Durchlauf fertig...

      //anfüg-Fall: bestIndex ist remain.size, darunter ist erster Fall
      if (bestIndex < remain.size()){
        std::list<int>::iterator bestPosition = remain.begin();
        std::advance(bestPosition, bestIndex);
        remain.insert(bestPosition, *it);
      } else {
        remain.push_back(*it);
      }
      //printList(remain);
    }

    // Ergebnis schreiben
    permutation.assign(remain.begin(), remain.end());
  }

  // performing one insert move
  void s1(std::vector<int>& permutation){
    insertOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
  }
  // one swap
  void s2(std::vector<int>& permutation){
    swapOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
  }
  // two insert
  void s3(std::vector<int>& permutation){
    insertOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
    insertOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
  }
  // two swap
  void s4(std::vector<int>& permutation){
    swapOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
    swapOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
  }
  // three insert
  void s5(std::vector<int>& permutation){
    insertOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
    insertOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
    insertOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
  }
  // three swaps
  void s6(std::vector<int>& permutation){
    swapOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
    swapOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
    swapOperator(permutation, getRandomNumber(0, permutation.size()-1), getRandomNumber(0, permutation.size()-1));
  }
  // Destruct/Construct mit Stärke destructionSize1_
  void s7(std::vector<int>& permutation){
    destructConstructOperator(permutation, destructionSize1_);
  }
  // Destruct/Construct mit Stärke destructionSize2_
  void s8(std::vector<int>& permutation){
    destructConstructOperator(permutation, destructionSize2_);
  }

  // # DABC 100: 40000
  // # DABC 75: 140000
  // # DABC 50: 140000
  long getMaxEvaluation(std::size_t n){
    switch(n){
    case(10):
      return 20000;
      break;
    case(25):
      return 100000;
      break;
    case(50):
      return 140000;
      break;
    case(75):
      return 150000;
      break;
    case(100):
      return 40000;
      break;
    default:
      return 10000;
    break;
    }
  }
};



//' @export
// [[Rcpp::export]]
void startDABC(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType = "intermediateBuffer",
               std::string targetCriterion = "makespan",
               unsigned int runNumber = 1, std::string fileSuffix = ""){

  // Rcpp::Rcout << "Start DABC" << std::endl;
  Rcpp::Environment myEnvironment(MYPACKAGE);
  DABCSolver dabcs(jobData, logFileName, runNumber, myEnvironment, maxBufferSize, bufferType,
                   targetCriterion, fileSuffix);

  ///////Bodeih/////////
  // Rcpp::Rcout << "run aufgerufen" << std::endl;
  dabcs.run();
  // Rcpp::Rcout << "Feddich";
}

//' @export
// [[Rcpp::export]]
void startDABCWithParameters(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType = "intermediateBuffer",
               std::string targetCriterion = "makespan",
               unsigned int runNumber = 1, std::string fileSuffix = "",
               int populationSize = 50,
               int perturbationStrength = 3,
               int destructionSize1 = 5,
               int destructionSize2 = 6,
               int numberOfInserts = 10,
               int localSearchAttempts = 10){

  // Rcpp::Rcout << "Start DABC" << std::endl;
  Rcpp::Environment myEnvironment(MYPACKAGE);
  DABCSolver dabcs(jobData, logFileName, runNumber, myEnvironment, maxBufferSize, bufferType,
                   targetCriterion, fileSuffix,
                   populationSize,
                   perturbationStrength,
                   destructionSize1,
                   destructionSize2,
                   numberOfInserts,
                   localSearchAttempts
  );

  ///////Bodeih/////////
  // Rcpp::Rcout << "run aufgerufen" << std::endl;
  dabcs.run();
  // Rcpp::Rcout << "Feddich";
}

