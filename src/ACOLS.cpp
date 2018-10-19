#include <Rcpp.h>
// #include <RcppArmadillo.h>
//
// // [Rcpp:depends(RcppArmadillo)]]

#include <algorithm>
#include <limits>
#include <iostream>
#include <fstream>
#include <chrono>
#include <deque>

#include "HelperFunctions.h"
#include "Solver.h"

#define DEBUG_ENABLED false

/*
 * To-Do:
 * -Abbruch der lokalen Suche nach 1000 Evaluationen
 */
struct ACOLSSolver : public Solver{

  bool improvementFoundInCurrentIteration_ = false;
  bool improvementFoundInPreviousIteration_ = true;

  int numberOfAnts_;
  std::vector<std::vector<double>> pheromones_;
  std::vector<std::vector<int>> currentSolutions_;
  double tauInit_;
  double delta_;
  double evaporation_;

  int lookAround_;
  double p_;
  double initialP_;

  long oldNumberOfEvaluation_;
  const long MAX_LOCAL_SEARCH_STEPS = 1000;

  std::string operationSequence_;

  std::vector<double> m1Times_;
  std::vector<double> m2Times_;
  std::vector<double> bufferUsage_;
  std::vector<int> dueTimes_;

  std::vector<int> nehSolution_;
  std::vector<int> baseSolution_;

  bool withNEH_;
  bool withACO_;
  bool withLS_;
  bool withReconstruct_;

  int tempNumber = 0;

  // Zufallszahlengenerator

  std::random_device rd;
  std::mt19937 gen;
  std::uniform_real_distribution<double> randomNumber; //Intervall [0,1)

  ACOLSSolver(Rcpp::DataFrame jobData, std::string logFileName, unsigned int runNumber, Rcpp::Environment rEnvironment,
             int maxBufferSize, std::string bufferType,
             std::string targetCriterion, std::string fileSuffix)

    : Solver(jobData, logFileName, runNumber, rEnvironment, maxBufferSize, bufferType, targetCriterion, "acols", fileSuffix),
      lookAround_(0),
      p_(0.9), initialP_(p_),
      operationSequence_("")
      {
    Rcpp::NumericVector tempVector = jobData["m1Time"];
    m1Times_ = std::vector<double>(tempVector.begin(), tempVector.end());
    tempVector = jobData["m1Time"];
    m1Times_ = std::vector<double>(tempVector.begin(), tempVector.end());
    tempVector = jobData["m2Time"];
    m2Times_ = std::vector<double>(tempVector.begin(), tempVector.end());
    tempVector = jobData["bufferUsage"];
    bufferUsage_ = std::vector<double>(tempVector.begin(), tempVector.end());

    tempVector = jobData["dueTime"];
    dueTimes_ = std::vector<int>(tempVector.begin(), tempVector.end());




    //RNG gen(std::mt19937(rd())),randomNumber(std::uniform_real_distribution<double>(0.0, 1.0))
    gen = std::mt19937(rd());
    randomNumber = std::uniform_real_distribution<double>(0.0, 1.0); //Intervall [0,1)
  }

  void run(){

    iterationNumber_ =  1;
    if (withACO_){
      initializePheromones();
    }

    std::vector<int> perm1(numberOfJobs_);
    if (withNEH_){
      Rcpp::Rcout << "NEH" << std::endl;
      std::vector<int> nehSolution_ = getNEHSolution(0);
      evaluateSolution(jobData_, nehSolution_, nehSolution_, maxBufferSize_, bufferType_, true);
      Rcpp::Rcout << "NEH fertig " << std::endl;
      perm1.assign(nehSolution_.begin(), nehSolution_.end());
    } else {
      perm1 = getRandomPermutation(numberOfJobs_, gen);
    }


    if (withReconstruct_){
      // Rcpp::Rcout << "reconstruct" << std::endl;
      // perm1 = reconstructSolution(perm1);
    }
    if (DEBUG_ENABLED){
      plotMatrixWithR(pheromones_);
      waitForInput("erste Matrix ist oben", DEBUG_ENABLED);
    }

    if (withACO_){
      Rcpp::Rcout << "update mit:" << std::endl;
      printVector(perm1);
      if (withNEH_){
        updateMatrix(perm1, 0.5);
      }
    }

    if (DEBUG_ENABLED){
      plotMatrixWithR(pheromones_);
      waitForInput("Init fertig", DEBUG_ENABLED);
    }


    do{
      int oldOptimalQuality = bestSolutionQuality_;
      improvementFoundInCurrentIteration_ = false;

      if (withACO_){
        iterationNumber_ =  1;
        Rcpp::Rcout << "generate Solutions" << std::endl;
        // konstruiere Lösungen und suche die beste
        generateSolutions();

        int bestAntIndex = 0;
        int bestAntValue = std::numeric_limits<int>::max();
        // waitForInput("Start MaxSearch", DEBUG_ENABLED);
        for (int i = 0; i < numberOfAnts_; i++){
          std::vector<int> tempSolution = currentSolutions_[i];
          int tempMakespan = evaluateSolution(
            jobData_, tempSolution,
            tempSolution, maxBufferSize_,
            bufferType_
          );
          if (tempMakespan < bestAntValue){
            bestAntValue = tempMakespan;
            bestAntIndex = i;
          }
        }
        // waitForInput("end MaxSearch", DEBUG_ENABLED);
        perm1.assign(currentSolutions_[bestAntIndex].begin(), currentSolutions_[bestAntIndex].end());
      } else {
        Rcpp::Rcout << "Take best solution" << std::endl;
        perm1.assign(bestSolution_.begin(), bestSolution_.end());
        if (!improvementFoundInPreviousIteration_){
          perturbSolution(perm1, -1);
        }
      }

      iterationNumber_ = tempNumber + 3;

      if (withLS_){
        oldNumberOfEvaluation_ = numberOfEvaluation_;
        // waitForInput("start LS", DEBUG_ENABLED);
        baseSolution_.assign(perm1.begin(), perm1.end());
        perturbSolution(baseSolution_, -1);
        // printVector(perm1);
        // printVector(baseSolution_);

        //////////// Lokale Suche //////////////
        bool lsSuccessful = true;
        Rcpp::checkUserInterrupt();

        for (char c : operationSequence_){
          int op = c - '0';
          switch(op){
            case(OP_INSERT):
              while(lsSuccessful && numberOfEvaluation_ <= oldNumberOfEvaluation_ + MAX_LOCAL_SEARCH_STEPS){
                Rcpp::Rcout << "LS: Insert " << improvementFoundInCurrentIteration_ << std::endl;
                lsSuccessful = localSearchByReferencePermutation(perm1, "insert");
              }

            break;
            case(OP_EDGEINSERT):
              while(lsSuccessful && numberOfEvaluation_ <= oldNumberOfEvaluation_ + MAX_LOCAL_SEARCH_STEPS){

                Rcpp::Rcout << "LS: EdgeInsert " << improvementFoundInCurrentIteration_ << std::endl;
                lsSuccessful = localSearchByReferencePermutation(perm1, "edgeInsert");
              }
              // Rcpp::Rcout << "LS: EdgeInsert " << improvementFoundInCurrentIteration_ << std::endl;
              break;
            case(OP_SWAP):
              while(lsSuccessful && numberOfEvaluation_ <= oldNumberOfEvaluation_ + MAX_LOCAL_SEARCH_STEPS){
                Rcpp::Rcout << "LS: Swap " << improvementFoundInCurrentIteration_ << std::endl;
                lsSuccessful = localSearchByReferencePermutation(perm1, "swap");
              }
              // Rcpp::Rcout << "LS: Swap " << improvementFoundInCurrentIteration_ << std::endl;
            break;
            default:
              Rcpp::stop("op ist unbekannt");
            break;

          }
          // iterationNumber_ *= -1;
        }
      }
      //////////// Ende lokale Suche //////////////////

      if (oldOptimalQuality > bestSolutionQuality_){
        improvementFoundInCurrentIteration_ = true;
      }
      Rcpp::Rcout << "LS vorbei und " << improvementFoundInCurrentIteration_ << std::endl;

      // wenn lokale Suche gescheitert, dann versuche eine
      // Neuzusammensetzung der Lösung
      if (!improvementFoundInCurrentIteration_ ){
        if (withReconstruct_){
          Rcpp::Rcout << "attempt reconstruct " <<  std::endl;
          std::vector<int> perm2 = reconstructSolution(perm1);
          if (improvementFoundInCurrentIteration_){
            perm1.assign(perm2.begin(), perm2.end());
            Rcpp::Rcout << "yeah " << std::endl;
            p_ = initialP_;
            //improvementFoundInCurrentIteration_ = true;
          } else {
            Rcpp::Rcout << "argh" << std::endl;
            p_ *= 1.1;
            if (p_ > 0.99){
              p_ = 0.99;
            }
          }
        } else {
          Rcpp::Rcout << "argh (ohne Reconstruct)" << std::endl;
          p_ *= 1.1;
          if (p_ > 0.99){
            p_ = 0.99;
          }
        }
      } else {
        p_ = initialP_;
      }


      if (withACO_){
        // printVector(perm1);
        if (DEBUG_ENABLED){
          Rcpp::Rcout << "update mit:" << std::endl;
          printVector(perm1);
        }
        updateMatrix(perm1, evaporation_);
      }


      improvementFoundInPreviousIteration_ = improvementFoundInCurrentIteration_;
      Rcpp::checkUserInterrupt();
      Rcpp::Rcout << "End Loop" << std::endl;
      // if (iterationNumber_ < 0){
      //   iterationNumber_--;
      // } else {
      //   iterationNumber_++;
      // }
      // Rcpp::stop("iteration end");
      tempNumber++;

      if (DEBUG_ENABLED){
        // printMatrix(pheromones_);
        plotMatrixWithR(pheromones_);
        waitForInput("pressEnter", DEBUG_ENABLED);
      }

      // if (tempNumber > 20){
      // Rcpp::stop("Endee");
      // }
    } while (!terminationCriterionSatisfied());

    writeBestSolution();

  }

  void generateSolutions(){
    currentSolutions_.clear();
    for (int entityIndex = 0; entityIndex < numberOfAnts_; entityIndex++){
      std::vector<int> currentSolution = std::vector<int>(numberOfJobs_, -1);

      //die einzelnen Komponenten
      for (std::size_t componentIndex = 0; componentIndex < numberOfJobs_ - 1; componentIndex++){
        // waitForInput("Tau Values");
        std::vector<double> tauValues = calculateTauValues(currentSolution, componentIndex, entityIndex);
        std::vector<int> unplannedJobs = std::vector<int>();

        for (std::size_t job = 1; job <= numberOfJobs_; job++){
          if (tauValues[job-1] != 0){
            unplannedJobs.push_back(job);
          }
        }
        // waitForInput("kumulative Prob");
        std::vector<double> cp = calculateCumulativeProbabilities(tauValues);



        double myRandomNumber = randomNumber(gen);

        if (DEBUG_ENABLED){
          Rcpp::Rcout << "Komponente " << componentIndex << std::endl;
          printVector(tauValues);
          printVector(unplannedJobs);
          Rcpp::Rcout << "cp: ";
          printVector(cp);
          Rcpp::Rcout << "randomNumber: " << myRandomNumber << std::endl;
        }
        for (std::size_t i = 0; i < unplannedJobs.size(); i++){
          if (myRandomNumber < cp[i]){
            currentSolution[componentIndex] = unplannedJobs[i];
            break;
          }
        }

        if (DEBUG_ENABLED){
          Rcpp::Rcout << "added: " << currentSolution[componentIndex] << std::endl;
        // waitForInput("press Enter", DEBUG_ENABLED);
        }
        if (currentSolution[componentIndex] == -1){
          printVector(tauValues);
          printVector(unplannedJobs);
          printVector(currentSolution);
          printVector(cp);
          printMatrix(pheromones_);
          Rcpp::stop("-1 steht im Vektor");
        }
      }
      // letzten job von Hand einfügen
      for (std::size_t job = 1; job <= numberOfJobs_; job++){
        if (std::find(currentSolution.begin(), currentSolution.end(), job) == currentSolution.end()){
          currentSolution[numberOfJobs_ - 1] = job;
          break;
        }
      }
      if (DEBUG_ENABLED){
        printVector(currentSolution);
        waitForInput("Vektor ist oben...", DEBUG_ENABLED);
      }
      // if (entityIndex==3){
        // Rcpp::stop("myEnd");
      // }
      currentSolutions_.push_back(currentSolution);
      // printVector(currentSolution);
      // waitForInput("pushback", DEBUG_ENABLED);
    }
    // waitForInput("ende generate", DEBUG_ENABLED);

  }
  std::vector<double> calculateCumulativeProbabilities(std::vector<double> tauValues){
    double sum = std::accumulate(tauValues.begin(), tauValues.end(), 0.0);
    std::vector<double> result = std::vector<double>();
    //result.push_back(0.0);

    double currentProbability = 0.0;
    for (std::size_t i = 0; i < tauValues.size(); i++){
      if (tauValues[i] != 0){
        double tempProbability = tauValues[i] / sum;
        currentProbability += tempProbability;
        result.push_back(currentProbability);
      }
    }
    //result.push_back(1.0);
    return result;
  }

  std::vector<double> calculateTauValues(std::vector<int> currentSolution, int componentIndex, int entityIndex){
    std::vector<double> tauValues = std::vector<double>(numberOfJobs_, 0);
    std::vector<int>::iterator myIt = std::find(currentSolution.begin(), currentSolution.end(), -1);
    std::vector<int> partialSolution = std::vector<int>(currentSolution.begin(), myIt);
    // printVector(partialSolution);
    if (partialSolution.size() > 0){
      int lastJob = partialSolution.back();
      for (int job = 1; job <= (int) numberOfJobs_; job++){ //die einzelnen Jobs
        // wenn Job schon drin, ist der Tau-Wert gleich 0
        if (std::find(partialSolution.begin(), partialSolution.end(), job) == partialSolution.end()){
          // warum -1+1? -1, weil Jobnumber zu Index und +1, weil noch die 0. Zeile da ist
          double tempValue = pheromones_[lastJob - 1 + 1][job - 1]; // (*** vorher war hier pheromones_[lastJob - 1][job - 1])
          // ### Heuristik einbeziehen
          // tempValue *= calculateHeuristicValue(entityIndex, componentIndex, job, partialSolution, oldMakespan);
          tauValues[job-1] = tempValue;
        }
      }
    } else { // es wurde vorher noch nichts geplant
      for (int job = 1; job <= (int) numberOfJobs_; job++){ //die einzelnen Jobs
        // wenn Job schon drin, ist der Tau-Wert gleich 0

        double tempValue = pheromones_[0][job-1];

        // ### Heuristik einbeziehen

        tempValue *= calculateHeuristicValue(entityIndex, componentIndex, job, partialSolution, 0);
        tauValues[job-1] = tempValue;
      }
    }


    //printVector(tauValues);
    return tauValues;
  }


  double calculateHeuristicValue(int entity, int i, int j, std::vector<int> previous, int oldMakespan){

    //  bevorzuge bald anstehende Jobs, 1/(1+(a_j - a_min)).

    double result = 1.0;
    if (targetCriterion_ == "dueTimes"){
      std::vector<int> dueTimesRemaining;
      int minDueTime = std::numeric_limits<int>::max();

      // minDueTime von den ungeplanten Jobs suchen
      for (int i = 1; i <= (int) numberOfJobs_; i++){
        if (std::find(previous.begin(), previous.end(), i) == previous.end()){
          if (dueTimes_[i-1] < minDueTime){
            minDueTime = dueTimes_[i-1];
          }
        }

      }

      result = 1.0 / (1.0 + ((double) dueTimes_[j-1] - (double) minDueTime));

      if (DEBUG_ENABLED){
        Rcpp::Rcout << "current / min: " << dueTimes_[j-1] << " / " << minDueTime << std::endl;
        Rcpp::Rcout << result <<  " " << std::pow(result, 0.1) << std::endl;
        waitForInput("...", DEBUG_ENABLED);
      }
      result = std::pow(result, 0.1);
    }

    return result;
  }


  void updateMatrix(std::vector<int> perm, double evaporation){
    for (int currentJob = 0; currentJob <= (int) numberOfJobs_; currentJob++){
      int nextJob = -1;

      if (currentJob > 0){
        // suche Job (i) in der Lösung
        for (int k = 0; k < (int) numberOfJobs_; k++){
          //nimm dann nächsten Job (falls existiert)
          if (perm[k] == currentJob && k+1 != (int) numberOfJobs_){
            nextJob = perm[k+1];
          }
        }
      } else {
        nextJob = perm[0];
      }

      if (nextJob != -1){
        // verdunstete Werte = hinzugefügte Werte
        double newDelta = 0.0;
        double newPheromone = 0.0;
        for (int j = 1; j <= (int) numberOfJobs_; j++){
          newPheromone = pheromones_[currentJob][j-1] * (1.0 - evaporation);
          if (newPheromone==0.0){
            newPheromone = pheromones_[currentJob][j-1];
          } else {
            newDelta += pheromones_[currentJob][j-1] - newPheromone;
            pheromones_[currentJob][j-1] = newPheromone;
          }
        }
        // Rcpp::Rcout << "newDelta: " << newDelta << std::endl;
        // printVector(perm);
        // Rcpp::Rcout << "currentJob: " << currentJob << std::endl;
        // Rcpp::Rcout << "nextJob: " << nextJob << std::endl;
        // waitForInput("enter", DEBUG_ENABLED);
        pheromones_[currentJob][nextJob-1] += newDelta;
      }
    }

    // printMatrix(pheromones_);
  }

  // ein Durchlauf dieser Funktion nimmt ca. N^2 Evaluationen in Anspruch
  // (N: Anzahl der Jobs)
  bool localSearchByReferencePermutation(std::vector<int>& permutation, std::string opString){
    std::size_t counter = 0;
    // std::size_t fallbackCounter = 0;
    int bestMakespan = std::numeric_limits<int>::max();
    int tempMakespan = std::numeric_limits<int>::max();
    std::size_t upperLimitForLoop = 0;
    //std::size_t bestTakePosition = 0;
    std::size_t bestOpPosition = 1;


    bool permutationHasImproved = false;


    int op = -1;

    if (opString == "insert"){
      op = OP_INSERT;
    }
    if (opString == "edgeInsert"){
      op = OP_EDGEINSERT;
    }
    if (opString == "swap"){
      op = OP_SWAP;
    }

    std::vector<int> myCopy(numberOfJobs_);
    int currentMakespan = evaluateSolution(jobData_, permutation, permutation, maxBufferSize_, bufferType_);

    for (counter = 0; counter < numberOfJobs_; counter++){
      std::size_t takePosition = 0;
      for (std::size_t i = 0; i < numberOfJobs_; i++){
        if (permutation[i] == baseSolution_[counter]){
          takePosition = i;
        }
      }
      if (op == OP_EDGEINSERT && takePosition == numberOfJobs_ - 1){
        continue;
      }

      bestMakespan = std::numeric_limits<int>::max();

      if (op == OP_EDGEINSERT){
        upperLimitForLoop = numberOfJobs_ - 1;
      } else {
        upperLimitForLoop = numberOfJobs_;
      }
      for (std::size_t opPosition = 0; opPosition < upperLimitForLoop; opPosition++){

        if (takePosition != opPosition){
          myCopy.assign(permutation.begin(), permutation.end());
          switch(op){
            case(OP_INSERT):
              insertOperator(myCopy, takePosition, opPosition);
              break;
            case(OP_EDGEINSERT):
              edgeInsertOperator(myCopy, takePosition, opPosition);
              break;
            case(OP_SWAP):
              swapOperator(myCopy, takePosition, opPosition);
              break;
            default:
              Rcpp::stop("op ist unbekannt");
              break;
          }
          tempMakespan = evaluateSolution(jobData_, myCopy, myCopy, maxBufferSize_, bufferType_);
          if (tempMakespan < bestMakespan){
            bestMakespan = tempMakespan;
            bestOpPosition = opPosition;
          }

          // vorzeitiger Abbruch der lokalen Suche
          // wenn bis dahin schon Verbesserung gefunden wurde, dann
          // überschreibe die Variable permutation

          if (numberOfEvaluation_ > oldNumberOfEvaluation_ + MAX_LOCAL_SEARCH_STEPS){
            if (bestMakespan <= currentMakespan){
              switch(op){
              case(OP_INSERT):
                insertOperator(permutation, takePosition, bestOpPosition);
                break;
              case(OP_EDGEINSERT):
                edgeInsertOperator(permutation, takePosition, bestOpPosition);
                break;
              case(OP_SWAP):
                swapOperator(permutation, takePosition, bestOpPosition);
                break;
              }
              if (bestMakespan < currentMakespan){
                permutationHasImproved = true;
              }

            }
            return permutationHasImproved;
          }
        }
      }

      if (bestMakespan <= currentMakespan){
        switch(op){
          case(OP_INSERT):
            insertOperator(permutation, takePosition, bestOpPosition);
            break;
          case(OP_EDGEINSERT):
            edgeInsertOperator(permutation, takePosition, bestOpPosition);
            break;
          case(OP_SWAP):
            swapOperator(permutation, takePosition, bestOpPosition);
            break;
        }
        if (bestMakespan < currentMakespan){
          permutationHasImproved = true;
        }
        currentMakespan = bestMakespan;
      }
    }
    return permutationHasImproved;
  }

  void perturbSolution(std::vector<int>& perm, int forbiddenJob){
    double myRandomNumber = randomNumber(gen);
    double whichOperation = 0.0;

    if (perm.size() == 1){
      while (myRandomNumber < p_){
        replaceOperator(perm, getRandomNumber(0, perm.size()-1), numberOfJobs_, forbiddenJob);
        myRandomNumber = randomNumber(gen);
      }
      return;
    }

    while (myRandomNumber < p_){ //es ist möglich, dass gar keine Perturbation passiert
      whichOperation = randomNumber(gen);
      myRandomNumber = randomNumber(gen);

      if (whichOperation < 0.25){
        insertOperator(perm, getRandomNumber(0, perm.size()-1), getRandomNumber(0, perm.size()-1));
      } else {
        if (whichOperation < 0.50){
          swapOperator(perm, getRandomNumber(0, perm.size()-1), getRandomNumber(0, perm.size()-1));
        } else { // [0.33,0.66)
          if (whichOperation < 0.75){
            if (perm.size() > 2){
              edgeInsertOperator(perm, getRandomNumber(0, perm.size()-2), getRandomNumber(0, perm.size()-2));
            } else {
              myRandomNumber = 0.0;
            }
          } else {
            if (perm.size() < numberOfJobs_ - 1){
              replaceOperator(perm, getRandomNumber(0, perm.size()-1), numberOfJobs_, forbiddenJob);
            } else {
              myRandomNumber = 0.0;
            }
          }
        }
      }
    }
  }

  double bestInsertion(std::vector<int>& perm1, std::vector<int>& perm2, int val){
    int currentMakespan = getSolutionQuality(jobData_, perm1, perm2, maxBufferSize_, bufferType_, targetCriterion_);
    numberOfEvaluation_++;

    std::vector<int> perm1Old(perm1.begin(), perm1.end());
    std::vector<int> perm2Old(perm2.begin(), perm2.end());

    double minValue = std::numeric_limits<double>::max();
    int bestInsertPos = 0;
    int bestInternalPos = 0;

    // wenn ein Vektor k Elemente hat, gibt es k+1 Einfügepositionen
    for (int insertPos = 0; insertPos < (int) perm1.size() + 1; insertPos++){
      insertJob(perm1, insertPos, val);

      for (int internalPos = insertPos - lookAround_; internalPos < insertPos + lookAround_ + 1; internalPos++){

        bool insertionSuccessful = insertJob(perm2, internalPos, val);
        // Rcpp::Rcout << "(" << insertPos << "," << internalPos << ")" << std::endl;
        if (insertionSuccessful){
          //In der nächsten Zeile wird numberOfEvaluation_ hochgezählt, Dummerchen!
          //int newMakespan = getSolutionQuality(jobData_, perm1, perm2, maxBufferSize_, bufferType_, false);
          int newMakespan = evaluateSolution(jobData_, perm1, perm2, maxBufferSize_, bufferType_, false);

          //numberOfEvaluation_++;
          if (newMakespan > 0){ //falls gültiger Plan

            double value = (newMakespan - currentMakespan) + 0;
            if (value < minValue){
              minValue = value;
              bestInsertPos = insertPos;
              bestInternalPos = internalPos;
            }
            // if (perm1.size() == numberOfJobs_ && perm2.size() == numberOfJobs_ && newMakespan < baseSolutionQuality_){
            //   baseSolution_.assign(perm1.begin(), perm1.end());
            // }
          }
        }
        perm2.assign(perm2Old.begin(), perm2Old.end());
      }
      perm1.assign(perm1Old.begin(), perm1Old.end());
    }
    // Rcpp::Rcout << "finisshu";
    insertJob(perm1, bestInsertPos, val);
    insertJob(perm2, bestInternalPos, val);

    return minValue;
  }

  std::vector<int> reconstructSolution(std::vector<int> solution){
    //printVector(solution);
    iterationNumber_ = tempNumber + 2;
    // bool improvementFound = false; //Variable wird nicht mehr benötigt, glaub ich
    std::vector<int> perm1;
    std::vector<int> perm2;
    perm1.push_back(solution[0]);
    perm2.push_back(solution[0]);
    for (std::size_t currentJob = 1; currentJob < numberOfJobs_; currentJob++){
      bestInsertion(perm1, perm2, solution[currentJob]);
    }

    // int oldQuality = bestSolutionQuality_;
    int resultingQuality = evaluateSolution(jobData_, perm1, perm2, maxBufferSize_, bufferType_);

    // if (resultingQuality < oldQuality){
    //   improvementFound = true;
    // }
   // iterationNumber_ *= -1;
    return perm1;

  }
  bool insertJob(std::vector<int>& perm, int pos, int value){
    if ((std::size_t) pos == perm.size()){
      //einfügen am Ende
      perm.push_back(value);
    } else {
      if (pos < 0 || (std::size_t) pos > perm.size()){
        return false;
      }

      perm.insert(perm.begin() + pos, value);
    }
    return true;
  }

  void initializePheromones(){
    // Matrix initialisieren
    // (i,j): nach Job i kommt Job j
    // i: 0...N, j: 1...N
    pheromones_ = std::vector<std::vector<double>>();
    for (int i = 0; i <= (int) numberOfJobs_; i++){
      std::vector<double> inits = std::vector<double>(numberOfJobs_, tauInit_);
      pheromones_.push_back(inits);
    }
  }

  std::size_t newNumberOfFeatures(std::size_t length){
    return 2 * length + 1;
  }

  // bool terminationCriterionSatisfied() override{
  //   return tempNumber > 100;
  // }
  int evaluateSolution(Rcpp::DataFrame jobData,
                               const std::vector<int>& permutationM1,
                               const std::vector<int>& permutationM2,
                               int maxBufferSize,
                               std::string bufferType,
                               bool forceLogging = false,
                               bool abortOnInvalidity = true,
                               bool m2Special = true) override{

    int oldOptimalMakespan = bestSolutionQuality_;
    int newMakespan = getAndLogSolutionQuality(jobData, permutationM1, permutationM2, maxBufferSize, bufferType,
                                               targetCriterion_,
                                          logFile_, startTime_, numberOfEvaluation_, iterationNumber_,
                                          bestSolutionQuality_, bestSolution_, bestSolutionM2_,
                                          forceLogging, abortOnInvalidity, m2Special);


    // printVector(permutationM1);

    if (newMakespan < oldOptimalMakespan && permutationM1.size() == numberOfJobs_){
      improvementFoundInCurrentIteration_ = true;
    }

    return newMakespan;
  }

  void printMatrix(std::vector<std::vector<double>> m, bool binary = false){
    if (binary){
      for (std::vector<double> v : m){
        std::vector<double>::iterator maxIt = std::max_element(v.begin(), v.end());
        std::vector<double> binaryVector;
        for (double d : v){
          binaryVector.push_back(d == *maxIt ? 1.0 : 0.0);
        }
        printVector(binaryVector);
      }
    }else{

      for (std::vector<double> v : m){
        printVector(v);
      }
    }

  }

  void plotMatrixWithR(std::vector<std::vector<double>> m){
    Rcpp::NumericMatrix valuesToPlot(numberOfJobs_+1, numberOfJobs_);
    for (std::size_t row = 0; row <= numberOfJobs_; row++){
      for (std::size_t col = 0; col < numberOfJobs_; col++){
        valuesToPlot(row,col) = pheromones_[row][col];
      }
    }
    Rcpp::Function plotMatrixFunction = rEnvironment_["plotMatrix"];
    plotMatrixFunction(valuesToPlot);
  }
  ~ACOLSSolver(){
    //delete [] coefficientMatrix_;
  }

};

//' @export
// [[Rcpp::export]]

void startACOLS(Rcpp::DataFrame jobData, std::string logFileName,
              int maxBufferSize, std::string bufferType = "intermediateBuffer",
              std::string targetCriterion = "makespan",
              unsigned int runNumber = 1, std::string fileSuffix = "",
              int lookAround = 0, double p = 0.9,
              std::string operationSequence = "213",
              int numberOfAnts = 10, double tauInit = 1,
              double evaporation = 0.3, double delta = 0.5,
              bool withACO = true,
              bool withLS = true,
              bool withReconstruct = true){
  Rcpp::Rcout << "Start ACOLS" << std::endl;
  Rcpp::Environment myEnvironment(MYPACKAGE);
  ACOLSSolver acolss(jobData, logFileName, runNumber,
                     myEnvironment, maxBufferSize, bufferType,
                     targetCriterion, fileSuffix);

  // Parameter setzen
  acolss.lookAround_ = lookAround;
  acolss.p_ = p;
  acolss.operationSequence_ = operationSequence;

  acolss.numberOfAnts_ = numberOfAnts;
  acolss.tauInit_ = 1/(double) acolss.numberOfJobs_;
  acolss.delta_ = delta;
  acolss.evaporation_ = evaporation;

  acolss.withACO_ = true;
  acolss.withLS_ = withLS;
  acolss.withReconstruct_ = withReconstruct;

  ///////Bodeih/////////

  Rcpp::Rcout << "run aufgerufen" << std::endl;
  acolss.run();
  Rcpp::Rcout << "Feddich" << std::endl;
}


