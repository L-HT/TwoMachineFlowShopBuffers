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

// #define OP_INSERT 1
// #define OP_EDGEINSERT 2
// #define OP_SWAP 3
#define DEBUG_ENABLED false

struct ILSSolver : public Solver{

  bool improvementFoundInCurrentIteration_ = false;
  bool improvementFoundInPreviousIteration_ = true;

  int numberOfAnts_;
  std::vector<std::vector<double>> pheromones_;
  std::vector<std::vector<int>> currentSolutions_;
  double tauInit_;
  double delta_;
  double evaporation_;

  int lookAround_;

  double initialP_;
  double p_;

  std::string operationSequence_;

  std::vector<double> m1Times_;
  std::vector<double> m2Times_;
  std::vector<double> bufferUsage_;

  std::vector<int> nehSolution_;
  std::vector<int> baseSolution_;


  bool withNEH_;
  bool withACO_;
  bool withLS_;
  bool withInitialReconstruct_;
  bool withReconstruct_;
  bool useRandomPermutationForLS_;
  bool useRandomPermutationForReconstruct_;

  int tempNumber = 0;

  // Zufallszahlengenerator

  std::random_device rd;
  std::mt19937 gen;
  std::uniform_real_distribution<double> randomNumber; //Intervall [0,1)

  ILSSolver(Rcpp::DataFrame jobData, std::string logFileName, unsigned int runNumber, Rcpp::Environment rEnvironment,
             int maxBufferSize, std::string bufferType,
             std::string targetCriterion, std::string fileSuffix)

    : Solver(jobData, logFileName, runNumber, rEnvironment, maxBufferSize, bufferType,
      targetCriterion, "ils", fileSuffix),
      lookAround_(0),
      initialP_(0.525),p_(initialP_),
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



    //RNG gen(std::mt19937(rd())),randomNumber(std::uniform_real_distribution<double>(0.0, 1.0))
    gen = std::mt19937(rd());
    randomNumber = std::uniform_real_distribution<double>(0.0, 1.0); //Intervall [0,1)
  }

  void run(){

    /*
     * x = NEH
       if (AVI){
          x = reconstruct(x)
          if (ACO){
            updateMatrix(x)
          }
        }

    while(){
      if (ACO){
        generateSolutions
        xBest = findBestSolution (wenn das erste Mal: random)
      } else {
        xBest = besteBekannteLösung (wenn das erste Mal: neh)
        if (noPreviousImprovement){
          xBest = perturb(xBest, p)
        }
      }

      if (LS){
        refPerm = perturb(xBest, p)
        LS(xBest, refPerm)
      }
      if (noImprovement && AVI){
        reconstruct(xBest)
        if (noImprovement){
          increaseP
        }
      } else {
        resetP
      }
      if (ACO){
        updateMatrix(xBest)
      }
    }

     Phasen: 1: Generierung
     2: Reconstruct, 3: LS
     */


    iterationNumber_ =  1;
    if (withACO_){
      initializePheromones();
    }
    waitForInput("startNEH", DEBUG_ENABLED);


    std::vector<int> perm1(numberOfJobs_);
    if (withNEH_){
      Rcpp::Rcout << "NEH" << std::endl;


      // hier das modifizierte NEH
      Rcpp::Function getNEHSolutionMod = rEnvironment_["getNEHSolutionMod"];
      //std::vector<int> nehSolution_ = getNEHSolution(0);
      std::vector<int> nehSolution_ = Rcpp::as<std::vector<int>>(getNEHSolutionMod(jobData_, bufferType_, maxBufferSize_, 0, targetCriterion_));
      Rcpp::Rcout << "NEH-Evaluations hinzugefügt: " << nehSolution_.back();
      numberOfEvaluation_ += nehSolution_.back();
      nehSolution_.pop_back();

      evaluateSolution(jobData_, nehSolution_, nehSolution_, maxBufferSize_, bufferType_, true);
      // Rcpp::Rcout << "NEH fertig " << std::endl;
      perm1.assign(nehSolution_.begin(), nehSolution_.end());
    } else {
      perm1 = getRandomPermutation(numberOfJobs_, gen);
      // printVector(perm1);
      evaluateSolution(jobData_, perm1, perm1, maxBufferSize_, bufferType_, true);
    }

    waitForInput("finished NEH", DEBUG_ENABLED);


    if (withInitialReconstruct_){
      Rcpp::Rcout << "reconstruct" << std::endl;
      //perm1 = reconstructSolution(perm1);
      perm1 = extendedReconstruct(perm1);
    }
    if (withACO_){
      Rcpp::Rcout << "update" << std::endl;
      updateMatrix(perm1);
    }
    // printVector(perm1);
    // printVector(bestSolution_);
    // printMatrix(pheromones_);
    // Rcpp::stop("before do end");

    // Rcpp::Rcout << p_ << std::endl;
    // waitForInput(".....", DEBUG_ENABLED);
    // waitForInput("Init fertig", DEBUG_ENABLED);
    do{
      int oldOptimalQuality = bestSolutionQuality_;
      improvementFoundInCurrentIteration_ = false;
      // Rcpp::Rcout << p_ << std::endl;
      // waitForInput(".", DEBUG_ENABLED);
      if (withACO_){
        iterationNumber_ =  1;
        Rcpp::Rcout << "generate Solutions" << std::endl;
        // konstruiere Lösungen und suche die beste
        generateSolutions();

        int bestAntIndex = 0;
        int bestAntValue = std::numeric_limits<int>::max();
        waitForInput("Start MaxSearch", DEBUG_ENABLED);
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
        waitForInput("end MaxSearch", DEBUG_ENABLED);
        perm1.assign(currentSolutions_[bestAntIndex].begin(), currentSolutions_[bestAntIndex].end());
      } else {
        Rcpp::Rcout << "Take best solution" << std::endl;
        perm1.assign(bestSolution_.begin(), bestSolution_.end());
        if (!improvementFoundInPreviousIteration_){
          perturbSolution(perm1, -1);
        }
      }

      iterationNumber_ = tempNumber + 3;
      // Rcpp::Rcout << p_ << std::endl;
      // waitForInput(".", DEBUG_ENABLED);
      if (withLS_){
        waitForInput("start LS", DEBUG_ENABLED);
        if (useRandomPermutationForLS_){
          Rcpp::Rcout << "RandomPerm" << std::endl;
          std::vector<int> tempPermutation = getRandomPermutation(numberOfJobs_, gen);
          baseSolution_.assign(tempPermutation.begin(), tempPermutation.end());
        } else {
          Rcpp::Rcout << "RefPerm" << std::endl;
          baseSolution_.assign(perm1.begin(), perm1.end());
          perturbSolution(baseSolution_, -1);
        }


        // waitForInput("before check opSequence", DEBUG_ENABLED);

        //////////// Lokale Suche //////////////
        bool lsSuccessful = false;
        Rcpp::checkUserInterrupt();

        for (char c : operationSequence_){
          int op = c - '0';
          switch(op){
            case(OP_INSERT):
              do{
            Rcpp::Rcout << "LS: Insert " << improvementFoundInCurrentIteration_ << std::endl;
                lsSuccessful = localSearchByReferencePermutation(perm1, "insert");
              } while (lsSuccessful && !terminationCriterionSatisfied());

            break;
            case(OP_EDGEINSERT):
              do{
                Rcpp::Rcout << "LS: EdgeInsert " << improvementFoundInCurrentIteration_ << std::endl;
                lsSuccessful = localSearchByReferencePermutation(perm1, "edgeInsert");
              } while (lsSuccessful && !terminationCriterionSatisfied());
              // Rcpp::Rcout << "LS: EdgeInsert " << improvementFoundInCurrentIteration_ << std::endl;
              break;
            case(OP_SWAP):
              do{
                Rcpp::Rcout << "LS: Swap " << improvementFoundInCurrentIteration_ << std::endl;

                lsSuccessful = localSearchByReferencePermutation(perm1, "swap");
              } while (lsSuccessful && !terminationCriterionSatisfied());
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
      // Rcpp::Rcout << p_ << std::endl;
      // waitForInput("...", DEBUG_ENABLED);
      if (oldOptimalQuality > bestSolutionQuality_){
        improvementFoundInCurrentIteration_ = true;
      }
      Rcpp::Rcout << "LS vorbei und " << improvementFoundInCurrentIteration_ << std::endl;

      // wenn lokale Suche gescheitert, dann versuche eine
      // Neuzusammensetzung der Lösung
      if (!improvementFoundInCurrentIteration_ ){
        if (withReconstruct_){
          Rcpp::Rcout << "attempt reconstruct " <<  std::endl;
          if (useRandomPermutationForReconstruct_){
            perm1 = getRandomPermutation(numberOfJobs_, gen);
            printVector(perm1);
          }
          std::vector<int> perm2 = reconstructSolution(perm1);
          if (improvementFoundInCurrentIteration_){
            perm1.assign(perm2.begin(), perm2.end());
            Rcpp::Rcout << "yeah " << std::endl;
            p_ = initialP_;
            //improvementFoundInCurrentIteration_ = true;
          } else {
            Rcpp::Rcout << "argh" << std::endl;
            p_ += 0.05; //*= 1.1
            if (p_ > 0.99){
              p_ = 0.99;
            }
          }
        } else {
          Rcpp::Rcout << "argh (ohne Reconstruct)" << std::endl;
          p_ += 0.05; //*= 1.1
          if (p_ > 0.99){
            p_ = 0.99;
          }
        }
      } else {
        p_ = initialP_;
      }


      if (withACO_){
        Rcpp::Rcout << "update Matrix" << std::endl;
        // printVector(perm1);
        updateMatrix(perm1);
      }

      // Rcpp::Rcout << "End Loop" << std::endl;
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

        // Rcpp::Rcout << "Komponente " << componentIndex << std::endl;
        // printVector(tauValues);
        // printVector(unplannedJobs);
        // Rcpp::Rcout << "cp: ";
        // printVector(cp);

        double myRandomNumber = randomNumber(gen);
        for (std::size_t i = 0; i < unplannedJobs.size(); i++){
          if (myRandomNumber < cp[i]){
            currentSolution[componentIndex] = unplannedJobs[i];
            break;
          }
        }
        if (currentSolution[componentIndex] == -1){
          printVector(tauValues);
          printVector(unplannedJobs);
          printVector(currentSolution);
          printVector(cp);
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
      // printVector(currentSolution);
      // if (entityIndex==3){
        // Rcpp::stop("myEnd");
      // }
      currentSolutions_.push_back(currentSolution);
      // printVector(currentSolution);
      waitForInput("pushback", DEBUG_ENABLED);
    }
    waitForInput("ende generate", DEBUG_ENABLED);

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
          double tempValue = pheromones_[lastJob - 1][job - 1];
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
        // tempValue *= calculateHeuristicValue(entityIndex, componentIndex, job, partialSolution, 0);
        tauValues[job-1] = tempValue;
      }
    }


    //printVector(tauValues);
    return tauValues;
  }

  void updateMatrix(std::vector<int> perm){
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

      // verdunstete Werte = hinzugefügte Werte
      double newDelta = 0.0;
      double newPheromone = 0.0;
      for (int j = 1; j <= (int) numberOfJobs_; j++){
        newPheromone = pheromones_[currentJob][j-1] * (1.0 - evaporation_);
        newDelta += pheromones_[currentJob][j-1] - newPheromone;
        pheromones_[currentJob][j+1] = newPheromone;
      }
      // Rcpp::Rcout << "newDelta: " << newDelta << std::endl;
      pheromones_[currentJob][nextJob-1] += newDelta;

    }

    // printMatrix(pheromones_);
  }

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

    // printVector(permutation);
    waitForInput("Before for-loop", DEBUG_ENABLED);
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

    int counter = 0;
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
      counter++;
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
    Rcpp::Rcout << "perturbed: " << counter << " with p=" << p_ << std::endl;
  }

  double bestInsertion(std::vector<int>& perm1, std::vector<int>& perm2, int val){
    int currentMakespan = evaluateSolution(jobData_, perm1, perm2, maxBufferSize_, bufferType_);
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
    // int resultingQuality = evaluateSolution(jobData_, perm1, perm2, maxBufferSize_, bufferType_);

    // if (resultingQuality < oldQuality){
    //   improvementFound = true;
    // }
   // iterationNumber_ *= -1;
    return perm1;
  }

  //2 Jobs werden zu einer Zeit eingeplant
  //Verallgemeinerung auf k: Man müsste irgendwie alle Permutationen von 1..k generieren können
  std::vector<int> extendedReconstruct(std::vector<int> solution){
    // int lookahead = 2;
    // int bestValue = 0;
    iterationNumber_ = tempNumber - 2;
    std::vector<int> result;


    std::vector<int> perm1;
    std::vector<int> perm2;
    std::vector<int> tempPerm;

    long oldNumberOfEvaluations = numberOfEvaluation_;

    //am Anfang reicht einfacher Check aus
    result.push_back(solution[0]);
    tempPerm.push_back(solution[0]);
    bestInsertion(result, tempPerm, solution[1]);

    // printVector(perm1);
    // printVector(perm2);
    // printVector(result);
    // waitForInput("for-loop", DEBUG_ENABLED);
    for (int i = 1; i < (int) numberOfJobs_/2; i++){

      //zwei Jobs einplanbar
      if (2*i + 1 < (int) numberOfJobs_){
        int firstJob = solution[2*i];
        int secondJob = solution[2*i + 1];

        int firstValue = 0;
        int secondValue = 0;

        perm1.clear();
        perm2.clear();
        tempPerm.clear();
        perm1.assign(result.begin(), result.end());
        perm2.assign(result.begin(), result.end());
        tempPerm.assign(result.begin(), result.end());

        bestInsertion(perm1, tempPerm, firstJob);
        bestInsertion(perm1, tempPerm, secondJob);
        firstValue = evaluateSolution(jobData_, perm1, perm1, maxBufferSize_, bufferType_);

        tempPerm.clear();
        tempPerm.assign(result.begin(), result.end());
        bestInsertion(perm2, tempPerm, secondJob);
        bestInsertion(perm2, tempPerm, firstJob);
        secondValue = evaluateSolution(jobData_, perm2, perm2, maxBufferSize_, bufferType_);

        if (firstValue < secondValue){
          result.assign(perm1.begin(), perm1.end());
        } else {
          result.assign(perm2.begin(), perm2.end());
        }

        // printVector(perm1);
        // printVector(perm2);
        // printVector(result);
        // waitForInput("assigned", DEBUG_ENABLED);

      } else {

        //ein Job einplanbar
        if (2*i < (int)  numberOfJobs_){
          bestInsertion(result, result, solution[2*i]);
        } else {
          Rcpp::stop("extendedReconstruct: Index überschritten");
        }
      }

    }
    //int resultingQuality = evaluateSolution(jobData_, result, result, maxBufferSize_, bufferType_);

    Rcpp::Rcout << "extended reconstruct evaluated: " << numberOfEvaluation_ - oldNumberOfEvaluations << std::endl;
    return result;
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

  void printMatrix(std::vector<std::vector<double>> m){
    for (std::vector<double> v : m){
      printVector(v);
    }
  }

  ~ILSSolver(){
    //delete [] coefficientMatrix_;
  }

};

//' @export
// [[Rcpp::export]]

void startILS(Rcpp::DataFrame jobData, std::string logFileName,
              int maxBufferSize, std::string bufferType = "intermediateBuffer",
              std::string targetCriterion = "makespan",
              unsigned int runNumber = 1, std::string fileSuffix = "",
              int lookAround = 0, double p = 0.9,
              std::string operationSequence = "213",
              int numberOfAnts = 10, double tauInit = 1,
              double evaporation = 0.3, double delta = 0.5,
              bool withACO = false,
              bool withLS = true,
              bool withReconstruct = true,
              bool useRandomPermutationForLS = false,
              bool useRandomPermutationForReconstruct = false,
              bool withInitialReconstruct = false,
              bool withNEH = true){
  Rcpp::Rcout << "Start ils" << std::endl;
  Rcpp::Environment myEnvironment(MYPACKAGE);
  ILSSolver ilss(jobData, logFileName, runNumber, myEnvironment, maxBufferSize, bufferType,
                 targetCriterion, fileSuffix);

  // Parameter setzen
  ilss.lookAround_ = lookAround;
  ilss.initialP_ = p;
  ilss.p_ = p;
  ilss.operationSequence_ = operationSequence;

  ilss.numberOfAnts_ = numberOfAnts;
  ilss.tauInit_ = tauInit;
  ilss.delta_ = delta;
  ilss.evaporation_ = evaporation;

  ilss.withACO_ = false;
  ilss.withLS_ = withLS;
  ilss.withReconstruct_ = withReconstruct;
  ilss.withNEH_ = withNEH;
  ilss.useRandomPermutationForReconstruct_ = useRandomPermutationForReconstruct;
  ilss.useRandomPermutationForLS_ = useRandomPermutationForLS;
  ilss.withInitialReconstruct_ = withInitialReconstruct;

  ///////Bodeih/////////

  Rcpp::Rcout << "run aufgerufen" << std::endl;
  Rcpp::Rcout << ilss.initialP_ << " . " << ilss.p_ << std::endl;
  ilss.run();
  Rcpp::Rcout << "Feddich" << std::endl;
}

