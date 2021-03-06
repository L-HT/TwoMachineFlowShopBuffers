// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// startACOLS
void startACOLS(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType, std::string targetCriterion, unsigned int runNumber, std::string fileSuffix, int lookAround, double p, std::string operationSequence, int numberOfAnts, double tauInit, double evaporation, double delta, bool withACO, bool withLS, bool withReconstruct);
RcppExport SEXP _TwoMachineFlowShopBuffers_startACOLS(SEXP jobDataSEXP, SEXP logFileNameSEXP, SEXP maxBufferSizeSEXP, SEXP bufferTypeSEXP, SEXP targetCriterionSEXP, SEXP runNumberSEXP, SEXP fileSuffixSEXP, SEXP lookAroundSEXP, SEXP pSEXP, SEXP operationSequenceSEXP, SEXP numberOfAntsSEXP, SEXP tauInitSEXP, SEXP evaporationSEXP, SEXP deltaSEXP, SEXP withACOSEXP, SEXP withLSSEXP, SEXP withReconstructSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobData(jobDataSEXP);
    Rcpp::traits::input_parameter< std::string >::type logFileName(logFileNameSEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type bufferType(bufferTypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type targetCriterion(targetCriterionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type runNumber(runNumberSEXP);
    Rcpp::traits::input_parameter< std::string >::type fileSuffix(fileSuffixSEXP);
    Rcpp::traits::input_parameter< int >::type lookAround(lookAroundSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< std::string >::type operationSequence(operationSequenceSEXP);
    Rcpp::traits::input_parameter< int >::type numberOfAnts(numberOfAntsSEXP);
    Rcpp::traits::input_parameter< double >::type tauInit(tauInitSEXP);
    Rcpp::traits::input_parameter< double >::type evaporation(evaporationSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< bool >::type withACO(withACOSEXP);
    Rcpp::traits::input_parameter< bool >::type withLS(withLSSEXP);
    Rcpp::traits::input_parameter< bool >::type withReconstruct(withReconstructSEXP);
    startACOLS(jobData, logFileName, maxBufferSize, bufferType, targetCriterion, runNumber, fileSuffix, lookAround, p, operationSequence, numberOfAnts, tauInit, evaporation, delta, withACO, withLS, withReconstruct);
    return R_NilValue;
END_RCPP
}
// startDABC
void startDABC(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType, std::string targetCriterion, unsigned int runNumber, std::string fileSuffix);
RcppExport SEXP _TwoMachineFlowShopBuffers_startDABC(SEXP jobDataSEXP, SEXP logFileNameSEXP, SEXP maxBufferSizeSEXP, SEXP bufferTypeSEXP, SEXP targetCriterionSEXP, SEXP runNumberSEXP, SEXP fileSuffixSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobData(jobDataSEXP);
    Rcpp::traits::input_parameter< std::string >::type logFileName(logFileNameSEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type bufferType(bufferTypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type targetCriterion(targetCriterionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type runNumber(runNumberSEXP);
    Rcpp::traits::input_parameter< std::string >::type fileSuffix(fileSuffixSEXP);
    startDABC(jobData, logFileName, maxBufferSize, bufferType, targetCriterion, runNumber, fileSuffix);
    return R_NilValue;
END_RCPP
}
// startDABCWithParameters
void startDABCWithParameters(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType, std::string targetCriterion, unsigned int runNumber, std::string fileSuffix, int populationSize, int perturbationStrength, int destructionSize1, int destructionSize2, int numberOfInserts, int localSearchAttempts);
RcppExport SEXP _TwoMachineFlowShopBuffers_startDABCWithParameters(SEXP jobDataSEXP, SEXP logFileNameSEXP, SEXP maxBufferSizeSEXP, SEXP bufferTypeSEXP, SEXP targetCriterionSEXP, SEXP runNumberSEXP, SEXP fileSuffixSEXP, SEXP populationSizeSEXP, SEXP perturbationStrengthSEXP, SEXP destructionSize1SEXP, SEXP destructionSize2SEXP, SEXP numberOfInsertsSEXP, SEXP localSearchAttemptsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobData(jobDataSEXP);
    Rcpp::traits::input_parameter< std::string >::type logFileName(logFileNameSEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type bufferType(bufferTypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type targetCriterion(targetCriterionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type runNumber(runNumberSEXP);
    Rcpp::traits::input_parameter< std::string >::type fileSuffix(fileSuffixSEXP);
    Rcpp::traits::input_parameter< int >::type populationSize(populationSizeSEXP);
    Rcpp::traits::input_parameter< int >::type perturbationStrength(perturbationStrengthSEXP);
    Rcpp::traits::input_parameter< int >::type destructionSize1(destructionSize1SEXP);
    Rcpp::traits::input_parameter< int >::type destructionSize2(destructionSize2SEXP);
    Rcpp::traits::input_parameter< int >::type numberOfInserts(numberOfInsertsSEXP);
    Rcpp::traits::input_parameter< int >::type localSearchAttempts(localSearchAttemptsSEXP);
    startDABCWithParameters(jobData, logFileName, maxBufferSize, bufferType, targetCriterion, runNumber, fileSuffix, populationSize, perturbationStrength, destructionSize1, destructionSize2, numberOfInserts, localSearchAttempts);
    return R_NilValue;
END_RCPP
}
// startHVNS
void startHVNS(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType, std::string targetCriterion, unsigned int runNumber, std::string fileSuffix);
RcppExport SEXP _TwoMachineFlowShopBuffers_startHVNS(SEXP jobDataSEXP, SEXP logFileNameSEXP, SEXP maxBufferSizeSEXP, SEXP bufferTypeSEXP, SEXP targetCriterionSEXP, SEXP runNumberSEXP, SEXP fileSuffixSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobData(jobDataSEXP);
    Rcpp::traits::input_parameter< std::string >::type logFileName(logFileNameSEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type bufferType(bufferTypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type targetCriterion(targetCriterionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type runNumber(runNumberSEXP);
    Rcpp::traits::input_parameter< std::string >::type fileSuffix(fileSuffixSEXP);
    startHVNS(jobData, logFileName, maxBufferSize, bufferType, targetCriterion, runNumber, fileSuffix);
    return R_NilValue;
END_RCPP
}
// startHVNSWithParams
void startHVNSWithParams(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType, std::string targetCriterion, unsigned int runNumber, std::string fileSuffix, int nIter, double endTemperatureFactor);
RcppExport SEXP _TwoMachineFlowShopBuffers_startHVNSWithParams(SEXP jobDataSEXP, SEXP logFileNameSEXP, SEXP maxBufferSizeSEXP, SEXP bufferTypeSEXP, SEXP targetCriterionSEXP, SEXP runNumberSEXP, SEXP fileSuffixSEXP, SEXP nIterSEXP, SEXP endTemperatureFactorSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobData(jobDataSEXP);
    Rcpp::traits::input_parameter< std::string >::type logFileName(logFileNameSEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type bufferType(bufferTypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type targetCriterion(targetCriterionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type runNumber(runNumberSEXP);
    Rcpp::traits::input_parameter< std::string >::type fileSuffix(fileSuffixSEXP);
    Rcpp::traits::input_parameter< int >::type nIter(nIterSEXP);
    Rcpp::traits::input_parameter< double >::type endTemperatureFactor(endTemperatureFactorSEXP);
    startHVNSWithParams(jobData, logFileName, maxBufferSize, bufferType, targetCriterion, runNumber, fileSuffix, nIter, endTemperatureFactor);
    return R_NilValue;
END_RCPP
}
// startILS
void startILS(Rcpp::DataFrame jobData, std::string logFileName, int maxBufferSize, std::string bufferType, std::string targetCriterion, unsigned int runNumber, std::string fileSuffix, int lookAround, double p, std::string operationSequence, int numberOfAnts, double tauInit, double evaporation, double delta, bool withACO, bool withLS, bool withReconstruct, bool useRandomPermutationForLS, bool useRandomPermutationForReconstruct, bool withInitialReconstruct, bool withNEH);
RcppExport SEXP _TwoMachineFlowShopBuffers_startILS(SEXP jobDataSEXP, SEXP logFileNameSEXP, SEXP maxBufferSizeSEXP, SEXP bufferTypeSEXP, SEXP targetCriterionSEXP, SEXP runNumberSEXP, SEXP fileSuffixSEXP, SEXP lookAroundSEXP, SEXP pSEXP, SEXP operationSequenceSEXP, SEXP numberOfAntsSEXP, SEXP tauInitSEXP, SEXP evaporationSEXP, SEXP deltaSEXP, SEXP withACOSEXP, SEXP withLSSEXP, SEXP withReconstructSEXP, SEXP useRandomPermutationForLSSEXP, SEXP useRandomPermutationForReconstructSEXP, SEXP withInitialReconstructSEXP, SEXP withNEHSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobData(jobDataSEXP);
    Rcpp::traits::input_parameter< std::string >::type logFileName(logFileNameSEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type bufferType(bufferTypeSEXP);
    Rcpp::traits::input_parameter< std::string >::type targetCriterion(targetCriterionSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type runNumber(runNumberSEXP);
    Rcpp::traits::input_parameter< std::string >::type fileSuffix(fileSuffixSEXP);
    Rcpp::traits::input_parameter< int >::type lookAround(lookAroundSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< std::string >::type operationSequence(operationSequenceSEXP);
    Rcpp::traits::input_parameter< int >::type numberOfAnts(numberOfAntsSEXP);
    Rcpp::traits::input_parameter< double >::type tauInit(tauInitSEXP);
    Rcpp::traits::input_parameter< double >::type evaporation(evaporationSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< bool >::type withACO(withACOSEXP);
    Rcpp::traits::input_parameter< bool >::type withLS(withLSSEXP);
    Rcpp::traits::input_parameter< bool >::type withReconstruct(withReconstructSEXP);
    Rcpp::traits::input_parameter< bool >::type useRandomPermutationForLS(useRandomPermutationForLSSEXP);
    Rcpp::traits::input_parameter< bool >::type useRandomPermutationForReconstruct(useRandomPermutationForReconstructSEXP);
    Rcpp::traits::input_parameter< bool >::type withInitialReconstruct(withInitialReconstructSEXP);
    Rcpp::traits::input_parameter< bool >::type withNEH(withNEHSEXP);
    startILS(jobData, logFileName, maxBufferSize, bufferType, targetCriterion, runNumber, fileSuffix, lookAround, p, operationSequence, numberOfAnts, tauInit, evaporation, delta, withACO, withLS, withReconstruct, useRandomPermutationForLS, useRandomPermutationForReconstruct, withInitialReconstruct, withNEH);
    return R_NilValue;
END_RCPP
}
// simulateFlowShopC
int simulateFlowShopC(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateFlowShopC(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateFlowShopC(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateFlowShopTotalBufferC
int simulateFlowShopTotalBufferC(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateFlowShopTotalBufferC(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateFlowShopC_TFT
int simulateFlowShopC_TFT(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateFlowShopC_TFT(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateFlowShopC_TFT(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateFlowShopTotalBufferC_TFT
int simulateFlowShopTotalBufferC_TFT(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC_TFT(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateFlowShopTotalBufferC_TFT(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateFlowShopC_DueTime
int simulateFlowShopC_DueTime(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateFlowShopC_DueTime(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateFlowShopC_DueTime(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateFlowShopTotalBufferC_DueTime
int simulateFlowShopTotalBufferC_DueTime(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC_DueTime(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateFlowShopTotalBufferC_DueTime(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateBufferUsage
int simulateBufferUsage(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateBufferUsage(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateBufferUsage(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateBufferUsageTotalBuffer
int simulateBufferUsageTotalBuffer(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, std::vector<std::string> permutationM2, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateBufferUsageTotalBuffer(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP permutationM2SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM2(permutationM2SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateBufferUsageTotalBuffer(jobDataDF, permutationM1, permutationM2, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateSpecial
int simulateSpecial(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, int maxBufferSize, bool abortOnInvalidity, bool getMakespan);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateSpecial(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP, SEXP getMakespanSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    Rcpp::traits::input_parameter< bool >::type getMakespan(getMakespanSEXP);
    rcpp_result_gen = Rcpp::wrap(simulateSpecial(jobDataDF, permutationM1, maxBufferSize, abortOnInvalidity, getMakespan));
    return rcpp_result_gen;
END_RCPP
}
// simulateSpecialTotalBuffer
int simulateSpecialTotalBuffer(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, int maxBufferSize, bool abortOnInvalidity, bool getMakespan);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateSpecialTotalBuffer(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP, SEXP getMakespanSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    Rcpp::traits::input_parameter< bool >::type getMakespan(getMakespanSEXP);
    rcpp_result_gen = Rcpp::wrap(simulateSpecialTotalBuffer(jobDataDF, permutationM1, maxBufferSize, abortOnInvalidity, getMakespan));
    return rcpp_result_gen;
END_RCPP
}
// simulateSpecialIdleTime
int simulateSpecialIdleTime(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateSpecialIdleTime(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateSpecialIdleTime(jobDataDF, permutationM1, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}
// simulateSpecialIdleTimeTotalBuffer
int simulateSpecialIdleTimeTotalBuffer(Rcpp::DataFrame jobDataDF, std::vector<std::string> permutationM1, int maxBufferSize, bool abortOnInvalidity);
RcppExport SEXP _TwoMachineFlowShopBuffers_simulateSpecialIdleTimeTotalBuffer(SEXP jobDataDFSEXP, SEXP permutationM1SEXP, SEXP maxBufferSizeSEXP, SEXP abortOnInvaliditySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type jobDataDF(jobDataDFSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type permutationM1(permutationM1SEXP);
    Rcpp::traits::input_parameter< int >::type maxBufferSize(maxBufferSizeSEXP);
    Rcpp::traits::input_parameter< bool >::type abortOnInvalidity(abortOnInvaliditySEXP);
    rcpp_result_gen = Rcpp::wrap(simulateSpecialIdleTimeTotalBuffer(jobDataDF, permutationM1, maxBufferSize, abortOnInvalidity));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_TwoMachineFlowShopBuffers_startACOLS", (DL_FUNC) &_TwoMachineFlowShopBuffers_startACOLS, 17},
    {"_TwoMachineFlowShopBuffers_startDABC", (DL_FUNC) &_TwoMachineFlowShopBuffers_startDABC, 7},
    {"_TwoMachineFlowShopBuffers_startDABCWithParameters", (DL_FUNC) &_TwoMachineFlowShopBuffers_startDABCWithParameters, 13},
    {"_TwoMachineFlowShopBuffers_startHVNS", (DL_FUNC) &_TwoMachineFlowShopBuffers_startHVNS, 7},
    {"_TwoMachineFlowShopBuffers_startHVNSWithParams", (DL_FUNC) &_TwoMachineFlowShopBuffers_startHVNSWithParams, 9},
    {"_TwoMachineFlowShopBuffers_startILS", (DL_FUNC) &_TwoMachineFlowShopBuffers_startILS, 21},
    {"_TwoMachineFlowShopBuffers_simulateFlowShopC", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateFlowShopC, 5},
    {"_TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC, 5},
    {"_TwoMachineFlowShopBuffers_simulateFlowShopC_TFT", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateFlowShopC_TFT, 5},
    {"_TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC_TFT", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC_TFT, 5},
    {"_TwoMachineFlowShopBuffers_simulateFlowShopC_DueTime", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateFlowShopC_DueTime, 5},
    {"_TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC_DueTime", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateFlowShopTotalBufferC_DueTime, 5},
    {"_TwoMachineFlowShopBuffers_simulateBufferUsage", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateBufferUsage, 5},
    {"_TwoMachineFlowShopBuffers_simulateBufferUsageTotalBuffer", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateBufferUsageTotalBuffer, 5},
    {"_TwoMachineFlowShopBuffers_simulateSpecial", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateSpecial, 5},
    {"_TwoMachineFlowShopBuffers_simulateSpecialTotalBuffer", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateSpecialTotalBuffer, 5},
    {"_TwoMachineFlowShopBuffers_simulateSpecialIdleTime", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateSpecialIdleTime, 4},
    {"_TwoMachineFlowShopBuffers_simulateSpecialIdleTimeTotalBuffer", (DL_FUNC) &_TwoMachineFlowShopBuffers_simulateSpecialIdleTimeTotalBuffer, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_TwoMachineFlowShopBuffers(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
