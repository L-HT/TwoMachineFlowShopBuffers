#' @export
SimData <- R6Class("SimData",
   public = list(
     simData = NULL,
     initialize = function(){
       simData <<- data.frame("type" = c(), "job" = c(), "start" = c(), "end" = c(), stringsAsFactors=F)
     },
     writeSimData = function(type, job, startOrEnd, time){

       affectedRowNumbers <- which(simData$job == job & simData$type == type)
       if (length(affectedRowNumbers) == 1){
         if (startOrEnd == "start"){
           simData[affectedRowNumbers, "start"] <<- time
           message("SimData: start geschrieben, obwohl schon da?")
         }
         if (startOrEnd == "end"){
           simData[affectedRowNumbers, "end"] <<- time
         }
       }
       if (length(affectedRowNumbers) == 0){
         newRow <- data.frame(type, job, time, NA)
         names(newRow) <- c("type", "job", "start", "end")
         simData <<- rbind(simData, newRow)#, stringsAsFactors = FALSE)
         if (startOrEnd == "end"){
           message("simData: End geschrieben, obwohl noch nicht da?")
         }
       }
     },
     finalize = function(){
       #print("Finalizer SimData aufgerufen")
     },
     getSimData = function(){
       return(simData)
     },
     sortSimData = function(){
       simData <<- simData[order(simData$type),]
       row.names(simData) <<- NULL
     }
   ),
   portable = FALSE
)

#' @export
getMakespan <- function(simRes){
  return(max(simRes$end))
}

#' @export
getMakespanOfSimulation <- function(jobData, permutationM1, permutationM2, maxBufferSize, type = "intermediateBuffer",
                                    targetCriterion = "makespan"){

  if (targetCriterion == "TFT"){
    if (type == "totalBuffer"){
      result <- simulateFlowShopTotalBufferC_TFT(jobData, permutationM1, permutationM2, maxBufferSize)
    } else {
      result <- simulateFlowShopC_TFT(jobData, permutationM1, permutationM2, maxBufferSize)
    }
  } else {
    if (targetCriterion == "makespan"){
      if (type == "totalBuffer"){
        result <- simulateFlowShopTotalBufferC(jobData, permutationM1, permutationM2, maxBufferSize)
      } else {
        result <- simulateFlowShopC(jobData, permutationM1, permutationM2, maxBufferSize)
      }
    } else {
      if (targetCriterion == "dueTimes"){
        if (type == "totalBuffer"){
          result <- simulateFlowShopTotalBufferC_DueTime(jobData, permutationM1, permutationM2, maxBufferSize)
        } else {
          result <- simulateFlowShopC_DueTime(jobData, permutationM1, permutationM2, maxBufferSize)
        }
      } else {
        stop("Zielkriterium unbekannt")
      }
    }
  }


  return(result)
}


