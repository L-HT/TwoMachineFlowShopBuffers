library(R6)

Machine <- R6Class("Machine",
  public = list(
    name = NA,
    currentJob = NA,
    jobTime = 0,
    executedTime = 0,
    busy = FALSE,
    blocked = FALSE,
    turnedOff = FALSE,
    work = function(){
      if (!blocked){
        executedTime <<- executedTime + 1
        if (executedTime == jobTime){
          #busy = FALSE
          return(statusReport("end"))
        }
        if (executedTime > jobTime){

          message(paste(name, ": bearbeitet Job ", currentJob, " zu viel.", sep=""))
        }
      }
      return(statusReport("none"))
    },
    give = function(jobName, time){
      currentJob <<- jobName
      jobTime <<- time
      executedTime <<- 0
      busy <<- TRUE
      blocked <<- FALSE
      return(statusReport("start"))
    },
    statusReport = function(state){
      return(list("job" = currentJob, "state" = state))
    },
    finalize = function(){
      #print("Finalizer Machine aufgerufen")
    }
  ),
  portable = FALSE
)
