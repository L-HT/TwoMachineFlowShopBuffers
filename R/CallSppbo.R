#' @export
init_rJava <- function(){
  library(rJava)
  .jinit("./123")
  .jaddClassPath("/home/lht/aufgaben/2mFS/tmfs/123/lib")
  .jclassPath()
  rJava_initialized <- TRUE
}

rJava_initialized <- FALSE

#' @export
startSPPBO <- function(jobData, fileName, runNumber, fileSuffix = "",
                       solutionsPerIteration = 10, populationSize = 5, wElite = 0.01,
                       wTotal = 1.5, filePathPrefix = "./instances/"){

  if (!rJava_initialized){
    init_rJava()
  }
  fullFileName <- paste(filePathPrefix, fileName, sep="")
  if (fileSuffix != ""){
    logFileName <- paste(fileName, "sppbo", runNumber, fileSuffix, sep="-")
  } else {
    logFileName <- paste(fileName, "sppbo", runNumber, sep="-")
    print(logFileName)
  }

  N <- nrow(jobData)
  runTimeInSeconds <- -1
  if (N == 25){ runTimeInSeconds <- 120 }
  if (N == 50){ runTimeInSeconds <- 300 }
  if (N == 75){ runTimeInSeconds <- 600 }
  if (N == 100){ runTimeInSeconds <- 900 }
  if (runTimeInSeconds < 0){
    stop("Jobzahl ungültig")
  } else {
    print(runTimeInSeconds)
  }

  wIterationBest <- (wTotal/2) / populationSize
  wPersonalPrevious <- (wTotal/2) / solutionsPerIteration
  args <- c("-algo", "FS3",
            "-label", logFileName,
            "-testfile",fullFileName,
            "-solutions", as.character(solutionsPerIteration),
            "-popsize", as.character(populationSize),
            "-iterations", as.character(runTimeInSeconds),
            "-runs", "1",
            "-loginterval", "1",
            "-weightElite", as.character(wElite),
            "-weightIterationBest", as.character(wIterationBest),
            "-weightPersonalBest", "0.0",
            "-weightPersonalPrevious", as.character(wPersonalPrevious),
            "-weightInit", as.character(1/(N-1)),
            "-alpha", "1",
            "-beta", "0")

  fakemain <- .jnew("SPPBOMain")
  .jcall(obj=fakemain, returnSig="V",method = "main",args)
}
