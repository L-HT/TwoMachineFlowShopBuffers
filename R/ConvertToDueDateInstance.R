
#' Generiere Deadline-Instanz von 2-Maschinen-Flow-Shop
#' aus normaler Job-Data-Datei
#'
#' @export
convertToDueDateInstance <- function(fileName){
  test <- read.csv(fileName, stringsAsFactors = F)
  instanceData <- parseFlowShopName(fileName)
  if (grepl("inter",instanceData["bufferType"])){
    bufferType = "intermediateBuffer"
  }
  if (grepl("total",instanceData["bufferType"])){
    bufferType = "totalBuffer"
  }
  test <- appendDeadlinesToJobData(test, bufferType,
                                      as.integer(instanceData["maxBufferSize"]))
  write.csv(test, fileName, quote=F, row.names=F)
}

#' Generiere Deadline-Instanz von 2-Maschinen-Flow-Shop
#' aus normaler Job-Data-Datei
#'
#' @export
convertFolderToDueDateInstance <- function(path){
  setwd(path)
  for (f in list.files()){
    convertToDueDateInstance(f)
  }
  setwd("../")
}
