
#' @export
parseFlowShopName <- function(fileName){
  parts <- strsplit(fileName, "-")[[1]]
  names(parts) <- c("bufferType", "numberOfJobs", "m2Type", "bufType", "constrainedness", "maxBufferSize", "instanceNumber")
  return(parts)
}


