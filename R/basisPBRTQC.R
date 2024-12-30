rollingFunc <- function(f, ...) {
  function(measurement, blockSize, ...){
    #  c(rep(NA_real_, blockSize - 1),
    slider::slide_vec(measurement,
              .f = f,
              .before = blockSize - 1,
              .complete = TRUE,
              ...)
  }
}

replaceNAWithPrevious <- function(vecWoNA, NApos){
  if(length(NApos) == 0){
    return(vecWoNA)
  }
  else{
    pos <- pmax(NApos-(1:length(NApos)), 1)
    return(vecWoNA[sort(c(pos, 1:length(vecWoNA)))])
  }
}

truncatedMean <- function(measurement, ll, ul){
  measurement[measurement < ll] <- ll
  measurement[measurement > ul] <- ul
  mean(measurement)
}

logMean <- function(measurement, ll, ul){
  measurement[measurement < ll] <- ll
  measurement[measurement > ul] <- ul
  measurement <- measurement + 10^5
  mean(log(measurement))
}

truncatedMed <- function(measurement, ll, ul){
  measurement[measurement < ll] <- ll
  measurement[measurement > ul] <- ul
  median(measurement)
}


#' Exponential Moving Average with winsorizing
#'
#' @param measurement a vector of measurements 
#' @param blockSize the block size
#' @param ll lower limit for winsorizing
#' @param ul upper limit for winsorizing
#'
#' @return vector of exponential moving averages
#' @export
truncatedEMA <- function(measurement, blockSize, ll, ul){
  measurement[measurement < ll] <- ll
  measurement[measurement > ul] <- ul
  
  NAidx <- which(is.na(measurement))
  
  if(length(measurement) <= blockSize){
    return(rep(NA, length(measurement)))
  }
  else{
    res <- TTR::EMA(na.omit(measurement), n = blockSize)
    replaceNAWithPrevious(res, NAidx) 
  }
}

truncatedlogEMA <- function(measurement, blockSize, ll, ul){
  measurement[measurement < ll] <- ll
  measurement[measurement > ul] <- ul
  measurement <- measurement + 10^5
  
  if(length(measurement) <= blockSize){
    return(rep(NA, length(measurement)))
  }
  else{
    TTR::EMA(log(measurement), n = blockSize)
  }
}

truncatedEMA.del <- function(measurement, blockSize, ll, ul){
  idx <- which(measurement >= ll & measurement <= ul)
  NAidx <- which(!(measurement >= ll & measurement <= ul))
  
  if(length(measurement[idx]) <= blockSize){
    return(rep(NA, length(measurement)))
  }
  else{
    res <- TTR::EMA(measurement[idx], n = blockSize)
    replaceNAWithPrevious(res, NAidx) 
  }
}

rollMean <- function(measurement, blockSize, ll, ul, ...){
  fx <- rollingFunc(truncatedMean)
  fx(measurement, blockSize, ll, ul)
}

rollLogMean <- rollingFunc(logMean)
rollMed <-  rollingFunc(truncatedMed)

rollMean.del <- rollingFunc(truncatedMean.del)
rollLogMean.del <- rollingFunc(logMean.del)
rollMed.del <-  rollingFunc(truncatedMed.del)