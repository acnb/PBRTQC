replaceNAWithPrevious <- function(vecWoNA, NApos){
  if(length(NApos) == 0){
    return(vecWoNA)
  }
  else{
    pos <- pmax(NApos-(1:length(NApos)), 1)
    return(vecWoNA[sort(c(pos, 1:length(vecWoNA)))])
  }
}


simFunction <- function(data, bias, fxs, blockSize,
                        lowerTrunc, upperTrunc, 
                        lead, controlLimits, calcContinous){
  
  d <- calcFunctions(data, bias, fxs, blockSize,
                     lowerTrunc, upperTrunc, 
                     lead, calcContinous)
  
  d |>
    dplyr::left_join(controlLimits, by=c("type")) |>
    dplyr::group_by(day, set, type) |>
    dplyr::mutate(detected = value < lcl | value > ucl) |>
    dplyr::summarise(firstDetected = suppressWarnings(min(counter[detected], 
                                                          na.rm = TRUE)), 
                     lcl = lcl[1], ucl = ucl[1]) |>
    dplyr::ungroup()
}

calcFunctions <- function(data, bias, fxs, blockSize,
                          lowerTrunc, upperTrunc, 
                          lead, calcContinous){
  
  dataSel <- data |>
    dplyr::select(measurement, day, set)
  
  if (!calcContinous){
    dataSel <- dataSel |>
      dplyr::group_by(day, set)
  }
  else {
    dataSel <- dataSel |>
      dplyr::group_by(set)
  }
  
  
  dataSel |>
    dplyr::mutate(leadtime = 1:dplyr::n() <= lead) |>
    dplyr::mutate(newValue = dplyr::case_when(
      leadtime ~ as.double(measurement),
      !leadtime ~ as.double(measurement * (1 + bias))
    )) |>
    dplyr::mutate_at(dplyr::vars(newValue), 
                     .funs = fxs, blockSize = blockSize, 
                     ll = lowerTrunc, ul = upperTrunc) |>
    dplyr::filter(!leadtime) |>
    dplyr::select(-leadtime) |>
    dplyr::group_by(day, set) |>
    dplyr::mutate(counter = 1:dplyr::n()) |>
    tidyr::gather(type, value, -day, -measurement,
                  -set, -counter, -newValue) |>
    dplyr::ungroup()
}

findControlLimits <- function(data, blockSize, 
                              lowerTrunc, upperTrunc, fxs, percAccAlarms, calcContinous){
  
  lowerPerc <- percAccAlarms/2
  upperPerc <- 1 - (percAccAlarms/2)
  
  dataSel <- data |>
    dplyr::select(day, measurement)
  
  if(!calcContinous){
    dataSel <- dataSel |>
      dplyr::group_by(day)
  }
  
  dataSel |>
    dplyr::mutate_at(dplyr::vars(measurement),
                     .funs = fxs, blockSize = blockSize, 
                     ll = lowerTrunc, ul = upperTrunc) |>
    tidyr::gather(type, value, -day, -measurement) |>
    dplyr::group_by(day, type) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::summarise(minday = min(value),
                     maxday = max(value)) |>
    dplyr::group_by(type) |>
    dplyr::summarise(lcl =  quantile(minday, lowerPerc, names = FALSE),
                     ucl =  quantile(maxday, upperPerc, names = FALSE)
    ) |>
    dplyr::ungroup()
}
