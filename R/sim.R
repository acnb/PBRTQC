simPBRTQC <- function(data, blockSizes, truncationLimits, biases, fxs, 
                         percAccAlarms, calcContinous){
  
  numberOfDaysSimulated <- 100
  
  varsCls <- cbind(data.frame(blockSize = blockSizes), truncationLimits) 
  
  if (shiny::isRunning()){
    shiny::incProgress(0, detail = 'Control limits')
  }
  
  ss <- Sys.time()
  cls <- varsCls |>
    dplyr::rowwise() |>
    dplyr::mutate(r =  (function(blockSize, lowerTrunc, upperTrunc){
      res <- findControlLimits(blockSize = blockSize, 
                               lowerTrunc = lowerTrunc,
                               upperTrunc = upperTrunc, 
                               data = data,
                               fxs = fxs, percAccAlarms, calcContinous)
      list(res)
    })(blockSize, lowerTrunc, upperTrunc)) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols  = c(r)) |>
    dplyr::select(truncation, type, blockSize,  ucl, lcl)
  
  ee <- Sys.time()
  lead <- max(blockSizes)
  
  params <- varsCls |>
    tidyr::crossing(tibble::tibble(bias = biases))
  
  nVars <- nrow(params)
  
  results <- params |>
    dplyr::rowwise() |>
    dplyr::mutate(r =  (function(bias, bs, trunc,
                                 lowerTrunc, upperTrunc){
      
      if (shiny::isRunning()){
        shiny::incProgress(1/nVars, detail = paste('Bias:', bias, 
                                                   '; Block Size:', bs))
      } else {
       message(paste('Bias:', bias, '; Block Size:', bs))
      }
      
      thisCls <- cls |>
        dplyr::filter(truncation == trunc & blockSize == bs) |>
        dplyr::select(-truncation, -blockSize)
      
      numDays <- length(unique(data$day))   
      
      if (numDays > 100 & shiny::isRunning()  && FALSE){
        data <- data |>
          dplyr::filter(day %in% base::sample(unique(data$day, 
                                                     numberOfDaysSimulated)))
      }
      
      res <- simFunction(data = data,
                         blockSize = blockSize, 
                         lowerTrunc = lowerTrunc,
                         upperTrunc = upperTrunc,
                         controlLimits = thisCls,
                         lead = lead,
                         bias = bias,
                         fxs = fxs,
                         calcContinous)
      
      list(res)
    })(bias, blockSize, truncation, lowerTrunc, upperTrunc)) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols  = c(r))
  
  results
}


simFunction <- function(data, bias, fxs, blockSize,
                        lowerTrunc, upperTrunc, 
                        lead, controlLimits, calcContinous){
  
  d <- calcFunctions(data, bias, fxs, blockSize,
                     lowerTrunc, upperTrunc, 
                     lead, calcContinous)
  
  d |>
    dplyr::left_join(controlLimits, by=c("type")) |>
    dplyr::group_by(day, type) |>
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
    dplyr::select(measurement, day)
  
  dataExtra <- data 
  
  if (!calcContinous){
    dataSel <- dataSel |>
      dplyr::group_by(day)
    
    dataExtra <- dataExtra |>
      dplyr::group_by(day)
  }

  
  dataSel |>
    dplyr::mutate(leadtime = 1:dplyr::n() <= lead) |>
    dplyr::mutate(newValue = dplyr::case_when(
      leadtime ~ as.double(measurement),
      !leadtime ~ as.double(measurement * (1 + bias))
    )) |>
    dplyr::mutate_at(dplyr::vars(newValue), 
                     .funs = fxs, blockSize = blockSize, 
                     ll = lowerTrunc, ul = upperTrunc,
                     dataExtra = dataExtra) |>
    dplyr::filter(!leadtime) |>
    dplyr::select(-leadtime) |>
    dplyr::group_by(day) |>
    dplyr::mutate(counter = 1:dplyr::n()) |>
    tidyr::gather(type, value, -day, -measurement,
                  -counter, -newValue) |>
    dplyr::ungroup()
}

findControlLimits <- function(data, blockSize, 
                              lowerTrunc, upperTrunc, fxs, percAccAlarms, calcContinous){
  
  lowerPerc <- percAccAlarms/2
  upperPerc <- 1 - (percAccAlarms/2)
  
  dataSel <- data |>
    dplyr::select(day, measurement)
  
  dataExtra <- data
  
  if(!calcContinous){
    dataSel <- dataSel |>
      dplyr::group_by(day)
    
    dataExtra <- dataExtra |>
      dplyr::group_by(day)
  }
  
  dataSel |>
    dplyr::mutate_at(dplyr::vars(measurement),
                     .funs = fxs, blockSize = blockSize, 
                     ll = lowerTrunc, ul = upperTrunc,
                     dataExtra = dataExtra) |>
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

winsorize <- function(x, ll, ul){
  x[x < ll] <- ll
  x[x > ul] <- ul
  x
}
