#' Regression adjusted PBRTQC
#' 
#' @inheritParams truncatedEMA
#' @param dataExtra 
#'
#' @return
#' @export
regAdjEMA <- function(measurement, blockSize, ll, ul, dataExtra){
  # settings
  offsetDataStart <- 80 
  offsetDataStop <- 20
  recalculate <- 5
  
  dataExtra <- dataExtra |>
    dplyr::mutate(dayGrp = floor(day/recalculate),
                  measurement = winsorize(.data$measurement, ll = ll, ul = ul),
                  measurementWithErrors = 
                    winsorize(.env$measurement, ll = ll, ul = ul))
  
  predictedVsMeasured <- purrr::map(unique(dataExtra$dayGrp), function(dg){
    
    minDay <- dataExtra |>
      dplyr::filter(dayGrp == dg) |>
      dplyr::pull(day) |>
      min()
    
    dataSelected <- dataExtra |>
      dplyr::filter(dplyr::between(day, 
                                   minDay-offsetDataStart, 
                                   minDay-offsetDataStop))
    
    if(nrow(dataSelected) > 0){
      
    # formula is generated from variables in dataExtra
    form <- "measurement ~ "
    vars <- colnames(dataSelected)
    vars <- vars[!vars %in% c('measurement', 'dayGrp', 'day',
                              "measurementWithErrors")]
    for(v in vars){
      if(is.numeric(dataSelected[[v]][1])){
        form <- paste0(form, v, " +")
      } else{
        if (length(unique(dataSelected[[v]])) > 1){
          form <- paste0(form, "(1|", v, ") +")
        }
      }
    }
    form <- stringr::str_sub(form, end=-2)
    
    form <- as.formula(form)
    
    model <- parsnip::linear_reg(mode = "regression", engine = "lmer") |>
      parsnip::fit(formula = form, data = dataSelected)
    
    dataForPrediction <- dataExtra |>
      dplyr::filter(dayGrp == dg)
    
    dataForPrediction$measurementWithErrors -
      parsnip::predict_raw(model, dataForPrediction, 
                           opts=list(allow.new.levels = TRUE))
    }
    else {
      dataForPrediction <- dataExtra |>
        dplyr::filter(dayGrp == dg)
      
      rep.int(NA_real_, nrow(dataForPrediction))
    }
  }) |>
    purrr::list_c()
  
  truncatedEMA(predictedVsMeasured, blockSize, -Inf, +Inf)
  }
