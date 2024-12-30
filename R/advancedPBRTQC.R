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
    
    dataForModel <- dataExtra |>
      dplyr::ungroup() |>
      dplyr::select(-measurementWithErrors, -dayGrp) |>
      dplyr::filter(dplyr::between(day, 
                                   minDay-offsetDataStart, 
                                   minDay-offsetDataStop))
    
    if(nrow(dataForModel) > 0){
      
    # formula is generated from variables in dataExtra
    form <- "measurement ~ "
    vars <- colnames(dataForModel)
    vars <- vars[!vars %in% c('measurement', 'dayGrp', 'day',
                              "measurementWithErrors")]
    for(v in vars){
      if(is.numeric(dataForModel[[v]][1])){
        form <- paste0(form, v, " +")
      } else{
        if (length(unique(dataForModel[[v]])) > 1){
          form <- paste0(form, "(1|", v, ") +")
        }
      }
    }
    form <- stringr::str_sub(form, end=-2)
    
    form <- as.formula(form)
    
    library(multilevelmod)
    
    hash <- digest::digest(dataForModel)
    model <- retrieve_advanced_model('regAdj', hash)
    
    if (is.null(model)){
      model <- parsnip::linear_reg() |>
        parsnip::set_engine("lmer") |>
        parsnip::fit(formula = form, data = dataForModel) |>
        store_advanced_model('regAdj', hash)
    } 
    
    dataForPrediction <- dataExtra |>
      dplyr::filter(dayGrp == dg)
    
    dataForPrediction$measurementWithErrors <-
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


#' Store Advanced Model
#' 
#' Stores a model from advanced PBRTQC in a global variable for later analysis.
#'
#' @param model The model.
#' @param name The name of the advanced PBRTQC method.
#' @param hash Hash of data and formular of model
#'
#' @return model
#' @export
store_advanced_model<- function(model, name, hash){
  if (!exists("tempStore")){
    tempStore <<- list()
  }
  tempStore[[name]][[hash]] <<- model
  model 
}

retrieve_advanced_model <- function(name, hash){
  if (!exists("tempStore")){
    return(NULL)
  }
  purrr::pluck(tempStore, name, hash)
}
  

analyse_models <- function(store){
  purrr::map(names(store), function(n){
    if (n == 'regAdj'){
      purrr::map(store[['regAdj']], analyse_RA_model)
    }
  }) |> 
    purrr::list_flatten() |>
    purrr::list_transpose(template = c('plot', 'tab'))
}


analyse_RA_model <- function(model){
  REs <- model |>
    parsnip::extract_fit_engine() |>
    merTools::REsim()
  
  p <- merTools::plotREsim(REs)
  list('plot' = p, 'tab' = REs)
}

