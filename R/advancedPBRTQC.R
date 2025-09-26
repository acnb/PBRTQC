#' Regression adjusted PBRTQC
#' 
#' @inheritParams truncatedEMA
#' @param dataExtra 
#'
#' @return
#' @export
regAdjEMA <- function(measurement, blockSize, ll, ul, dataExtra){
  # settings
  offsetDataStart <- 120 # start Model Data
  offsetDataStop <- 20 # Stop Model Data
  recalculate <- 20 # recalculate every x days
  
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
      from <- min(dataForModel$day)
      to <- max(dataForModel$day)

      model <- parsnip::linear_reg() |>
        parsnip::set_engine("lmer") |>
        parsnip::fit(formula = form, data = dataForModel) |>
        store_advanced_model('regAdj', hash, from, to)
    } 
    
    dataForPrediction <- dataExtra |>
      dplyr::filter(dayGrp == dg)
    
    dataForPrediction$measurementWithErrors <-
      parsnip::predict_raw(model, dataForPrediction, 
                           opts=list(allow.new.levels = TRUE))
    
    dataForPrediction$measurement - dataForPrediction$measurementWithErrors
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
#' @param from first day of data
#' @param to last day of data
#'
#' @return model
#' @export
store_advanced_model<- function(model, name, hash, from, to){
  if (!exists("tempStore")){
    tempStore <<- list()
  }
  tempStore[[name]][[hash]][['model']] <<- model
  tempStore[[name]][[hash]]['from'] <<- from
  tempStore[[name]][[hash]]['to'] <<- to
  
  model 
}

retrieve_advanced_model <- function(name, hash){
  if (!exists("tempStore")){
    return(NULL)
  }
  purrr::pluck(tempStore, name, hash, 'model')
}
  

analyse_models <- function(store){
  purrr::map(names(store), function(n){
    if (n == 'regAdj'){
      analyse_RA_models(store[['regAdj']])
    }
  }) |> 
    purrr::list_flatten() #|>
 #   purrr::list_transpose(template = c('plot', 'tab'))
}


analyse_RA_models <- function(models){
  len <- length(models)
  
  tos <- models |>
    purrr::map_dbl('to') |>
    sort(decreasing = TRUE) 
  
  REs <- models |>
    purrr::keep(function(x){
      purrr::pluck(x, 'to') %in% tos[c(1,2)]
    }) |>
    purrr::map(function(m){
      m[['model']] |>
        parsnip::extract_fit_engine() |>
        merTools::REsim() |>
        dplyr::mutate(from = m[['from']],
               to = m[['to']])
      
    })
  
  trend <- REs[[1]] |>
    dplyr::as_tibble() |>
    dplyr::mutate(absMean = abs(mean)) |>
    dplyr::group_by(groupFctr) |>
    dplyr::slice_max(absMean, prop = 0.05) |>
    dplyr::ungroup() |>
    dplyr::select(groupFctr, groupID, term, mean, sd) |>
    dplyr::mutate(set = 'new')
  
  trend <- trend |>
    dplyr::bind_rows(REs[[2]] |>
                       dplyr::as_tibble() |>
                       dplyr::inner_join(trend |> 
                                           dplyr::select(groupFctr, 
                                                         groupID, term),
                                         by=dplyr::join_by(groupFctr, 
                                                           groupID, term)
                                         )  |>
                       dplyr::mutate(set = 'old')
                     ) |>
    dplyr::group_by(groupFctr) |>
    dplyr::mutate(groupID = forcats::fct_reorder(groupID, mean)) |>
    dplyr::ungroup()
    
    p <- ggplot2::ggplot(trend, ggplot2::aes(x=groupID, y=mean, ymin=mean-1.96*sd,
                                        ymax=mean+1.96*sd,color=set)) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_point(position=ggplot2::position_dodge(width=.9)) +
      ggplot2::geom_linerange(position=ggplot2::position_dodge(width=.9)) +
      ggplot2::facet_wrap(groupFctr~.) +
      ggplot2::scale_color_discrete(breaks=c('old', 'new')) +
      ggplot2::theme_minimal() +
      ggplot2::xlab('') +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45))

  list('plot' = list(p), 'tab' = unname(REs))
}

