#' Factory for regression-adjusted PBRTQC
#'
#' Creates an isolated instance of the regAdjEMA algorithm with its own
#' model cache. Each call returns a fresh instance, so Shiny sessions and
#' scripts stay independent with no shared global state.
#'
#' @param offsetDataStart Days of history before the current window (default: 120)
#' @param offsetDataStop  Days before the current window where history stops (default: 20)
#' @param recalculate     Recalculate model every this many days (default: 20)
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{fn}}{Algorithm function to pass to \code{simPBRTQC()}}
#'     \item{\code{getStore}}{Returns cached models for \code{analyse_models()}}
#'   }
#' @export
makeRegAdjEMA <- function(offsetDataStart = 120,
                          offsetDataStop  = 20,
                          recalculate     = 20) {
  .cache <- list()

  fn <- function(measurement, blockSize, ll, ul, dataExtra) {
    dataExtra <- dataExtra |>
      dplyr::mutate(
        dayGrp               = floor(day / recalculate),
        measurement          = winsorize(.data$measurement, ll = ll, ul = ul),
        measurementWithErrors = winsorize(.env$measurement, ll = ll, ul = ul)
      )

    predictedVsMeasured <- purrr::map(unique(dataExtra$dayGrp), function(dg) {
      minDay <- dataExtra |>
        dplyr::filter(dayGrp == dg) |>
        dplyr::pull(day) |>
        min()

      dataForModel <- dataExtra |>
        dplyr::ungroup() |>
        dplyr::select(-measurementWithErrors, -dayGrp) |>
        dplyr::filter(dplyr::between(day,
                                     minDay - offsetDataStart,
                                     minDay - offsetDataStop))

      dataForPrediction <- dataExtra |>
        dplyr::filter(dayGrp == dg)

      if (nrow(dataForModel) == 0) {
        return(rep.int(NA_real_, nrow(dataForPrediction)))
      }

      # Build formula from column types in dataForModel
      vars  <- setdiff(colnames(dataForModel),
                       c("measurement", "dayGrp", "day", "measurementWithErrors"))
      terms <- character(0)
      for (v in vars) {
        if (is.numeric(dataForModel[[v]][1])) {
          terms <- c(terms, v)
        } else if (length(unique(dataForModel[[v]])) > 1) {
          terms <- c(terms, paste0("(1|", v, ")"))
        }
      }
      if (length(terms) == 0) {
        return(rep.int(NA_real_, nrow(dataForPrediction)))
      }
      form <- as.formula(paste("measurement ~", paste(terms, collapse = " + ")))

      hash   <- digest::digest(dataForModel)
      cached <- .cache[[hash]]

      if (is.null(cached)) {
        requireNamespace("multilevelmod", quietly = TRUE)
        model <- parsnip::linear_reg() |>
          parsnip::set_engine("lmer") |>
          parsnip::fit(formula = form, data = dataForModel)

        cached <- list(model = model,
                       from  = min(dataForModel$day),
                       to    = max(dataForModel$day))
        .cache[[hash]] <<- cached
      }

      dataForPrediction$measurementWithErrors <-
        parsnip::predict_raw(cached$model, dataForPrediction,
                             opts = list(allow.new.levels = TRUE))

      dataForPrediction$measurement - dataForPrediction$measurementWithErrors
    }) |>
      purrr::list_c()

    truncatedEMA(predictedVsMeasured, blockSize, -Inf, +Inf)
  }

  getStore <- function() list(regAdj = .cache)

  list(fn = fn, getStore = getStore)
}
  

#' Factory for device-difference PBRTQC
#'
#' At each time point computes the difference between the per-device rolling
#' median and the overall rolling median.  Requires a column \code{device} in
#' \code{dataExtra}.
#'
#' @return A list with \code{fn} and \code{getStore} (same interface as
#'   \code{\link{makeRegAdjEMA}})
#' @export
makeDeviceDiff <- function() {
  fn <- function(measurement, blockSize, ll, ul, dataExtra) {
    measurement[measurement < ll] <- ll
    measurement[measurement > ul] <- ul

    n      <- length(measurement)
    device <- dataExtra$device

    if (is.null(device)) {
      warning("makeDeviceDiff: column 'device' missing from dataExtra — returning all NAs")
      return(rep(NA_real_, n))
    }

    overall_med <- slider::slide_dbl(
      measurement, median,
      .before   = blockSize - 1,
      .complete = TRUE
    )

    device_med <- rep(NA_real_, n)
    for (dev in unique(device)) {
      idx <- which(device == dev)
      device_med[idx] <- slider::slide_dbl(
        measurement[idx], median,
        .before   = blockSize - 1,
        .complete = TRUE
      )
    }

    device_med - overall_med
  }

  getStore <- function() list()

  list(fn = fn, getStore = getStore)
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

