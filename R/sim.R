#' Simualtion of PBRTQC methods
#'
#' @param data data.frame
#' @param blockSizes numeric vector
#' @param truncationLimits data.frame with columns 'lowerTrunc', 'upperTrunc'
#'                          and 'truncation'.
#' @param biases numeric vector
#' @param fxs named list of functions for PBRTQC algorithms.
#' @param percAccAlarms percentage of acceptable false alarms per day
#' @param calcContinous
#' @param max_samples optional integer. If set, at most this many samples per
#'   day are used. If a \code{device} column is present, the limit applies per
#'   device per day.
#'
#' @returns data.frame with results
#' @export
#'
#' @examples
#' \dontrun{
#' biases <- bias * seq(-1.5, 1.5, by=0.2)
#' blockSizes <- seq(10, 150, by = 20)
#' fxs <- list("mean" = rollMean,
#'             'median' = rollMed,
#'             'EMA' = truncatedEMA)
#'  truncationLimits <- data.frame('lowerTrunc' = ll,
#'                                 'upperTrunc' = ul,
#'                                 'truncation' = 'userSelected')
#'  res <- simPBRTQC(dataForSim,
#'                   blockSizes, truncationLimits, biases, fxs,
#'                    0.05)
#' }
#'
#'
simPBRTQC <- function(
  data,
  blockSizes,
  truncationLimits,
  biases,
  fxs,
  percAccAlarms,
  calcContinous,
  max_samples = NULL,
  label = ""
) {
  #numberOfDaysSimulated <- 100

  # Limit samples per day (per device if applicable)
  if (!is.null(max_samples)) {
    group_vars <- if ("device" %in% colnames(data)) {
      c("day", "device")
    } else {
      "day"
    }
    data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::slice_head(n = max_samples) |>
      dplyr::ungroup()
  }

  varsCls <- cbind(data.frame(blockSize = blockSizes), truncationLimits)

  if (shiny::isRunning()) {
    shiny::incProgress(0, detail = 'Control limits')
  }

  ss <- Sys.time()
  cls <- varsCls |>
    dplyr::rowwise() |>
    dplyr::mutate(
      r = (function(blockSize, lowerTrunc, upperTrunc) {
        res <- findControlLimits(
          blockSize = blockSize,
          lowerTrunc = lowerTrunc,
          upperTrunc = upperTrunc,
          data = data,
          fxs = fxs,
          percAccAlarms,
          calcContinous
        )
        list(res)
      })(blockSize, lowerTrunc, upperTrunc)
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = c(r)) |>
    dplyr::select(truncation, type, blockSize, ucl, lcl)

  ee <- Sys.time()
  lead <- max(blockSizes)

  params <- varsCls |>
    tidyr::crossing(tibble::tibble(bias = biases))

  nVars <- nrow(params)

  pb <- NULL
  if (!shiny::isRunning()) {
    prefix <- if (nzchar(label)) paste0("  ", label, "\n") else ""
    pb <- progress::progress_bar$new(
      format = paste0(prefix, "  [:bar] :current/:total | ETA :eta | :percent"),
      total  = nVars,
      clear  = FALSE
    )
  }

  # When a device column is present, bias is applied only to the device with the
  # most samples (matching the logic in calcFunctions). max_samples_affected
  # therefore reflects how many samples of that device can be measured per day
  # at most. Without a device column all samples are affected.
  max_samples_affected <- if ("device" %in% colnames(data)) {
    biased_device <- names(which.max(table(data$device)))
    data |>
      dplyr::filter(device == biased_device) |>
      dplyr::count(day) |>
      dplyr::pull(n) |>
      max()
  } else {
    data |>
      dplyr::count(day) |>
      dplyr::pull(n) |>
      max()
  }

  results <- params |>
    dplyr::rowwise() |>
    dplyr::mutate(
      r = (function(bias, bs, trunc, lowerTrunc, upperTrunc) {
        if (shiny::isRunning()) {
          shiny::incProgress(
            1 / nVars,
            detail = paste('Bias:', bias, '; Block Size:', bs)
          )
        } else if (!is.null(pb)) {
          pb$tick()
        }

        thisCls <- cls |>
          dplyr::filter(truncation == trunc & blockSize == bs) |>
          dplyr::select(-truncation, -blockSize)

        numDays <- length(unique(data$day))

        # if (numDays > 100 & shiny::isRunning()  && FALSE){
        #   data <- data |>
        #     dplyr::filter(day %in% base::sample(unique(data$day,
        #                                                numberOfDaysSimulated)))
        # }

        res <- simFunction(
          data = data,
          blockSize = blockSize,
          lowerTrunc = lowerTrunc,
          upperTrunc = upperTrunc,
          controlLimits = thisCls,
          lead = lead,
          bias = bias,
          fxs = fxs,
          calcContinous
        )

        list(res)
      })(bias, blockSize, truncation, lowerTrunc, upperTrunc)
    ) |>
    dplyr::ungroup() |>
    tidyr::unnest(cols = c(r))

  results |>
    dplyr::mutate(max_samples_affected = max_samples_affected)
}


simFunction <- function(
  data,
  bias,
  fxs,
  blockSize,
  lowerTrunc,
  upperTrunc,
  lead,
  controlLimits,
  calcContinous
) {
  d <- calcFunctions(
    data,
    bias,
    fxs,
    blockSize,
    lowerTrunc,
    upperTrunc,
    lead,
    calcContinous
  )

  d |>
    dplyr::left_join(controlLimits, dplyr::join_by("type")) |>
    dplyr::group_by(day, type) |>
    dplyr::mutate(detected = value < lcl | value > ucl) |>
    dplyr::summarise(
      firstDetected = suppressWarnings(min(counter[detected], na.rm = TRUE)),
      lcl = lcl[1],
      ucl = ucl[1]
    ) |>
    dplyr::ungroup()
}

calcFunctions <- function(
  data,
  bias,
  fxs,
  blockSize,
  lowerTrunc,
  upperTrunc,
  lead,
  calcContinous
) {
  dataSel <- data |>
    dplyr::select(measurement, day)

  dataExtra <- data

  if (!calcContinous) {
    dataSel <- dataSel |>
      dplyr::group_by(day)

    dataExtra <- dataExtra |>
      dplyr::group_by(day)
  }

  # If a 'device' column is present, bias only the device with the most samples.
  # This simulates a single-device systematic error rather than a lab-wide shift,
  # which is the relevant scenario for makeDeviceDiff.
  # Without a 'device' column, all measurements are biased as before.
  isbiased <- if ("device" %in% colnames(data)) {
    data$device == names(which.max(table(data$device)))
  } else {
    rep(TRUE, nrow(data))
  }

  dataSel |>
    dplyr::mutate(leadtime = 1:dplyr::n() <= lead) |>
    dplyr::mutate(
      newValue = dplyr::case_when(
        leadtime ~ as.double(measurement),
        !leadtime & isbiased ~ as.double(measurement * (1 + bias)),
        TRUE ~ as.double(measurement)
      )
    ) |>
    dplyr::mutate_at(
      dplyr::vars(newValue),
      .funs = fxs,
      blockSize = blockSize,
      ll = lowerTrunc,
      ul = upperTrunc,
      dataExtra = dataExtra
    ) |>
    dplyr::filter(!leadtime) |>
    dplyr::select(-leadtime) |>
    dplyr::group_by(day) |>
    dplyr::mutate(counter = 1:dplyr::n()) |>
    tidyr::gather(type, value, -day, -measurement, -counter, -newValue) |>
    dplyr::ungroup()
}

findControlLimits <- function(
  data,
  blockSize,
  lowerTrunc,
  upperTrunc,
  fxs,
  percAccAlarms,
  calcContinous
) {
  lowerPerc <- percAccAlarms / 2
  upperPerc <- 1 - (percAccAlarms / 2)

  dataSel <- data |>
    dplyr::select(day, measurement)

  dataExtra <- data

  if (!calcContinous) {
    dataSel <- dataSel |>
      dplyr::group_by(day)

    dataExtra <- dataExtra |>
      dplyr::group_by(day)
  }

  dataSel |>
    dplyr::mutate_at(
      dplyr::vars(measurement),
      .funs = fxs,
      blockSize = blockSize,
      ll = lowerTrunc,
      ul = upperTrunc,
      dataExtra = dataExtra
    ) |>
    tidyr::gather(type, value, -day, -measurement) |>
    dplyr::group_by(day, type) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::summarise(minday = min(value), maxday = max(value)) |>
    dplyr::group_by(type) |>
    dplyr::summarise(
      lcl = quantile(minday, lowerPerc, names = FALSE),
      ucl = quantile(maxday, upperPerc, names = FALSE)
    ) |>
    dplyr::ungroup()
}

winsorize <- function(x, ll, ul) {
  x[x < ll] <- ll
  x[x > ul] <- ul
  x
}
