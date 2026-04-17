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
makeRegAdjEMA <- function(
  offsetDataStart = 120,
  offsetDataStop = 20,
  recalculate = 20
) {
  .cache <- list()

  fn <- function(measurement, blockSize, ll, ul, dataExtra) {
    dataExtra <- dataExtra |>
      dplyr::mutate(
        dayGrp = floor(day / recalculate),
        measurement = winsorize(.data$measurement, ll = ll, ul = ul),
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
        dplyr::filter(dplyr::between(
          day,
          minDay - offsetDataStart,
          minDay - offsetDataStop
        ))

      dataForPrediction <- dataExtra |>
        dplyr::filter(dayGrp == dg)

      if (nrow(dataForModel) == 0) {
        return(rep.int(NA_real_, nrow(dataForPrediction)))
      }

      # Build formula from column types in dataForModel
      vars <- setdiff(
        colnames(dataForModel),
        c("measurement", "dayGrp", "day", "measurementWithErrors")
      )
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

      hash <- digest::digest(dataForModel)
      cached <- .cache[[hash]]

      if (is.null(cached)) {
        requireNamespace("multilevelmod", quietly = TRUE)
        model <- parsnip::linear_reg() |>
          parsnip::set_engine("lmer") |>
          parsnip::fit(formula = form, data = dataForModel)

        cached <- list(
          model = model,
          from = min(dataForModel$day),
          to = max(dataForModel$day)
        )
        .cache[[hash]] <<- cached
      }

      dataForPrediction$measurementWithErrors <-
        parsnip::predict_raw(
          cached$model,
          dataForPrediction,
          opts = list(allow.new.levels = TRUE)
        )

      dataForPrediction$measurement - dataForPrediction$measurementWithErrors
    }) |>
      purrr::list_c()

    truncatedEMA(predictedVsMeasured, blockSize, -Inf, +Inf)
  }

  getStore <- function() list(regAdj = .cache)

  list(fn = fn, getStore = getStore)
}


# Internal helper used by makeDeviceDiff / makeDeviceMeanDiff
.makeDeviceDiffWith <- function(estimator, fname) {
  fn <- function(measurement, blockSize, ll, ul, dataExtra) {
    measurement[measurement < ll] <- ll
    measurement[measurement > ul] <- ul

    n <- length(measurement)
    device <- dataExtra$device

    if (is.null(device)) {
      warning(
        fname,
        ": column 'device' missing from dataExtra — returning all NAs"
      )
      return(rep(NA_real_, n))
    }

    overall_est <- slider::slide_dbl(
      measurement,
      estimator,
      .before = blockSize - 1,
      .complete = TRUE
    )

    device_est <- rep(NA_real_, n)
    for (dev in unique(device)) {
      idx <- which(device == dev)
      device_est[idx] <- slider::slide_dbl(
        measurement[idx],
        estimator,
        .before = blockSize - 1,
        .complete = TRUE
      )
    }

    device_est - overall_est
  }

  getStore <- function() list()

  list(fn = fn, getStore = getStore)
}

#' Factory for device-difference PBRTQC (median)
#'
#' At each time point computes the difference between the per-device rolling
#' median and the overall rolling median.  Requires a column \code{device} in
#' \code{dataExtra}.
#'
#' @return A list with \code{fn} and \code{getStore} (same interface as
#'   \code{\link{makeRegAdjEMA}})
#' @export
makeDeviceDiff <- function() {
  .makeDeviceDiffWith(median, "makeDeviceDiff")
}

#' Factory for device-difference PBRTQC (mean)
#'
#' At each time point computes the difference between the per-device rolling
#' mean and the overall rolling mean.  Requires a column \code{device} in
#' \code{dataExtra}.
#'
#' @return A list with \code{fn} and \code{getStore} (same interface as
#'   \code{\link{makeRegAdjEMA}})
#' @export
makeDeviceMeanDiff <- function() {
  .makeDeviceDiffWith(mean, "makeDeviceMeanDiff")
}


# Internal helper: count run lengths of out-of-bounds values.
# Returns the consecutive count for values above ul (side +1) or below ll
# (side -1).  Switches between sides reset the count to 1.  Values within
# [ll, ul] return 0 and reset the run state.  NA values propagate and also
# reset the run state.
.countRunLengths <- function(values, ll, ul) {
  if (ll > ul) {
    stop("ll must be <= ul")
  }

  n <- length(values)
  result <- numeric(n)
  current_side <- 0L # 0 = none, 1 = above ul, -1 = below ll
  current_count <- 0L

  for (i in seq_len(n)) {
    v <- values[i]
    if (is.na(v)) {
      result[i] <- NA_real_
      current_side <- 0L
      current_count <- 0L
    } else if (v > ul) {
      if (current_side == 1L) {
        current_count <- current_count + 1L
      } else {
        current_count <- 1L
        current_side <- 1L
      }
      result[i] <- current_count
    } else if (v < ll) {
      if (current_side == -1L) {
        current_count <- current_count + 1L
      } else {
        current_count <- 1L
        current_side <- -1L
      }
      result[i] <- current_count
    } else {
      result[i] <- 0
      current_side <- 0L
      current_count <- 0L
    }
  }
  result
}

#' Factory for run-length counter PBRTQC
#'
#' Wraps a base PBRTQC function (such as \code{rollMed}) and counts
#' consecutive violations on the same side of the inner control limits.
#' A switch from above \code{iucl} to below \code{ilcl} (or vice versa)
#' resets the counter to 1.  Values within \code{[ilcl, iucl]} return 0.
#' NA values (e.g. warm-up period) propagate as NA and also reset the run
#' state.
#'
#' The three limit parameters serve distinct roles:
#' \describe{
#'   \item{Truncation limits (\code{ll}, \code{ul})}{Passed at call time to
#'     \code{baseFn} (e.g. for winsorisation).  Set via \code{simPBRTQC()}.}
#'   \item{Inner control limits (\code{ilcl}, \code{iucl})}{Captured at
#'     factory time.  Used by the run-length counter to decide whether the
#'     output of \code{baseFn} constitutes a violation.}
#'   \item{Outer control limits}{Computed by \code{simPBRTQC()} from the
#'     run-length counter output; determine when an alarm is raised.}
#' }
#'
#' @param baseFn A PBRTQC algorithm function with the standard signature
#'   \code{function(measurement, blockSize, ll, ul, dataExtra = NULL)}.
#'   The canonical choice is \code{rollMed}.
#' @param ilcl Lower inner control limit: the run-length counter increments
#'   whenever the output of \code{baseFn} is below \code{ilcl}.
#' @param iucl Upper inner control limit: the run-length counter increments
#'   whenever the output of \code{baseFn} is above \code{iucl}.
#'   Must satisfy \code{ilcl <= iucl}.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{fn}}{Algorithm function suitable for \code{simPBRTQC()}.
#'       Returns a numeric vector of the same length as \code{measurement}:
#'       run-length counts (>= 1) for violations, 0 for in-bounds values,
#'       and NA for warm-up positions.}
#'     \item{\code{getStore}}{Returns an empty list (no internal state)}
#'   }
#' @export
makeRunLengthCounter <- function(baseFn, ilcl, iucl) {
  if (!is.function(baseFn)) {
    stop("'baseFn' must be a function")
  }
  if (ilcl > iucl) {
    stop("'ilcl' must be <= 'iucl'")
  }

  fn <- function(measurement, blockSize, ll, ul, dataExtra = NULL) {
    base_values <- baseFn(measurement, blockSize, ll, ul, dataExtra)
    .countRunLengths(base_values, ilcl, iucl)
  }

  getStore <- function() list()

  list(fn = fn, getStore = getStore)
}


#' Factory for percentile-based run-length counter PBRTQC
#'
#' Computes inner control limits from baseline data so that the central
#' \code{percentage} of rolling-median values fall within \code{[ilcl, iucl]},
#' then wraps \code{\link{rollMed}} inside \code{\link{makeRunLengthCounter}}.
#'
#' The inner limits are derived from \code{\link{findControlLimits}} with
#' \code{percAccAlarms = 1 - percentage}, i.e. a 90\% interval uses
#' \code{percAccAlarms = 0.10}.  The limits are fixed at factory creation
#' time for the supplied \code{block_size}.
#'
#' @param data A data frame with columns \code{day} (integer) and
#'   \code{measurement} (numeric) — the baseline dataset used to derive
#'   inner limits.
#' @param percentage Numeric scalar in (0, 1).  Proportion of baseline
#'   rolling-median values that should fall within the inner limits
#'   (e.g. \code{0.9} for 90\%).
#' @param block_size Integer scalar.  Block size used when computing the
#'   inner limits via \code{\link{findControlLimits}}.
#' @param ll Numeric scalar.  Lower truncation limit forwarded to
#'   \code{\link{rollMed}} at call time.
#' @param ul Numeric scalar.  Upper truncation limit forwarded to
#'   \code{\link{rollMed}} at call time.
#'
#' @return A list with \code{fn} and \code{getStore} (same interface as
#'   \code{\link{makeRunLengthCounter}}).
#' @export
makePercentileRLC <- function(data, percentage, block_size, ll, ul) {
  # isTRUE prevents NA from slipping through the || chain
  if (
    !isTRUE(
      is.numeric(percentage) &&
        length(percentage) == 1L &&
        percentage > 0 &&
        percentage < 1
    )
  ) {
    stop("'percentage' must be a single numeric value in (0, 1)")
  }

  # Derive inner limits: central `percentage` of the rolling-median distribution.
  # NOTE: ll and ul passed to factory$fn at call time must match those used here;
  # mismatched truncation limits produce statistically invalid inner limits.
  perc_acc <- 1 - percentage
  limits <- findControlLimits(
    data = data,
    blockSize = block_size,
    lowerTrunc = ll,
    upperTrunc = ul,
    fxs = list(rollMed = rollMed),
    percAccAlarms = perc_acc,
    calcContinous = TRUE
  )

  matched <- limits[limits$type == "rollMed", ]
  if (nrow(matched) != 1L) {
    stop(
      "findControlLimits did not return exactly one 'rollMed' row; ",
      "check that 'data' contains enough observations for 'block_size'"
    )
  }

  ilcl <- matched$lcl
  iucl <- matched$ucl

  if (!is.finite(ilcl) || !is.finite(iucl)) {
    stop(
      "inner control limits are not finite (ilcl = ",
      ilcl,
      ", iucl = ",
      iucl,
      "); 'block_size' may be too large relative to samples per day in 'data'"
    )
  }

  makeRunLengthCounter(rollMed, ilcl = ilcl, iucl = iucl)
}


analyse_models <- function(store) {
  purrr::map(names(store), function(n) {
    if (n == 'regAdj') {
      analyse_RA_models(store[['regAdj']])
    }
  }) |>
    purrr::list_flatten() #|>
  #   purrr::list_transpose(template = c('plot', 'tab'))
}


analyse_RA_models <- function(models) {
  len <- length(models)

  tos <- models |>
    purrr::map_dbl('to') |>
    sort(decreasing = TRUE)

  REs <- models |>
    purrr::keep(function(x) {
      purrr::pluck(x, 'to') %in% tos[c(1, 2)]
    }) |>
    purrr::map(function(m) {
      m[['model']] |>
        parsnip::extract_fit_engine() |>
        merTools::REsim() |>
        dplyr::mutate(from = m[['from']], to = m[['to']])
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
    dplyr::bind_rows(
      REs[[2]] |>
        dplyr::as_tibble() |>
        dplyr::inner_join(
          trend |>
            dplyr::select(groupFctr, groupID, term),
          by = dplyr::join_by(groupFctr, groupID, term)
        ) |>
        dplyr::mutate(set = 'old')
    ) |>
    dplyr::group_by(groupFctr) |>
    dplyr::mutate(groupID = forcats::fct_reorder(groupID, mean)) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(
    trend,
    ggplot2::aes(
      x = groupID,
      y = mean,
      ymin = mean - 1.96 * sd,
      ymax = mean + 1.96 * sd,
      color = set
    )
  ) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .9)) +
    ggplot2::geom_linerange(position = ggplot2::position_dodge(width = .9)) +
    ggplot2::facet_wrap(groupFctr ~ .) +
    ggplot2::scale_color_discrete(breaks = c('old', 'new')) +
    ggplot2::theme_minimal() +
    ggplot2::xlab('') +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45)
    )

  list('plot' = list(p), 'tab' = unname(REs))
}
