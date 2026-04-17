#' Evaluate and rank PBRTQC algorithm settings
#'
#' Runs \code{\link{simPBRTQC}} across a grid of block sizes and bias levels,
#' summarises each algorithm by median patients until error detection (MNPed)
#' and the 95th-percentile equivalent (q95NPed), and returns one row per
#' algorithm with the optimal block size and control limits.
#'
#' Both plain functions and factory objects (lists with a \code{$fn} element,
#' such as those returned by \code{\link{makeDeviceDiff}}) are accepted in
#' \code{fxs} and \code{fxs_device}.
#'
#' @param data_for_sim A data frame with columns \code{day} (integer),
#'   \code{measurement} (numeric), and optionally \code{device}
#'   (character/factor). The \code{device} column is required for
#'   \code{fxs_device} algorithms; it is excluded automatically for
#'   standard \code{fxs} algorithms.
#' @param bias Numeric scalar. Magnitude of the simulated bias (e.g.
#'   \code{0.03} for 3\%). Positive and negative multiples from 1x to 2x
#'   are tested automatically.
#' @param ll Numeric scalar. Lower truncation/winsorisation limit.
#' @param ul Numeric scalar. Upper truncation/winsorisation limit.
#' @param fxs Named list of standard PBRTQC algorithm functions or factory
#'   objects (see \code{\link{makeRegAdjEMA}}). Pass \code{list()} to skip.
#' @param fxs_device Named list of device-level algorithm functions or factory
#'   objects. Silently ignored when \code{data_for_sim} has no \code{device}
#'   column. Pass \code{list()} to skip.
#' @param perc_acc_alarms Numeric scalar. Acceptable false alarm proportion
#'   per day passed to \code{\link{simPBRTQC}}. Default: \code{0.025}.
#' @param max_samples Integer scalar. Maximum samples per day passed to
#'   \code{\link{simPBRTQC}}. Default: \code{400L}.
#'
#' @return A tibble with one row per algorithm (\code{type}) containing
#'   \code{blockSize}, \code{ucl}, \code{lcl}, \code{sumMNped},
#'   \code{sumq95NPed}, \code{mean_q_detected}, \code{max_samples_affected},
#'   \code{mean_MNPed}, \code{mean_q95NPed}.
#'
#' @seealso \code{\link{simPBRTQC}}, \code{\link{makeDeviceDiff}},
#'   \code{\link{makeDeviceMeanDiff}}
#'
#' @examples
#' \dontrun{
#' data_for_sim <- data.frame(
#'   day         = rep(1:50, each = 20),
#'   measurement = rnorm(1000, mean = 1.0, sd = 0.1),
#'   device      = rep(c("A", "B"), 500)
#' )
#' result <- evalAlgos(
#'   data_for_sim = data_for_sim,
#'   bias         = 0.03,
#'   ll           = 0.7,
#'   ul           = 1.3,
#'   fxs          = list("mean" = rollMean, "median" = rollMed, "EMA" = truncatedEMA),
#'   fxs_device   = list("DiffsMedian" = makeDeviceDiff(), "DiffsMean" = makeDeviceMeanDiff())
#' )
#' print(result)
#' }
#' @export
evalAlgos <- function(
  data_for_sim,
  bias,
  ll,
  ul,
  fxs,
  fxs_device,
  perc_acc_alarms = 0.025,
  max_samples = 400L
) {
  # Unwrap factory objects (list with $fn) to their inner function.
  # Plain functions are passed through unchanged.
  normalise <- function(lst) {
    lapply(lst, function(f) if (is.list(f) && is.function(f$fn)) f$fn else f)
  }
  fxs        <- normalise(fxs)
  fxs_device <- normalise(fxs_device)

  # Test positive and negative bias multiples from 1x to 2x
  biases      <- c(bias * seq(1, 2, by = 0.2), bias * -1 * seq(1, 2, by = 0.2))
  block_sizes <- seq(10, 150, by = 20)

  truncation_limits <- data.frame(
    lowerTrunc = ll,
    upperTrunc = ul,
    truncation = "userSelected"
  )

  # Standard algorithms run without the device column
  has_device    <- "device" %in% colnames(data_for_sim)
  data_no_device <- if (has_device) dplyr::select(data_for_sim, -device) else data_for_sim

  res <- simPBRTQC(
    data_no_device,
    block_sizes,
    truncation_limits,
    biases,
    fxs,
    perc_acc_alarms,
    calcContinous = TRUE,
    max_samples = max_samples
  )

  # Device-level algorithms require a device column; skip silently if absent
  if (length(fxs_device) > 0 && has_device) {
    res_device <- simPBRTQC(
      data_for_sim,
      block_sizes,
      truncation_limits,
      biases,
      fxs_device,
      perc_acc_alarms,
      calcContinous = TRUE,
      max_samples = max_samples
    )
    res <- dplyr::bind_rows(res, res_device)
  }

  # Summarise each combination of algorithm, block size and control limits
  result_eval <- res |>
    dplyr::group_by(blockSize, truncation, bias, type, ucl, lcl) |>
    dplyr::summarise(
      q95NPed              = quantile(firstDetected, probs = 0.95),
      MNPed                = median(firstDetected),
      q_detected           = sum(is.finite(firstDetected)) / dplyr::n(),
      n                    = dplyr::n(),
      max_samples_affected = max(max_samples_affected),
      .groups = "drop"
    )

  # Replace infinite summary values with a penalty above the maximum observed
  best_settings_by_alg <- result_eval |>
    dplyr::mutate(
      MNPed   = dplyr::if_else(is.finite(MNPed), MNPed, max_samples_affected * 1.1),
      q95NPed = dplyr::if_else(is.finite(q95NPed), q95NPed, max_samples_affected * 1.1)
    ) |>
    dplyr::group_by(type, blockSize, ucl, lcl) |>
    dplyr::summarise(
      sumMNped             = sum(MNPed),
      sumq95NPed           = sum(q95NPed),
      mean_q_detected      = mean(q_detected),
      max_samples_affected = max(max_samples_affected),
      mean_MNPed           = mean(MNPed),
      mean_q95NPed         = mean(q95NPed),
      .groups = "drop"
    )

  # Select the single best block size per algorithm:
  # minimise sumMNped, then sumq95NPed, then maximise detection rate,
  # then prefer the smallest block size
  best_settings_by_alg |>
    dplyr::group_by(type) |>
    dplyr::filter(sumMNped == min(sumMNped)) |>
    dplyr::filter(sumq95NPed == min(sumq95NPed)) |>
    dplyr::filter(mean_q_detected == max(mean_q_detected)) |>
    dplyr::filter(blockSize == min(blockSize)) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
}
