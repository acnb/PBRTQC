# Helper: minimal data without device column
make_data_no_device <- function(n_days = 30, n_per_day = 10) {
  set.seed(42)
  data.frame(
    day         = rep(seq_len(n_days), each = n_per_day),
    measurement = rnorm(n_days * n_per_day, mean = 1.0, sd = 0.1)
  )
}

# Helper: minimal data with device column
make_data_with_device <- function(n_days = 30, n_per_day = 10) {
  set.seed(42)
  data.frame(
    day         = rep(seq_len(n_days), each = n_per_day),
    measurement = rnorm(n_days * n_per_day, mean = 1.0, sd = 0.1),
    device      = rep(c("A", "B"), length.out = n_days * n_per_day)
  )
}

test_that("evalAlgos returns one row per algorithm for standard fxs", {
  result <- evalAlgos(
    data_for_sim = make_data_no_device(),
    bias         = 0.1,
    ll           = 0.7,
    ul           = 1.3,
    fxs          = list("mean" = rollMean, "median" = rollMed),
    fxs_device   = list(),
    max_samples  = 20L
  )
  expect_equal(nrow(result), 2)
  expect_true("type" %in% names(result))
})

test_that("evalAlgos returns expected columns", {
  result <- evalAlgos(
    data_for_sim = make_data_no_device(),
    bias         = 0.1,
    ll           = 0.7,
    ul           = 1.3,
    fxs          = list("EMA" = truncatedEMA),
    fxs_device   = list(),
    max_samples  = 20L
  )
  expected_cols <- c(
    "type", "blockSize", "ucl", "lcl",
    "sumMNped", "sumq95NPed", "mean_q_detected",
    "max_samples_affected", "mean_MNPed", "mean_q95NPed"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("evalAlgos includes device algorithm rows when device column present", {
  result <- evalAlgos(
    data_for_sim = make_data_with_device(),
    bias         = 0.1,
    ll           = 0.7,
    ul           = 1.3,
    fxs          = list("median" = rollMed),
    fxs_device   = list("DiffsMedian" = makeDeviceDiff()),
    max_samples  = 20L
  )
  types <- unique(result$type)
  expect_true("median" %in% types)
  expect_true("DiffsMedian" %in% types)
})

test_that("evalAlgos silently skips fxs_device when no device column", {
  expect_no_error(
    evalAlgos(
      data_for_sim = make_data_no_device(),
      bias         = 0.1,
      ll           = 0.7,
      ul           = 1.3,
      fxs          = list("median" = rollMed),
      fxs_device   = list("DiffsMedian" = makeDeviceDiff()),
      max_samples  = 20L
    )
  )
})

test_that("evalAlgos accepts factory objects in fxs", {
  expect_no_error(
    evalAlgos(
      data_for_sim = make_data_with_device(),
      bias         = 0.1,
      ll           = 0.7,
      ul           = 1.3,
      fxs          = list("DevDiff" = makeDeviceDiff()),
      fxs_device   = list(),
      max_samples  = 20L
    )
  )
})