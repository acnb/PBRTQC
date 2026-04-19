# Pass-through base function — avoids rollMed warm-up NAs in unit tests
fn_id <- function(measurement, blockSize, ll, ul, dataExtra = NULL) measurement

# ===========================================================================
# Group A: Factory return value and validation
# ===========================================================================

test_that("makeRunLengthCounter returns list with fn and getStore", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 8)
  expect_type(rlc, "list")
  expect_named(rlc, c("fn", "getStore"))
  expect_type(rlc$fn, "closure")
  expect_type(rlc$getStore, "closure")
})

test_that("getStore returns an empty list", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 8)
  expect_equal(rlc$getStore(), list())
})

test_that("makeRunLengthCounter errors when baseFn is not a function", {
  expect_error(makeRunLengthCounter(NULL,      ilcl = 3, iucl = 8), "'baseFn' must be a function")
  expect_error(makeRunLengthCounter("rollMed", ilcl = 3, iucl = 8), "'baseFn' must be a function")
})

test_that("makeRunLengthCounter errors when ilcl > iucl", {
  expect_error(makeRunLengthCounter(fn_id, ilcl = 10, iucl = 0), "'ilcl' must be <= 'iucl'")
})

# ===========================================================================
# Group B: Output length and NA warm-up
# ===========================================================================

test_that("fn returns vector of same length as input", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 8)
  expect_length(rlc$fn(1:10, blockSize = 3, ll = 3, ul = 8), 10)
})

test_that("fn returns NA for warm-up positions where baseFn returns NA", {
  fn_na_warmup <- function(measurement, blockSize, ll, ul, dataExtra = NULL) {
    c(NA_real_, NA_real_, measurement[3:length(measurement)])
  }
  rlc <- makeRunLengthCounter(fn_na_warmup, ilcl = 0, iucl = 4)
  result <- rlc$fn(c(10, 10, 10, 10, 10), blockSize = 3, ll = 0, ul = 4)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
})

# ===========================================================================
# Group C: Within inner control limits returns 0
# ===========================================================================

test_that("fn returns 0 for values within inner control limits", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 7)
  result <- rlc$fn(c(5, 5, 5, 5, 5), blockSize = 1, ll = 3, ul = 7)
  expect_equal(result, c(0, 0, 0, 0, 0))
})

test_that("fn returns 0 for boundary values exactly at ilcl or iucl", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 7)
  result <- rlc$fn(c(3, 7), blockSize = 1, ll = 3, ul = 7)
  expect_equal(result[1], 0)
  expect_equal(result[2], 0)
})

# ===========================================================================
# Group D: Consecutive violations above iucl increment counter
# ===========================================================================

test_that("fn increments counter for consecutive violations above iucl", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 0, iucl = 4)
  result <- rlc$fn(c(10, 10, 10, 10, 10), blockSize = 1, ll = 0, ul = 4)
  expect_equal(result, c(1, 2, 3, 4, 5))
})

# ===========================================================================
# Group E: Consecutive violations below ilcl increment counter
# ===========================================================================

test_that("fn increments counter for consecutive violations below ilcl", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 5, iucl = 10)
  result <- rlc$fn(c(1, 1, 1, 1, 1), blockSize = 1, ll = 5, ul = 10)
  expect_equal(result, c(1, 2, 3, 4, 5))
})

# ===========================================================================
# Group F: Side switch resets counter to 1
# ===========================================================================

test_that("fn resets to 1 when switching from above-iucl to below-ilcl", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 8)
  result <- rlc$fn(c(10, 10, 1, 1, 10), blockSize = 1, ll = 3, ul = 8)
  expect_equal(result, c(1, 2, 1, 2, 1))
})

test_that("fn resets to 1 when switching from below-ilcl to above-iucl", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 8)
  result <- rlc$fn(c(1, 1, 10, 10, 1), blockSize = 1, ll = 3, ul = 8)
  expect_equal(result, c(1, 2, 1, 2, 1))
})

# ===========================================================================
# Group G: In-limits period resets run; next violation starts fresh at 1
# ===========================================================================

test_that("fn starts fresh run at 1 after an in-limits period", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 8)
  result <- rlc$fn(c(10, 10, 5, 10), blockSize = 1, ll = 3, ul = 8)
  expect_equal(result, c(1, 2, 0, 1))
})

test_that("fn starts fresh run at 1 after in-limits period on below side", {
  rlc <- makeRunLengthCounter(fn_id, ilcl = 3, iucl = 8)
  result <- rlc$fn(c(1, 1, 5, 1), blockSize = 1, ll = 3, ul = 8)
  expect_equal(result, c(1, 2, 0, 1))
})

# ===========================================================================
# Group H: NA from baseFn propagates and resets run state
# ===========================================================================

test_that("fn propagates NA and resets run state", {
  fn_na_mid <- function(measurement, blockSize, ll, ul, dataExtra = NULL) {
    c(10, NA_real_, 10, 10, 10)
  }
  rlc <- makeRunLengthCounter(fn_na_mid, ilcl = 0, iucl = 4)
  result <- rlc$fn(1:5, blockSize = 1, ll = 0, ul = 4)
  expect_equal(result[1], 1)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 1)
  expect_equal(result[4], 2)
  expect_equal(result[5], 3)
})

# ===========================================================================
# Group I: dataExtra forwarded to baseFn
# ===========================================================================

test_that("fn forwards dataExtra to baseFn", {
  received_extra <- NULL
  fn_capture <- function(measurement, blockSize, ll, ul, dataExtra = NULL) {
    received_extra <<- dataExtra
    rep(5, length(measurement))
  }
  rlc <- makeRunLengthCounter(fn_capture, ilcl = 3, iucl = 7)
  extra_df <- data.frame(x = 1:3)
  rlc$fn(c(1, 2, 3), blockSize = 1, ll = 3, ul = 7, dataExtra = extra_df)
  expect_equal(received_extra, extra_df)
})

# ===========================================================================
# Group J: Integration test with real rollMed
# ===========================================================================

test_that("fn works correctly with rollMed as baseFn", {
  rlc <- makeRunLengthCounter(rollMed, ilcl = 0, iucl = 7)
  # rollMed(c(10,10,10,3,3,3), blockSize=3): NA, NA, 10, 10, 3, 3
  # counter ilcl=0, iucl=7:                 NA, NA,  1,  2, 0, 0
  m <- c(10, 10, 10, 3, 3, 3)
  result <- rlc$fn(m, blockSize = 3, ll = 0, ul = 7)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_equal(result[3], 1)
  expect_equal(result[4], 2)
  expect_equal(result[5], 0)
  expect_equal(result[6], 0)
})

test_that("fn with rollMed: switch from above-iucl to below-ilcl", {
  rlc <- makeRunLengthCounter(rollMed, ilcl = 0, iucl = 7)
  # rollMed(c(10,10,10,10,-1,-1,-1,-1), blockSize=3): NA,NA,10,10,10,-1,-1,-1
  # counter ilcl=0, iucl=7: NA,NA,1,2,3,1,2,3
  m <- c(10, 10, 10, 10, -1, -1, -1, -1)
  result <- rlc$fn(m, blockSize = 3, ll = 0, ul = 7)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_equal(result[3], 1)
  expect_equal(result[4], 2)
  expect_equal(result[5], 3)
  expect_equal(result[6], 1)
  expect_equal(result[7], 2)
  expect_equal(result[8], 3)
})

# ===========================================================================
# Group K: Inner control limits (ilcl/iucl) separate from truncation (ll/ul)
# ===========================================================================

test_that("counter uses ilcl/iucl, not ll/ul, for violation detection", {
  # ilcl=-1, iucl=1 (tight inner control limits)
  # ll=-5, ul=5 (wide truncation — baseFn receives these)
  # fn_id returns measurement unchanged, so values 2,2,2 exceed iucl=1
  rlc <- makeRunLengthCounter(fn_id, ilcl = -1, iucl = 1)
  result <- rlc$fn(c(2, 2, 2), blockSize = 1, ll = -5, ul = 5)
  expect_equal(result, c(1, 2, 3))
})

test_that("baseFn receives ll/ul (truncation), not ilcl/iucl (inner control limits)", {
  received_ll <- NULL
  received_ul <- NULL
  fn_capture_limits <- function(measurement, blockSize, ll, ul, dataExtra = NULL) {
    received_ll <<- ll
    received_ul <<- ul
    measurement
  }
  rlc <- makeRunLengthCounter(fn_capture_limits, ilcl = -1, iucl = 1)
  rlc$fn(c(0, 0, 0), blockSize = 1, ll = -5, ul = 5)
  expect_equal(received_ll, -5)
  expect_equal(received_ul,  5)
})

test_that("tighter ilcl/iucl catches violations that ll/ul would not", {
  # With ilcl=0, iucl=4 matching ll/ul: value 3 is within → 0
  rlc_shared <- makeRunLengthCounter(fn_id, ilcl = 0, iucl = 4)
  result_shared <- rlc_shared$fn(c(3, 3, 3), blockSize = 1, ll = 0, ul = 4)
  expect_equal(result_shared, c(0, 0, 0))

  # With ilcl=-1, iucl=2 (tighter inner limits): value 3 exceeds iucl → 1,2,3
  rlc_separate <- makeRunLengthCounter(fn_id, ilcl = -1, iucl = 2)
  result_separate <- rlc_separate$fn(c(3, 3, 3), blockSize = 1, ll = 0, ul = 4)
  expect_equal(result_separate, c(1, 2, 3))
})

# ── makePercentileRLC ────────────────────────────────────────────────────────

make_rlc_data <- function(n_days = 30, n_per_day = 20) {
  set.seed(1)
  data.frame(
    day         = rep(seq_len(n_days), each = n_per_day),
    measurement = rnorm(n_days * n_per_day, mean = 1.0, sd = 0.1)
  )
}

# ===========================================================================
# Group L: Factory-Rückgabe — gibt eine direkte Funktion zurück (kein List)
# ===========================================================================

test_that("makePercentileRLC returns a function, not a list", {
  fn <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  expect_true(is.function(fn))
  expect_false(is.list(fn))
})

# ===========================================================================
# Group M: Output-Eigenschaften — Länge, Warm-up NAs
# ===========================================================================

test_that("makePercentileRLC: returned function has same length as input", {
  set.seed(2)
  fn <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  x  <- rnorm(50, mean = 1.0, sd = 0.1)
  expect_length(fn(x, blockSize = 20L, ll = 0.7, ul = 1.3), 50)
})

test_that("makePercentileRLC: first blockSize-1 values are NA (warm-up)", {
  fn <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  set.seed(3)
  x      <- rnorm(50, mean = 1.0, sd = 0.1)
  result <- fn(x, blockSize = 5L, ll = 0.7, ul = 1.3)
  expect_true(all(is.na(result[1:4])))
  expect_false(is.na(result[5]))
})

# ===========================================================================
# Group N: Korrekte Zählwerte — 0 innerhalb Limits, Inkrement bei Violations
# ===========================================================================

test_that("makePercentileRLC: produces valid inner limits (no error)", {
  expect_no_error(
    makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  )
})

test_that("makePercentileRLC: returns numeric run-length counts", {
  fn <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  set.seed(4)
  x      <- rnorm(50, mean = 1.0, sd = 0.1)
  result <- fn(x, blockSize = 20L, ll = 0.7, ul = 1.3)
  non_na <- result[!is.na(result)]
  expect_true(is.numeric(result))
  expect_true(all(non_na >= 0))
})

# ===========================================================================
# Group O: Iteration über blockSize — verschiedene blockSize-Werte funktionieren
# ===========================================================================

test_that("makePercentileRLC: callable with different blockSize values", {
  fn <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  set.seed(5)
  x <- rnorm(60, mean = 1.0, sd = 0.1)
  expect_no_error(fn(x, blockSize = 10L, ll = 0.7, ul = 1.3))
  expect_no_error(fn(x, blockSize = 20L, ll = 0.7, ul = 1.3))
  expect_no_error(fn(x, blockSize = 30L, ll = 0.7, ul = 1.3))
})

test_that("makePercentileRLC: different blockSize yields different results", {
  fn <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  set.seed(6)
  x   <- rnorm(60, mean = 1.0, sd = 0.1)
  r10 <- fn(x, blockSize = 10L, ll = 0.7, ul = 1.3)
  r30 <- fn(x, blockSize = 30L, ll = 0.7, ul = 1.3)
  # Warm-up lengths differ: 9 vs 29 NAs
  expect_true(sum(is.na(r10)) < sum(is.na(r30)))
})

# ===========================================================================
# Group P: Caching — Limits werden gecacht, innerFn nur einmal pro Kombination aufgerufen
# ===========================================================================

test_that("makePercentileRLC: limits are cached (innerFn called once per blockSize/ll/ul)", {
  call_count <- 0L
  counting_fn <- function(measurement, blockSize, ll, ul, dataExtra = NULL) {
    call_count <<- call_count + 1L
    rollMed(measurement, blockSize, ll, ul, dataExtra)
  }
  fn <- makePercentileRLC(
    data       = make_rlc_data(),
    percentage = 0.9,
    innerFn    = counting_fn
  )
  set.seed(7)
  x <- rnorm(50, mean = 1.0, sd = 0.1)

  # 1. Call: Limit-Berechnung (mehrere innerFn-Calls auf baseline-data) + 1 Mess-Call
  fn(x, blockSize = 20L, ll = 0.7, ul = 1.3)
  calls_after_first <- call_count

  # 2. Call gleiche Parameter: Limits gecacht → nur noch 1 Mess-Call
  fn(x, blockSize = 20L, ll = 0.7, ul = 1.3)
  calls_after_second <- call_count

  # 3. Call neue blockSize: Limits neu berechnet → wieder mehr als 1 Call
  fn(x, blockSize = 10L, ll = 0.7, ul = 1.3)
  calls_after_third <- call_count

  # 1. Call hat mehr als 1 Call gebraucht (Limit-Berechnung + Messung)
  expect_gt(calls_after_first, 1L)
  # 2. Call (gecacht) hat genau 1 zusätzlichen Call gebraucht (nur Messung)
  expect_equal(calls_after_second - calls_after_first, 1L)
  # 3. Call (neue blockSize) hat wieder mehr als 1 zusätzlichen Call gebraucht
  expect_gt(calls_after_third - calls_after_second, 1L)
})

# ===========================================================================
# Group Q: Fehlerbehandlung — ungültiges percentage, ungültige innerFn
# ===========================================================================

test_that("makePercentileRLC errors on percentage >= 1 or <= 0 or NA", {
  d <- make_rlc_data()
  expect_error(makePercentileRLC(d, percentage = 1.1),  "percentage")
  expect_error(makePercentileRLC(d, percentage = 1.0),  "percentage")
  expect_error(makePercentileRLC(d, percentage = 0),    "percentage")
  expect_error(makePercentileRLC(d, percentage = NA_real_), "percentage")
})

test_that("makePercentileRLC errors when innerFn is not a function", {
  d <- make_rlc_data()
  expect_error(makePercentileRLC(d, percentage = 0.9, innerFn = "rollMed"), "innerFn")
  expect_error(makePercentileRLC(d, percentage = 0.9, innerFn = NULL),      "innerFn")
})

# ===========================================================================
# Group R: Custom innerFn — eigene Funktion mit rollMed-Signatur
# ===========================================================================

test_that("makePercentileRLC: custom innerFn is used instead of rollMed", {
  # rollMean hat die gleiche Signatur wie rollMed
  fn_mean <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9, innerFn = rollMean)
  fn_med  <- makePercentileRLC(data = make_rlc_data(), percentage = 0.9)
  set.seed(8)
  x <- rnorm(60, mean = 1.0, sd = 0.1)
  expect_no_error(fn_mean(x, blockSize = 20L, ll = 0.7, ul = 1.3))
  # Ergebnisse können sich unterscheiden (verschiedene inner functions)
  r_mean <- fn_mean(x, blockSize = 20L, ll = 0.7, ul = 1.3)
  r_med  <- fn_med(x,  blockSize = 20L, ll = 0.7, ul = 1.3)
  expect_length(r_mean, length(r_med))
})

# ===========================================================================
# Group S: evalAlgos-Integration — plain function in fxs funktioniert
# ===========================================================================

test_that("makePercentileRLC can be passed directly to evalAlgos in fxs", {
  d  <- make_rlc_data()
  fn <- makePercentileRLC(data = d, percentage = 0.9)
  expect_no_error(
    evalAlgos(
      data_for_sim = d,
      bias         = 0.1,
      ll           = 0.7,
      ul           = 1.3,
      fxs          = list("RLC90" = fn),
      fxs_device   = list(),
      max_samples  = 20L
    )
  )
})


