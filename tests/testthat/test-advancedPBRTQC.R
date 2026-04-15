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
