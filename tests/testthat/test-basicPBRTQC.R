test_that("replace NA with previous: basic case with alternating NAs", {
  # measurement = [1, NA, 3, NA, 5, 6]  → NApos=[2,4], vecWoNA=EMA([1,3,5,6])
  vecWoNA <- c(1, 2, 3, 4)
  NApos   <- c(2, 4)
  expect_equal(replaceNAWithPrevious(vecWoNA, NApos), c(1, 1, 2, 2, 3, 4))
})

test_that("replace NA with previous: no NAs → vector unchanged", {
  vecWoNA <- c(1, 2, 3)
  expect_equal(replaceNAWithPrevious(vecWoNA, integer(0)), c(1, 2, 3))
})

test_that("replace NA with previous: three consecutive NAs in middle", {
  # measurement = [1, 2, NA, NA, NA, 6, 7]
  # vecWoNA = EMA([1, 2, 6, 7]) = [v1, v2, v3, v4]
  # All three NAs should be filled with v2 (value just before the gap)
  vecWoNA <- c(10, 20, 30, 40)
  NApos   <- c(3, 4, 5)
  expect_equal(replaceNAWithPrevious(vecWoNA, NApos), c(10, 20, 20, 20, 20, 30, 40))
})

test_that("replace NA with previous: leading NAs stay NA (no backfill)", {
  # measurement = [NA, NA, 3, 4, 5]
  # vecWoNA = EMA([3, 4, 5]) — leading NAs have no previous value
  vecWoNA <- c(30, 40, 50)
  NApos   <- c(1, 2)
  result  <- replaceNAWithPrevious(vecWoNA, NApos)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_equal(result[3:5], c(30, 40, 50))
})

test_that("replace NA with previous: trailing NAs filled with last value", {
  # measurement = [1, 2, 3, NA, NA]
  vecWoNA <- c(10, 20, 30)
  NApos   <- c(4, 5)
  expect_equal(replaceNAWithPrevious(vecWoNA, NApos), c(10, 20, 30, 30, 30))
})

test_that("replace NA with previous: non-consecutive NAs across the vector", {
  # measurement = [NA, NA, 3, NA, 5]
  # NApos=[1,2,4], vecWoNA=[v1,v2] (2 non-NA values)
  # pos 1+2: no previous → NA; pos 4: fill with v1
  vecWoNA <- c(30, 50)
  NApos   <- c(1, 2, 4)
  result  <- replaceNAWithPrevious(vecWoNA, NApos)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_equal(result[3], 30)
  expect_equal(result[4], 30)
  expect_equal(result[5], 50)
})

test_that("replace NA with previous: single NA at start", {
  vecWoNA <- c(10, 20, 30)
  NApos   <- c(1)
  result  <- replaceNAWithPrevious(vecWoNA, NApos)
  expect_true(is.na(result[1]))
  expect_equal(result[2:4], c(10, 20, 30))
})

test_that("replace NA with previous: single NA at end", {
  vecWoNA <- c(10, 20, 30)
  NApos   <- c(4)
  expect_equal(replaceNAWithPrevious(vecWoNA, NApos), c(10, 20, 30, 30))
})
