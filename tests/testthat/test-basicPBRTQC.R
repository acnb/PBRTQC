test_that("replace NA with previous PBRTQC value", {
  vecWoNA <- c(1,2,3,4)
  NApos <- c(2, 4)
  res <- replaceNAWithPrevious(vecWoNA, NApos)
  expect_equal(res, c(1,1, 2, 2, 3, 4))
})
