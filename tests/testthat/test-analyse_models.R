test_that("Analysis prdoces at least one plot per model", {
  load(testthat::test_path('data', 'models.rda'))
  res <- analyse_models(models)
  
  expect_gte(length(res[['plot']]), 10)
})
