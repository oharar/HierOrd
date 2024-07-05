sim <- SimulateData(NRows = 10, NCols = 5, NLVs = 1,
                    ColEff=0, RowEff=0 ,Sigma=0.1, Intercept=1)


test_that("Data simulated correctly works", {
  expect_equal(length(sim), 6)
  expect_equal(dim(sim$Counts), c(10,5))
  expect_equal(dim(sim$RowCov), c(10,1))
  expect_equal(dim(sim$ColCov), c(5,1))
})
