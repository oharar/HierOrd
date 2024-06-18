lc1 <- MakeLincomb(covs=NULL, lvname="LV1", lcname="lc", epsname="eps", N=3)
lc2 <- MakeLincomb(covs=data.frame(x =1:5), lvname="LV1", lcname="lc", epsname="eps")


LCA <- MakeLincombs(covar.df=NULL, nlv=2, EpsName="eps", N=3)

test_that("MakeLincomb works", {
  expect_equal(length(lc1), 3)
  expect_equal(length(lc2), 5)
})

test_that("MakeLincombs works", {
  expect_equal(length(LCA), 6)
  expect_equal(length(LCA[[1]]), 1)
})
