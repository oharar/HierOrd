lc1 <- MakeLincomb(covs=NULL, lvname="LV1", lcname="lc", epsname="eps", N=3)
lc2 <- MakeLincomb(covs=data.frame(x =1:5), lvname="LV1", lcname="lc", epsname="eps")
vec <- 1:5
lc3 <- MakeLincomb(covs=as.data.frame(vec), lvname="LV1", lcname="lc", epsname="eps")


LCA <- MakeLincombs(covar.df=NULL, nlv=2, EpsName="eps", N=3)
LCB <- MakeLincombs(covar.df=data.frame(x =1:5), nlv=2, EpsName="eps")

test_that("MakeLincomb works", {
  expect_equal(length(lc1), 3)
  expect_equal(length(lc2), 5)
  expect_equal(length(lc3), 5) # works if a vector is passed
  expect_equal(names(lc3[[3]][[1]]), "vec.LV1")
})

test_that("MakeLincombs works", {
  expect_equal(length(LCA), 6)
  expect_equal(length(LCA[[1]]), 1)
})

test_that("MakeLincombs works with covariates", {
  expect_equal(length(LCB), 10)
  expect_equal(length(LCB[[1]]), 2)
  expect_equal(LCB[[3]][[1]]$x.LV1$weight, 3)
})
