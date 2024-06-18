Mat <- matrix(1:6, ncol=2)
nLVs <- 2
Data <- CreateDataframe(mat=Mat,
                        row.data=data.frame(R=1:nrow(Mat)),
                        col.data=data.frame(C=1:ncol(Mat)), nLVs = nLVs)
LCA <- MakeLincombs(covar.df =NULL, EpsName = "Roweps", nlv =nLVs, N=Data$NRows)
Form <- MakeFormula(nlv=nLVs, linnames="R", epsname="Row")
mod1 <- INLA::inla(Form, data=Data$data, lincomb = LCA)

sim1 <- SimLinComb(mod1, n=1)
sim4 <- SimLinComb(mod1, n=4)


MatBig <- matrix(1:40, ncol=2)
DataBig <- CreateDataframe(mat=MatBig,
                        row.data=data.frame(R=1:nrow(MatBig)),
                        col.data=data.frame(C=1:ncol(MatBig)), nLVs = nLVs)
LCABig <- MakeLincombs(covar.df =NULL, EpsName = "Roweps", nlv =nLVs, N=DataBig$NRows)
FormBig <- MakeFormula(nlv=nLVs, linnames="R", epsname="Row")
modBig1 <- INLA::inla(FormBig, data=DataBig$data, lincomb = LCABig)

simBig1 <- SimLinComb(modBig1, n=1)

test_that("SimLinComb works", {
  expect_equal(dim(sim1), c(3,2))
  expect_equal(colnames(sim1), paste0("LV", 1:2))
  expect_equal(rownames(sim1), paste0("lc", 1:3))
  expect_equal(dim(sim4), c(3,8))
  expect_equal(colnames(sim4), paste0("LV", rep(1:2, each=4), rep(1:4, times=2)))
  expect_equal(rownames(sim4), paste0("lc", 1:3))
})

test_that("SimLinComb works for a bigger dataset", {
  expect_equal(dim(simBig1), c(20,2))
  expect_equal(colnames(simBig1), paste0("LV", 1:2))
  expect_equal(rownames(simBig1), paste0("lc", 1:20))
})
