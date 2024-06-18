f1 <- MakeFormula(nlv=1, linnames=NULL, factnames=NULL, epsname="")
f2 <- MakeFormula(nlv=1, linnames=NULL, factnames=NULL, epsname="")

# add tests for linnames and factnames

test_that("multiplication works", {
  expect_equal(class(f1), "formula")
  expect_equal(as.character(f1[[3]][[5]])[[2]], "list(prior = \"pc.prec\", param = c(1, 0.01))")
  expect_equal(class(f1[[3]][[5]]), "call")
})
