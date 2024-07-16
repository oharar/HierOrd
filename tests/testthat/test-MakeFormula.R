f1 <- MakeFormula(nlv=1, linnames=NULL, factnames=NULL, epsname="")
LinNames <- c("Lin1", "Lin2")
f2 <- MakeFormula(nlv=1, linnames=LinNames, factnames=NULL, epsname="")
FactNames <- c("Fac1", "Fac2")
f3 <- MakeFormula(nlv=1, linnames=LinNames, factnames=FactNames, epsname="",
                  factprior="A")

# add tests for linnames and factnames

test_that("Simple version works", {
  expect_equal(class(f1), "formula")
  expect_equal(as.character(f1[[3]][[5]])[[2]], "list(prior = \"pc.prec\", param = c(1, 0.01))")
  expect_equal(as.character(f1[[3]][[2]]), "eps.LV1")
  expect_equal(class(f1[[3]][[5]]), "call")
})


test_that("Linear terms work", {
  expect_equal(class(f2), "formula")
  expect_equal(as.character(f2[[3]][[2]][[2]][[2]]), LinNames[1])
  expect_equal(as.character(f2[[3]][[2]][[2]][[3]]), "linear")
  expect_equal(as.character(f2[[3]][[2]][[3]][[2]]), LinNames[2])
  expect_equal(as.character(f2[[3]][[3]][[2]]), "eps.LV1")
})

test_that("Factor terms work", {
  expect_equal(class(f3), "formula")
  expect_equal(as.character(f3[[3]][[2]][[2]][[2]][[2]][[2]]), LinNames[1])
  expect_equal(as.character(f3[[3]][[2]][[2]][[2]][[2]][[3]]), "linear")
  expect_equal(as.character(f3[[3]][[3]][[2]]), "eps.LV1")

  expect_equal(as.character(f3[[3]][[2]][[3]][[2]]), FactNames[2])
  expect_equal(as.character(f3[[3]][[2]][[3]][[3]]), paste0("Wt.", FactNames[2]))
  expect_equal(as.character(f3[[3]][[2]][[3]][[5]][2]), "A")
})
