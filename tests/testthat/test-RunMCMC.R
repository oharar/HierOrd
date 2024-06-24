m <- matrix(1:6, ncol=2)

test_that("Dummy tests", {
  expect_equal(dim(m), c(3,2))
})
