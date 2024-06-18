m <- matrix(1:6, ncol=2)
rdf <- data.frame(r=1:3)
cdf <- data.frame(c=1:2)
df <- CreateDataframe(mat=m, row.data=rdf,
                      col.data=cdf, nLVs=1)


test_that("data frame created", {
  expect_equal(length(df), 4)
  expect_equal(nrow(df$data), 6)
  expect_equal(length(df$Names), 9)
})
