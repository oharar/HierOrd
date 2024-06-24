m <- matrix(1:6, ncol=2)
rdf <- data.frame(r=1:3)
cdf <- data.frame(c=1:2)
df <- CreateDataframe(mat=m, row.data=rdf,
                      col.data=cdf, nLVs=1)
df.null <- CreateDataframe(mat=m, row.data=NULL,
                      col.data=NULL, nLVs=2)


test_that("data frame created", {
  expect_equal(length(df), 5)
  expect_equal(nrow(df$data), 6)
  expect_equal(length(df$Names), 9)
  expect_equal(df$nLVs, 1)
})

test_that("null data frame created", {
  expect_equal(length(df.null), 5)
  expect_equal(nrow(df.null$data), 6)
  expect_equal(length(df.null$Names), 9)
  expect_equal(df.null$nLVs, 2)
})
