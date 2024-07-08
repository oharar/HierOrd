m <- data.frame(matrix(1:6, ncol=2, dimnames=list(paste0("row", 1:3),
                                                   paste0("col", 1:2))))
rdf <- data.frame(r=1:3)
cdf <- data.frame(c=1:2, c2=factor(3:4))
df <- CreateDataframe(mat=m, row.data=rdf,
                      col.data=cdf, nLVs=2)
df.null <- CreateDataframe(mat=as.matrix(m), row.data=NULL,
                      col.data=NULL, nLVs=1)


test_that("data frame created", {
  expect_equal(length(df), 5)
  expect_equal(dim(df$data), c(6, 26))
  expect_equal(length(df$Names), 9)
  expect_equal(df$nLVs, 2)
  expect_equal(df$Names$LVCovs$Row, c("r.LV1", "r.LV2"))
  expect_equal(df$Names$LVCovs$Col, c("c.LV1", "c2.LV1", "c.LV2", "c2.LV2"))
  expect_equal(df$Names$NumericCovs$Row, "r")
  expect_equal(df$Names$FactorCovs$Col, "c2")
})

test_that("null data frame created", {
  expect_equal(length(df.null), 5)
  expect_equal(dim(df.null$data), c(6,9))
  expect_equal(length(df.null$Names), 9)
  expect_equal(df.null$nLVs, 1)
})
