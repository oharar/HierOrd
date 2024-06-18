mat <- matrix(1:6, ncol=2)
rownames(mat) <- paste0("r", 1:3)
colnames(mat) <- paste0("c", 1:2)

rot1 <- rotateLVs(mat, rotation.fn="svd", scale=FALSE)
rot1s <- rotateLVs(mat, rotation.fn="svd", scale=TRUE)

rot2 <- rotateLVs(mat, rotation.fn="Varimax", scale=FALSE)
rot2s <- rotateLVs(mat, rotation.fn="Varimax", scale=TRUE)

test_that("rotation works", {
  expect_equal(dim(rot1$Th), c(2,2))
  expect_equal(dim(rot1$loadings), c(3,2))
  expect_equal(rot1$loadings[1,1], -4.0757808)
  expect_equal(rot1$Th, rot1s$Th)
  expect_equal(rot1s$loadings[1,1], 1)
  expect_equal(rot1s$sd, c(1.30868348, 0.53604808))
  expect_equal(rownames(mat), rownames(rot1s$loadings))
  expect_equal(colnames(mat), colnames(rot1s$loadings))
})

  test_that("Varimax works", {
    expect_equal(dim(rot2$Th), c(2,2))
  expect_equal(dim(rot2$loadings), c(3,2))
  expect_equal(rot2$loadings[1,1], -1.37115325)
  expect_equal(rot2$Th, rot2s$Th)
  expect_equal(rot2s$loadings[1,1], -1)
  expect_equal(c(rot2s$sd, use.names=FALSE), c(0.28291432, 1.38562603))
  expect_equal(rownames(mat), rownames(rot2s$loadings))
  expect_equal(colnames(mat), colnames(rot2s$loadings))
})
