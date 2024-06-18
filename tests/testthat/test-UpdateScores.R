Mat <- matrix(1:6, ncol=2)
Data <- CreateDataframe(mat=Mat,
                        row.data=data.frame(R=1:nrow(Mat)),
                        col.data=data.frame(C=1:ncol(Mat)), nLVs = 1)
RowU <- UpdateScores(scores=1:3, data=Data$data, rowcol="Row", names=Data$Names)
ColU <- UpdateScores(scores=1:2, data=Data$data, rowcol="Col", names=Data$Names)

Data2 <- CreateDataframe(mat=Mat,
                        row.data=data.frame(R=1:nrow(Mat)),
                        col.data=data.frame(C=1:ncol(Mat)), nLVs = 2)
RowU2 <- UpdateScores(scores=Mat+10, data=Data2$data, rowcol="Row", names=Data2$Names)
ColU2 <- UpdateScores(scores=Mat[1:2,]+10, data=Data2$data, rowcol="Col", names=Data2$Names)


test_that("UpdateScores works", {
  expect_equal(nrow(RowU), 6)
  expect_equal(RowU$RowScore1, rep(1:3, 2))
  expect_equal(nrow(ColU), 6)
  expect_equal(ColU$ColScore1, rep(1:2, each=3))
})

test_that("UpdateScores works with >1 LV", {
  expect_equal(nrow(RowU2), 6)
  expect_equal(RowU2$RowScore1, rep(11:13, 2))
  expect_equal(RowU2$RowScore2, rep(14:16, 2))
  expect_equal(nrow(ColU2), 6)
  expect_equal(ColU2$ColScore1, rep(11:12, each=3))
  expect_equal(ColU2$ColScore2, rep(14:15, each=3))
})
