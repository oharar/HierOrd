#' Create a data frame
#'
#' @param mat A matrix
#' @param row.data Data
#' @param col.data Data
#' @param nLVs Number of latent variables
#'
#' @details Create a data frame
#'
#' @return A data frame
#'
#' @examples
#' CreateDataframe(mat=matrix(1:6, ncol=2), row.data=data.frame(r=1:3),
#' col.data=data.frame(c=1:2), nLVs=1)
#' @export
#'@importFrom stats formula rnorm

CreateDataframe <- function(mat, row.data, col.data, nLVs) {
  nrows <- nrow(mat)
  ncols <- ncol(mat)
  if(!is.null(row.data)) {
    if(nrows!=nrow(row.data)) stop("mat should have same number of rows as row.data")
  }
  if(!is.null(col.data)) {
    if(ncols!=nrow(col.data)) stop("mat should have same number of columns as rows of col.data")
  }

  data.stub <- data.frame(Y = c(mat),
                          RowInd = rep(1:nrows, times=ncols),
                          ColInd = rep(1:ncols, each=nrows))

  # Create data frame with row & column effects,
  # RowInd <- matrix(data.stub$RowInd, nrow=nrow(data.stub), ncol=nLVs, byrow=FALSE)
  # colnames(RowInd) <- paste0("Row", 1:nLVs)
  #
  # ColInd <- matrix(data.stub$ColInd, nrow=nrow(data.stub), ncol=nLVs, byrow=FALSE)
  # colnames(ColInd) <- paste0("Col", 1:nLVs)

  # Create scores with initial values. Improve this later...
  RowScore.sim <- matrix(stats::rnorm(nLVs*nrows, 0, 1), ncol=nLVs)
  ColScore.sim <- matrix(stats::rnorm(nLVs*ncols, 0, 1), ncol=nLVs)
  Scores <- cbind(RowScore.sim[data.stub$RowInd,], ColScore.sim[data.stub$ColInd,])
  colnames(Scores) <- c(paste0("RowScore", 1:nLVs), paste0("ColScore", 1:nLVs))

  # Create data frame with row & column covariates. These are passed into the models,
  data.cov <- data.frame(row.data[data.stub$RowInd,], col.data[data.stub$ColInd,], check.rows = FALSE)
  CovNames <- list(Row=names(row.data), Col=names(col.data))
  names(data.cov) <- c(names(row.data), names(col.data))

  # add covariates for each LV, which are passed into the models
  if(nrow(data.cov)>0) {
    list.covs <- sapply(1:nLVs, function(lv, dcov) {
      names(dcov) <- paste0(names(dcov), ".LV", lv)
      dcov
    }, dcov=data.cov, simplify=FALSE)
    data.covs <- do.call(cbind, list.covs)
  } else {
    data.covs <- NULL
  }

  GetNames <- function(dat) {
    if(is.null(dat)) {
      res <- NULL
    } else {
      res <- names(apply(dat, 2, is.numeric))
    }
    res
  }

  NumericCovs <- list(Row=GetNames(row.data), Col=GetNames(col.data))
  FactorCovs <- list(Row=names(row.data[!names(row.data)%in%NumericCovs$Row]),
                     Col=names(col.data[!names(col.data)%in%NumericCovs$Col]))

  #  Create data frame with weights for factors
  MakeWt <- function(nm, score) {
    wts <- score
    colnames(wts) <- paste0("Wt.", nm, ".LV", 1:ncol(wts))
    wts
  }
  # This produces a NULL if there are no row/column factors
  RowWts <- sapply(FactorCovs$Row, MakeWt, score= Scores[,grep("^Row", colnames(Scores))], simplify=FALSE)
  ColWts <- sapply(FactorCovs$Col, MakeWt, score= Scores[,grep("^Col", colnames(Scores))], simplify=FALSE)
  data.rowwt <- do.call(rbind, RowWts)
  data.colwt <- do.call(rbind, ColWts)
  data.covwt <- cbind(data.rowwt, data.colwt)

  # Create data frame with dummy covariates for residuals
  data.res <- data.frame(
    replicate(n=nLVs, data.stub$RowInd),
    replicate(n=nLVs, data.stub$ColInd),
    Scores
  )
  names(data.res) <- c(paste0("Roweps.LV", 1:nLVs), paste0("Coleps.LV", 1:nLVs),
                       paste0("Wt.Roweps.LV", 1:nLVs), paste0("Wt.Coleps.LV", 1:nLVs)
  )
  if(!is.null(data.covwt)) data.res <- cbind(data.covwt, data.res)

  data <- cbind(data.stub, Scores, data.res)
  if(nrow(data.cov)==nrow(data))  data <- cbind(data, data.cov)
  if(!is.null(data.covs))  data <- cbind(data, data.covs)


  Names <- list(Response = names(data.stub)[!grepl("Ind", names(data.stub))],
                RowColInds = names(data.stub)[grepl("Ind", names(data.stub))],
                NumericCovs = NumericCovs,
                FactorCovs = FactorCovs,
                Score = colnames(Scores),
                Covs = colnames(data.covs),
                eps = list(Row = names(data.res)[grep("^Roweps", names(data.res))],
                           Col = names(data.res)[grep("^Coleps", names(data.res))]),

                Weights  = list(Row = c(colnames(data.rowwt),
                                        names(data.res)[grep("Wt.Row", names(data.res))]),
                                Col = c(colnames(data.colwt),
                                        names(data.res)[grep("Wt.Col", names(data.res))])
                ),
                LVCovs  = list(Row = colnames(data.covs)[grep("Row", colnames(data.covs))],
                               Col = colnames(data.covs)[grep("Col", colnames(data.covs))])
  )

  list(data=data, Names=Names, NRows = nrows, NCols = ncols, nLVs = nLVs)

}
