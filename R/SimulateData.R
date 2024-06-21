#' Function to simulate data from a hierarchical ordination model, assuming a Poisson distribution
#'
#' @param NRows Number of rows, defaults to 10
#' @param NCols Number of rows, defaults to 5
#' @param NLVs Number of latent variables, defaults to 1
#' @param ColVar Vector of Column effects
#' @param RowVar Variance of row effects
#' @param Sigma1 Scale for latent variables: standard deviation
#' @param Intercept1 Intercept
#'
#' @details This simulates data from a hierarchical ordination, assuming a Poisson distributed response.
#'
#' @return A list with elements Counts (response), RowCov (row covariates), and ColCov (column covariates)
#'
#' @examples
#' SimulateData(NRows = 10, NCols = 5, NLVs = 1,
#'                          ColVar=0, RowVar=0 ,Sigma1=0.1, Intercept1=1)
#' @export
#'@importFrom stats rnorm rpois

SimulateData <- function(NRows = 10, NCols = 5, NLVs = 1,
                         ColVar, RowVar, Sigma1, Intercept1) {
  if(length(ColVar)!=NLVs) stop("ColVar should have NLVs elements")
  if(length(RowVar)!=NLVs) stop("RowVar should have NLVs elements")

  ColCov1 <- stats::rnorm(NCols, 0, 1)
  RowCov1 <- stats::rnorm(NRows, 0, 1)

  ColScores1 <- SimTrueScores(eff=ColVar, covs=ColCov1, scale=TRUE)
  RowScores1 <- SimTrueScores(eff=RowVar, covs=RowCov1, scale=TRUE)

  eta.m <- Intercept1 + Sigma1*RowScores1%*%t(ColScores1)
  Counts <- apply(eta.m, 2, function(v) stats::rpois(length(v), exp(v)))

  RowCov <- data.frame(RowCovariate = RowCov1)
  ColCov <- data.frame(ColCovariate = ColCov1)

  list(Counts=Counts, RowCov=RowCov, ColCov=ColCov,
       TrueRowScores = RowScores1, TrueColScores = ColScores1)
}


#' Simulate true row/column scores
#'
#' @param eff Proportion of each LV explained by covariates (I think!). Length equals number of LVs
#' @param covs Covaraites
#' @param scale Should the results be scale to unit variance? Defaults to FALSE
#' @details This simulates row or column scores.
#'
#' @return A matrix of scores, with some rows and columns. This is used in SimulateData()
#' If scale=TRUE, the standard deviation before scaling is set to be the attribute StdDev
#'
#' @examples
#'\dontrun{
#' SimTrueScores(eff, covs, scale=FALSE)
#'}
#' @export
#'@importFrom stats formula sd rnorm

SimTrueScores <- function(eff, covs, scale=FALSE) {
  nlv <- length(eff)
  ncol <- length(covs)
  CovEff <- covs%o%eff
  ResEff <- sweep(matrix(stats::rnorm(nlv*ncol, 0, 1), ncol=nlv), 2, sqrt(1-eff^2), "*")

  Score <- CovEff + ResEff
  if(scale) {
    SD <- apply(Score, 2, stats::sd)
    Score <- apply(Score, 2, scale)
    attr(Score, "StdDev") <- SD
  }

  Score
}
