#' Function to simulate data from a hierarchical ordination model, assuming a Poisson distribution
#'
#' @param NRows Number of rows, defaults to 10
#' @param NCols Number of rows, defaults to 5
#' @param NLVs Number of latent variables, defaults to 1
#' @param ColEff Vector of Column effects
#' @param RowEff Variance of row effects
#' @param Sigma Scale for latent variables: standard deviation
#' @param Intercept1 Intercept
#'
#' @details This simulates data from a hierarchical ordination, assuming a Poisson distributed response.
#'
#' @return A list with elements Counts (response), RowCov (row covariates), and ColCov (column covariates)
#'
#' @examples
#' SimulateData(NRows = 10, NCols = 5, NLVs = 1,
#'                          ColEff=0, RowEff=0 ,Sigma=0.1, Intercept=1)
#' @export
#'@importFrom stats rnorm rpois

SimulateData <- function(NRows = 10, NCols = 5, NLVs = 1,
                         ColEff, RowEff, Sigma, Intercept) {
  if(length(ColEff)!=NLVs) stop("ColVar should have NLVs elements")
  if(length(RowEff)!=NLVs) stop("RowVar should have NLVs elements")

  ColCov <- stats::rnorm(NCols, 0, 1)
  RowCov <- stats::rnorm(NRows, 0, 1)

  ColScores <- SimTrueScores(eff=ColEff, covs=ColCov, scale=TRUE)
  RowScores <- SimTrueScores(eff=RowEff, covs=RowCov, scale=TRUE)

  eta.m <- Intercept + Sigma*RowScores%*%t(ColScores)
  Counts <- apply(eta.m, 2, function(v) stats::rpois(length(v), exp(v)))

  RowCov <- data.frame(RowCovariate = RowCov)
  ColCov <- data.frame(ColCovariate = ColCov)

  list(Counts=Counts, RowCov=RowCov, ColCov=ColCov,
       TrueRowScores = RowScores, TrueColScores = ColScores,
       pars=list(NRows = NRows, NCols = NCols, NLVs = NLVs, ColEff = ColEff,
                 RowEff = RowEff, Sigma = Sigma, Intercept = Intercept))
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
