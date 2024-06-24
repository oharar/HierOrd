#' Function to fit a hierarchical ordination
#'
#' @param Y the matrix of data
#' @param rowdata row covariates
#' @param coldata column covariates
#' @param nLVs number of latent variables, defaults to 1,
#' @param family family
#' @param EpsHyper distributon of hyperparameters for residual variation, Defaults to "list(prec=list(prior = \"pc.prec\", param = c(1, 0.0001)))",
#' @param NBurnin Number of iterations as burnin, default 20
#' @param NIter Number of iterations after burnin, default 100
#' @param INLAobj Logical, should the INLA objects be kept? Default FALSE
#'
#' @details Fit.a hierarchical model
#'
#' @return a list (more later...)
#'
#' @examples
#'\dontrun{
#'UpdateScores(scores, data, rowcol, names)
#'}
#'
#' @export
#'@importFrom stats formula

FitHierOrd <- function(Y, rowdata, coldata, nLVs = 1, family,
                       RowCovHyper = "list(prec=list(prior = \"pc.prec\", param = c(1, 0.0001)))",
                       EpsHyper = "list(prec=list(prior = \"pc.prec\", param = c(1, 0.0001)))",
                       NBurnin = 20, NIter = 100, INLAobj=FALSE) {

# format data
  data <- CreateDataframe(mat=Y, row.data=rowdata, col.data=coldata, nLVs=nLVs)

# Create row and column formulae
  row.formula <- MakeFormula(nlv=nLVs, linnames=NULL, epsname="Row",
                             factprior = RowCovHyper, epsprior = EpsHyper)
  col.formula <- MakeFormula(nlv=nLVs, linnames=NULL, epsname="Col",
                             factprior = RowCovHyper, epsprior = EpsHyper)

  RowLinComb <- MakeLincombs(covar.df =NULL, EpsName = "Roweps", nlv =nLVs, N=data$NRows)
  ColLinComb <- MakeLincombs(covar.df =NULL, EpsName = "Coleps", nlv = nLVs, N=data$NCols)

  # which row to fix sign (use maximum)
#  RowSign <- which(SimDat$TrueRowScores==max(SimDat$TrueRowScores))
#  ColSign <- which(SimDat$TrueColScores==max(SimDat$TrueColScores))

  # This isn't WORKING!!!!!!!

  # One chain for now, should do more chains and parallelise
  res <- RunMCMC(data=data, row.form=row.formula, col.form=col.formula,
                 rowLinComb=RowLinComb, colLinComb=ColLinComb,
                 nBurnin=NBurnin, nIter=NIter, family=family, inlaobj=INLAobj)
  res
}




