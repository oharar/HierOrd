#' Function to fit a hierarchical ordination
#'
#' @param Y the matrix of data
#' @param rowdata row covariates
#' @param coldata column covariates
#' @param nLVs number of latent variables, defaults to 1,
#' @param family family
#' @param EpsHyper distributon of hyperparameters for residual variation, Defaults to "list(prec=list(prior = \"pc.prec\", param = c(1, 0.0001)))",
#' @param NBurnin Number of iterations as burnin, default 20
#' @param NChains Number of chains to run, default 1
#' @param NCores Number of cores to use if more than one chain is run. If NULL (the default),
#' set to equal NChains.
#' @param parallel Should the MCMC be paralellised if there is more than 1 chain? Defaults to TRUE
#' @param NIter Number of iterations after burnin, default 100
#' @param INLAobj Logical, should the INLA objects be kept? Default FALSE
#'
#' @details Fit.a hierarchical model
#'
#' Parallelisation is done one chain per cluster.
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
#'@importFrom parallel makeCluster
#'@importFrom parallel clusterExport
#'@importFrom parallel parLapply

FitHierOrd <- function(Y, rowdata, coldata, nLVs = 1, family,
                       RowCovHyper = "list(prec=list(prior = \"pc.prec\", param = c(1, 0.0001)))",
                       EpsHyper = "list(prec=list(prior = \"pc.prec\", param = c(1, 0.0001)))",
                       NBurnin = 20, NIter = 100, NChains = 1, NCores=NULL, parallel=TRUE,
                       INLAobj=FALSE) {

# format data
  data <- CreateDataframe(mat=Y, row.data=rowdata, col.data=coldata, nLVs=nLVs)

# Create row and column formulae
  row.formula <- MakeFormula(nlv=nLVs, linnames=data$Names$LVCovs$Row, epsname="Row",
                             factprior = RowCovHyper, epsprior = EpsHyper)
  col.formula <- MakeFormula(nlv=nLVs, linnames=data$Names$LVCovs$Col, epsname="Col",
                             factprior = RowCovHyper, epsprior = EpsHyper)

  RowLinComb <- MakeLincombs(covar.df =NULL, EpsName = "Roweps", nlv =nLVs, N=data$NRows)
  ColLinComb <- MakeLincombs(covar.df =NULL, EpsName = "Coleps", nlv = nLVs, N=data$NCols)

  # which row to fix sign (use maximum)
#  RowSign <- which(SimDat$TrueRowScores==max(SimDat$TrueRowScores))
#  ColSign <- which(SimDat$TrueColScores==max(SimDat$TrueColScores))

  # One chain for now, should do more chains and parallelise
  if(NChains==1) {
    res <- RunMCMC(data=data, row.form=row.formula, col.form=col.formula,
                   rowLinComb=RowLinComb, colLinComb=ColLinComb,
                   nBurnin=NBurnin, nIter=NIter, family=family, inlaobj=INLAobj)
  } else {
    if(is.null(NCores)) NCores <- NChains
    if(parallel) {
    cl <- parallel::makeCluster(NChains)
#    parallel::clusterSetRNGStream(cl, 123)
    parallel::clusterExport(cl, varlist=c("RunMCMC"))
    res1 <- parallel::parLapply(cl, 1:NChains, function(ch, ...) { RunMCMC(...) },
                                data=data, row.form=row.formula, col.form=col.formula,
                                rowLinComb=RowLinComb, colLinComb=ColLinComb,
                                nBurnin=NBurnin, nIter=NIter, family=family, inlaobj=INLAobj)
    parallel::stopCluster(cl)
    } else {
      res1 <- lapply(1:NChains, function(ch, ...) { RunMCMC(...) },
                                  data=data, row.form=row.formula, col.form=col.formula,
                                  rowLinComb=RowLinComb, colLinComb=ColLinComb,
                                  nBurnin=NBurnin, nIter=NIter, family=family, inlaobj=INLAobj)
    }

# Should probably format so that the mcmc objects are in an mcmc.list, nad INLA objects in a different list.
    res <- list(
      row = as.mcmc.list(lapply(res1, function(l) l$row.mcmc)),
      col = as.mcmc.list(lapply(res1, function(l) l$col.mcmc))
    )
    if(INLAobj) {
      res$rowinla <- do.call(list, lapply(res1, function(l) l$RowINLA))
      res$colinla <- do.call(list, lapply(res1, function(l) l$ColINLA))
    }
    # res <- res1
  }
  res
}




