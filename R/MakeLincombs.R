#' make a linear combination for LVs
#'
#' @param covs covariates
#' @param lvname Latent variable name. Defaults to "LV1",
#' @param lcname Linear combination name. Defaults to "lc"
#' @param epsname Residual error name. Defaults to "eps",
#' @param N Nu,nber of rows. Defaulta to NULL
#'
#' @details make linear combinations for LVs
#'
#' @return Linear combination from INLA
#'
#' @examples
#' \dontrun{
#' MakeLincomb(covs=NULL, lvname="LV1", lcname="lc", epsname="eps", N=3)
#' }
#' @export
#'@importFrom INLA inla.make.lincombs

MakeLincomb <- function(covs, lvname="LV1", lcname="lc", epsname="eps", N=NULL) {
  if(!is.null(covs)) {
    if(!is.data.frame(covs)) stop("covs must be a data frame")
    names(covs) <- paste(names(covs), lvname, sep=".")
    N <- nrow(covs)
  }
  epsname <- paste(epsname, lvname, sep=".")
  eps <- diag(rep(1, N))
  Args <- covs
  Args[[epsname]] <- eps

  LinComb <- INLA::inla.make.lincombs(Args)

  names(LinComb) <- paste0(lcname, 1:N, lvname)
  LinComb
}

#' make linear combinations for LVs
#'
#' @param covar.df Data frame of covaraites. Can be NULL, but N must be specified
#' @param nlv Number of latent variables. Defaults to 2
#' @param EpsName Name for residual error. Defaults to "eps"
#' @param N Number of rows, if covar.df is NULL
#'
#' @details make linear combinations for LVs
#'
#' @return Linear combinations from INLA
#'
#' @examples
#' MakeLincombs(covar.df=NULL, nlv=2, EpsName="eps", N=3)
#' @export
#' @import INLA
#'@importFrom INLA inla.make.lincombs

MakeLincombs <- function(covar.df, nlv=2, EpsName="eps", N=NULL) {
  if(is.null(covar.df) & is.null(N)) stop("One of covar.df & N must be not NULL")
  LCs <- sapply(1:nlv, function(l, CV) MakeLincomb(covs=CV,
                                                   lvname=paste0("LV", l),
                                                   lcname=paste0("Row.lc", l),
                                                   epsname=EpsName, N=N),
                CV=covar.df)

  LCs.v <- c(LCs)
  names(LCs.v) <- paste0("lc", rep(1:nrow(LCs), times=ncol(LCs)),
                         ".LV", rep(1:ncol(LCs), each=nrow(LCs)))
  LCs.v
}

