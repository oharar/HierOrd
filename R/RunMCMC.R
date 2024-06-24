#' Function to run one chain of MCMC
#'
#' @param data data (full object)
#' @param row.form row formula
#' @param col.form column formula
#' @param rowLinComb linear combinationsd for rows
#' @param colLinComb linear combinationsd for columns
#' @param nBurnin number of iterations as burn in
#' @param nIter number of iterations to keep, after burn in
#' @param family family, passed to inla()
#' @param inlaobj Should the INLA objects be returned? Defaults to FALSE
#'
#' @details This runs the MCMC for one chain: it should be an internal function, I guess.
#'
#' @return A list. Details to follow
#'
#' @examples
#'\dontrun{
#'UpdateScores(scores, data, rowcol, names)
#'}
#'
#' @export
#'@importFrom INLA inla
#'@importFrom coda mcmc


RunMCMC <- function(data, row.form, col.form, rowLinComb, colLinComb,
                    nBurnin, nIter, family, inlaobj=FALSE) {

  # Matrices to store the row and column effects.
  RowRes <- matrix(NA, nrow=nIter, ncol=data$nLVs*data$NRows)
  colnames(RowRes) <- paste0("Row", rep(1:data$NRows, times=data$nLVs),
                             "lv", rep(1:data$nLVs, each=data$NRows))

  ColRes <- matrix(NA, nrow=nIter, ncol=data$nLVs*data$NCols)
  colnames(ColRes) <- paste0("Col", rep(1:data$NCols, times=data$nLVs),
                             "lv", rep(1:data$nLVs, each=data$NCols))
# Vector to store INLA objects, if inlaobj is TRUE
  if(inlaobj) {
    RowObjs <- ColObjs <- vector("list", nIter)
  }

  for(i in 1:(nBurnin+nIter)) {
    ## Fit row model, with col fixed
    mod.row <- inla(row.form, data=data$data, family=family,
                    lincomb = rowLinComb)

    # simulate row effects
    SimRow <- SimLinComb(mod.row)
    SimRow.r <- rotateLVs(SimRow, scale = TRUE)$loadings #*sign(SimRow[RowSign,]))

    # update row effects
    data.update <- UpdateScores(scores=SimRow.r,
                                data=data$data, rowcol="Row", names=data$Names)
    ### Fit col model, with row fixed
    mod.col <- inla(col.form, data=data$data, family=family,
                    lincomb = colLinComb)

    # simulate col effects
    SimCol <- SimLinComb(mod.col)
    # Force the sign constraint
    SimCol.r <- rotateLVs(SimCol, scale = TRUE)$loadings #*sign(SimCol[ColSign,])

    # update col effects
    data.update <- UpdateScores(scores=SimCol.r,
                                data=data$data, rowcol="Col", names=data$Names)


    if(i > nBurnin) {
      RowRes[i - nBurnin, ]  <- c(SimRow.r)
      ColRes[i - nBurnin, ]  <- c(SimCol.r)
      if(inlaobj) {
        ColObjs[[i - nBurnin]] <- mod.col
        RowObjs[[i - nBurnin]] <- mod.row
      }
    }
  }
  if(inlaobj) {
    res <- list(row.mcmc = mcmc(RowRes, start=nBurnin+1, end=nBurnin+nIter, thin=1),
                col.mcmc = mcmc(ColRes, start=nBurnin+1, end=nBurnin+nIter, thin=1),
                ColINLA = ColObjs, RowINLA = RowObjs)
  } else {
    res <- list(row.mcmc = mcmc(RowRes, start=nBurnin+1, end=nBurnin+nIter, thin=1),
                col.mcmc = mcmc(ColRes, start=nBurnin+1, end=nBurnin+nIter, thin=1))
  }
  res
}
