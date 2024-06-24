#' Function to update latent variables
#'
#' @param scores Scores
#' @param data Data frame
#' @param rowcol Row or column? Must be either "Row" or "Col"
#' @param names Names
#'
#' @details update latent variables, i.e. import new LV and then update weights
#'
#' @return An updated data frame
#'
#' @examples
#'\dontrun{
#'UpdateScores(scores, data, rowcol, names)
#'}
#'
#' @export
#'@importFrom stats formula
#'@importFrom graphics abline points text

UpdateScores <- function(scores, data, rowcol, names) {
  if(!rowcol%in%c("Row", "Col")) stop("rowcol must be either Row or Col")
  if(is.vector(scores)) scores <- data.frame(scores)
  nlv <- length(names$eps$Row)
  scorenames <- paste0(rowcol, "Score")
  Wtnames <- names$Weights[[rowcol]]
  if(length(names$eps[[rowcol]])!=ncol(scores)) stop("number of columns of scores not equal to number of latent variables")

  # Add new scores
  data[,grep(scorenames, names(data))] <- scores[data[,paste0(rowcol, "Ind")],]

  # update weights
  wtnm <- names$Weights[[rowcol]]
  for(l in 1:nlv) {
    data[,wtnm[grep(paste0(l,"$"), wtnm)]] <- scores[,l]
  }

  # update continuous covariates
  nm <- names$LVCovs[[rowcol]]
  for(l in 1:nlv) {
    nml <- nm[grep(paste0(l,"$"), nm)]
    data[,nml] <- scores[,l]*data[,gsub(paste0(".LV", l), "", nml)]
  }
  data
}
