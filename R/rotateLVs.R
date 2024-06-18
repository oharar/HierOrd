#' rotate a latent variable
#'
#' @param mat matrix to rotate
#' @param rotation.fn Function in GPArotation to calculate rotation, or svd. Defaults to svd.
#' @param scale Should rotated matrix be scaled? Defaults to FALSE
#'
#' @details Function to rotate a latent variable, defaults to Varimax. Methods in GPArotation package
#'
#' @return Rotated latenr variables
#'
#' @examples
#' \dontrun{
#' rotateLVs <- function(matrix(1:6, ncol=2), rotation.fn="svd", scale=FALSE)
#' }
#' @export
#'@importFrom stats sd
#'@import GPArotation

rotateLVs <- function(mat, rotation.fn="svd", scale=FALSE) {
  RowN <- rownames(mat)
  ColN <- colnames(mat)
  if(rotation.fn!="svd"){
    if (!requireNamespace("GPArotation", quietly = TRUE)) {
      stop(
        "Package \"GPArotation\" must be installed to use this function.",
        call. = FALSE
      )
    }
    rot  <- do.call(rotation.fn, list(A=mat)) # rotate
    l.r <- list(Th=rot$Th, loadings = rot$loadings)
  }else{
    rot <- svd(mat)$v
    l.r <- list(Th=rot, loadings = as.matrix(mat)%*%rot)
  }
  if(scale) {
    l.r$sd <- apply(l.r$loadings, 2, stats::sd)
    l.r$loadings <- apply(l.r$loadings, 2, scale)
  }
  rownames(l.r$loadings) <- RowN
  colnames(l.r$loadings) <- ColN
  l.r
}
