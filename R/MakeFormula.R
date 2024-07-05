#' make formulae for row & column effects
#'
#' @param nlv Number of latent variables
#' @param linnames names of variables that will be linear, defaults to NULL,
#' @param factnames names of variables that will be factors, defaults to NULL
#' @param factprior prior for hyperparameters for precision of factors. Defaults to "list(prec = \"loggamma\", param = c(1, 1), initial = 1, fixed = TRUE)",
#' @param epsprior prior for hyperparameters for residual precision. Defaults to "list(prior = \"pc.prec\", param = c(1, 0.01))",
#' @param epsname Name of residual error
#'
#' @details Writes a formula for row or column effects. Adds weight terms etc.
#'
#' @return A formula
#'
#' @examples
#' MakeFormula(nlv=1, linnames=NULL, factnames=NULL, epsname="")
#' @export
#'@importFrom stats formula
#'@importFrom graphics abline points text

MakeFormula <- function(nlv, linnames=NULL, factnames=NULL,
                        #                        linprior="list(theta = list(prec = \"loggamma\", param = c(1, 1), initial = 1, fixed = TRUE)",
                        factprior="list(theta = list(prec = \"loggamma\", param = c(1, 1), initial = 1, fixed = TRUE)",
                        epsprior="list(prior = \"pc.prec\", param = c(1, 0.01))",
                        epsname="") {
#  lnames <- c(sapply(linnames, paste0, ".LV", 1:nlv))
  lnames <- linnames
  fnames <- c(sapply(factnames, paste0, ".LV", 1:nlv))
  enames <- paste0(epsname, "eps.LV", 1:nlv)

  lform <- sapply(lnames, function(nm) paste0("f(", nm, ", model=\"linear\", mean.linear=0, prec.linear=1)"))
  fform <- sapply(fnames, function(nm) paste0("f(", nm, ", Wt.", nm,
                                              ", model=\"iid\", hyper=list(theta = ", factprior, ")"))
  eform <- paste0("f(", epsname, "eps.LV", 1:nlv, ", Wt.", epsname, "eps.LV", 1:nlv,
                  ", model=\"iid\", hyper=list(prec = ", epsprior, "))")

  res <- paste0("Y ~ ", paste(c(lform, fform, eform), collapse =" + "))
  stats::formula(res)
}
#    MakeFormula(nlv=2, datanames=LETTERS[1:3], epsname="aa")
