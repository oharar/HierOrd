#' simulate linear combinations.
#'
#' @param mod INLA Model to simulate from
#' @param n Number of simulations, defaluts to 1.
#'
#' @details Simulates LVs
#'
#' @return Matrix of LVs
#'
#' @examples
#' \dontrun{
#' SimLinComb(mod, n=1)
#' }
#' @export
#'@importFrom stats formula
#'@importFrom utils unstack
#'@importFrom INLA inla.rmarginal

SimLinComb <- function(mod, n=1) {
  Sim <- unlist(lapply(mod$marginals.lincomb.derived,
                       function(lc, N) INLA::inla.rmarginal(N, lc), N=n))
  l1 <- gsub(".*lc", "", names(Sim))
  lvs <- factor(gsub(".*\\.LV", "", l1))
  Sim.df <- data.frame(Sim=Sim, lvs=lvs)
  Sim.mat <- utils::unstack(Sim.df, form = Sim ~ lvs)
  rownames(Sim.mat) <- gsub("\\..*", "", rownames(Sim.df)[grep("LV11?$", rownames(Sim.df))])
  colnames(Sim.mat) <- gsub(".*\\.", "", rownames(Sim.df)[grep("lc1\\.", rownames(Sim.df))])

  Sim.mat
}

