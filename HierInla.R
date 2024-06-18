library(HierOrd)
library(INLA)
library(coda)


set.seed(42)
SimDat <- SimulateData(NRows = 100, NCols = 50, Sigma1 = 0.25, NLVs = 1,
                       ColEff1 = 1, RowEff1 = 1,  Intercept1 = 4)
# hist(SimDat$Counts)

nLVs <- 1
Family = "Poisson"
LVdata <- CreateDataframe(mat=SimDat$Counts, row.data=SimDat$RowCov, col.data=SimDat$ColCov, nLVs=nLVs)

# Update row & column effects

#ColScore.sim <- matrix(rnorm(nLVs*LVdata$NCols, 0, 1), ncol=nLVs)
ColScore.sim <- matrix(sort(rnorm(nLVs*LVdata$NCols, 0, 1)), ncol=nLVs)
colnames(ColScore.sim) <- paste0("ColScore", 1:nLVs)
# RowScore.sim <- matrix(rnorm(nLVs*LVdata$NRows, 0, 1), ncol=nLVs)
RowScore.sim <- matrix(sort(rnorm(nLVs*LVdata$NRows, 0, 1)), ncol=nLVs)
colnames(RowScore.sim) <- paste0("RowScore", 1:nLVs)

datanew <- UpdateScores(scores = ColScore.sim, data=LVdata$data,
                        rowcol="Col", names=LVdata$Names)
datanew <- UpdateScores(scores = RowScore.sim, data=LVdata$data,
                        rowcol="Row", names=LVdata$Names)

# rbind(LVdata$data[1,], datanew[1,])

## Create row and column formulae

 RowCovHyper <- list(prior = "pc.prec", param = c(1, 0.1))
 EpsHyper <- "list(prec=list(prior = \"pc.prec\", param = c(1, 0.1)))"

row.formula <- MakeFormula(nlv=nLVs, linnames=NULL, epsname="Row",
                           factprior = RowCovHyper, epsprior = EpsHyper)
col.formula <- MakeFormula(nlv=nLVs, linnames=NULL, epsname="Col",
                           factprior = RowCovHyper, epsprior = EpsHyper)

# row.formula <- MakeFormula(nlv=nLVs, linnames=names(SimDat$RowCov), epsname="Row")
# col.formula <- MakeFormula(nlv=nLVs, linnames=names(SimDat$ColCov), epsname="Col")

# Add linear combinations
RowLinComb <- MakeLincombs(covar.df =NULL, EpsName = "Roweps", nlv =nLVs, N=LVdata$NRows)
ColLinComb <- MakeLincombs(covar.df =NULL, EpsName = "Coleps", nlv = nLVs, N=LVdata$NCols)
# RowLinComb <- MakeLincombs(covar.df =SimDat$RowCov, EpsName = "Roweps", nlv =nLVs)
# ColLinComb <- MakeLincombs(covar.df =SimDat$ColCov, EpsName = "Coleps",nlv = nLVs)


### Fit col model, with row fixed
mod.col <- inla(col.formula, data=LVdata$data, family=Family,
                lincomb = ColLinComb)

# simulate col effects
SimCol <- SimLinComb(mod.col)
SimCol.r <- rotateLVs(SimCol, scale = TRUE)

pairs(cbind(mod.col$summary.lincomb.derived$mean, SimCol$LV1, SimCol.r$loadings))

# update col effects
data.update <- UpdateScores(scores=SimCol.r$loadings,
                            data=LVdata$data, rowcol="Col", names=LVdata$Names)

### Fit row model, with col fixed
mod.row <- inla(row.formula, data=data.update, family=Family,
                lincomb = RowLinComb)

# simulate row effects
SimRow <- SimLinComb(mod.row)
SimRow.r <- rotateLVs(SimRow, scale = TRUE)

pairs(cbind(mod.row$summary.lincomb.derived$mean, SimRow$LV1, SimRow.r$loadings))


# update row effects
data.update <- UpdateScores(scores=SimRow.r$loadings,
                            data=data.update, rowcol="Row", names=LVdata$Names)


NBurnin <- 100
NIter <- 500

# temporary data
data.update <- LVdata$data
ResMat <- matrix(NA, nrow=NIter, ncol=length(c(SimRow.r$loadings))+length(c(SimCol.r$loadings)))
# c(c(SimCol.r$loadings), c(SimRow.r$loadings))
ResRowN <- paste0("Row", rep(1:nrow(SimRow.r$loadings), times=ncol(SimRow.r$loadings)),
                  "lv", rep(1:ncol(SimRow.r$loadings), each=nrow(SimRow.r$loadings)))
ResColN <- paste0("Col", rep(1:nrow(SimCol.r$loadings), times=ncol(SimCol.r$loadings)),
                  "lv", rep(1:ncol(SimCol.r$loadings), each=nrow(SimCol.r$loadings)))
colnames(ResMat) <- c(ResRowN, ResColN)

inlaobj <- TRUE
RowObjs <- ColObjs <- vector("list", NIter)

# Run MCMC (finally)
system.time(
for(i in 1:(NBurnin+NIter)) {
  ### Fit col model, with row fixed
  mod.col <- inla(col.formula, data=data.update, family=Family,
                  lincomb = ColLinComb)

  # simulate col effects
  SimCol <- SimLinComb(mod.col)
  SimCol.r$loadings <- scale(unlist(SimCol))
#  SimCol.r <- rotateLVs(SimCol, scale = TRUE)

  # update col effects
  data.update <- UpdateScores(scores=SimCol.r$loadings,
                              data=data.update, rowcol="Col", names=LVdata$Names)


  ### Fit row model, with col fixed
  mod.row <- inla(row.formula, data=data.update, family=Family,
                  lincomb = RowLinComb)

  # simulate row effects
  SimRow <- SimLinComb(mod.row)
#  SimRow.r$loadings <- unlist(SimRow)
  SimRow.r <- rotateLVs(SimRow, scale = TRUE)

  # update row effects
  data.update <- UpdateScores(scores=SimRow.r$loadings,
                              data=data.update, rowcol="Row", names=LVdata$Names)

  # Need to update this
  if(i > NBurnin) {
    ResMat[i - NBurnin, ]  <- c(c(SimCol.r$loadings), c(SimRow.r$loadings))
    if(inlaobj) {
      ColObjs[[i - NBurnin]] <- mod.col
      RowObjs[[i - NBurnin]] <- mod.row
    }
  }
  if(inlaobj) {
    res <- list(mcmc = mcmc(ResMat, start=NBurnin+1, end=NBurnin+NIter, thin=1),
                ColINLA = ColObjs, RowINLA = RowObjs)
  } else {
    res <- mcmc(ResMat)
  }
}
)

 res1 <- res
# res2 <- res

 plot(res1$mcmc[,2])
 plot(res2$mcmc[,2])

library(mcmcplots)
 plot(res$mcmc[,grep("Row", colnames(res$mcmc))])
 caterplot(res$mcmc[,grep("Row", colnames(res$mcmc))])
 caterplot(res$mcmc[,grep("Col", colnames(res$mcmc))])

plot(c(res1$mcmc[,"Row1lv1"]), type="l")
lines(c(res2$mcmc[,"Row53lv1"]), col=2)


Summ <- summary(res$mcmc)
Means <- Summ$statistics[,"Mean"]

plot(ColScore.sim, Means[grep("Col", names(Means))])
plot(RowScore.sim, Means[grep("Row", names(Means))])


