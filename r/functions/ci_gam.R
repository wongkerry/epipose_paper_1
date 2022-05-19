ci_gam <- function(mod, level = 0.95) {
  msum <- mgcv:::summary.gam(mod)
  dt <- as.data.table(msum$p.table)
  dt <- dt[, 1:2]
  names(dt) <- c("est", "se")
  dt$coef <- row.names(msum$p.table)
  nu <- msum$residual.df
  
  dt[ , lci := est + se * qt(df = nu, p = (1 - level) / 2)]
  dt[ , uci := est + se * qt(df = nu, p = 1 - (1 - level) / 2)]
  dt
  return(dt)
}