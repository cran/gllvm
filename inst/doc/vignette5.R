## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size="footnotesize", fig.width=5, fig.height=5, fig.align="center",dev="png", code.frame = TRUE, warning = FALSE, fig.pos='H')

## ----message=F, warning=F-----------------------------------------------------
library(gllvm)
data("spider", package = "mvabund")
Y <- spider$abund

## ----echo=FALSE---------------------------------------------------------------
load(file = "ftEqTol.RData")
load(file = "ftComTol.RData")
load(file = "ftUneqTol.RData")
ftEqTol$col.eff$col.eff <- ftComTol$col.eff$col.eff <- ftUneqTol$col.eff$col.eff <- FALSE

## ----eval = FALSE-------------------------------------------------------------
#  ftEqTol <- gllvm(Y, family = "poisson", row.eff = "random", num.lv = 2)

## ----eval = FALSE-------------------------------------------------------------
#  ftComTol <- gllvm(Y, family = "poisson", num.lv = 2, quadratic = "LV")

## ----eval = FALSE-------------------------------------------------------------
#  ftUneqTol <- gllvm(Y, family = "poisson", num.lv = 2, quadratic = TRUE)

## -----------------------------------------------------------------------------
AICc(ftEqTol,ftComTol,ftUneqTol)

## -----------------------------------------------------------------------------
#Species optima for LVs
optima(ftUneqTol)

#Species tolerances
tolerances(ftUneqTol)

## -----------------------------------------------------------------------------
#Residual variance per latent variable
#for the linear term
getResidualCov(ftUneqTol)$var.q

#for the quadratic term
getResidualCov(ftUneqTol)$var.q2

## ----quad_plot----------------------------------------------------------------
ordiplot(ftUneqTol, biplot=TRUE, spp.arrows = TRUE)

## ----grad_length--------------------------------------------------------------
# Extract tolerances
tol <- tolerances(ftComTol, sd.errors = FALSE)
gradLength <- 4/tol[1,]
turn <- 2*qnorm(.999, sd = tol[1,])

## ----grad_length_res----------------------------------------------------------
cat("Gradient length:", gradLength)

## ----turn---------------------------------------------------------------------
cat("Turnover rate:", turn)

## ----curves, results = "hide", fig.height = 10--------------------------------
par(mfrow=c(2,1))
LVs = getLV(ftComTol)
newLV = cbind(LV1 = seq(min(LVs[,1]), max(LVs[,1]), length.out=1000), LV2 = 0)
preds <- predict(ftComTol, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(ftComTol))), ylab  = "Predicted response", xlab = "LV1")
segments(x0=optima(ftComTol, sd.errors = FALSE)[,1],x1 = optima(ftComTol, sd.errors = FALSE)[,1], y0 = rep(0, ncol(ftComTol$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(ftComTol)[,1])
sapply(1:ncol(ftComTol$y), function(j)lines(sort(newLV[,1]), preds[order(newLV[,1]),j], lwd = 2))

LVs = getLV(ftComTol)
newLV = cbind(LV1 = 0, LV2 =  seq(min(LVs[,2]), max(LVs[,2]), length.out=1000))
preds <- predict(ftComTol, type = "response", newLV = newLV)
plot(NA, ylim = range(preds), xlim = c(range(getLV(ftComTol))), ylab  = "Predicted response", xlab = "LV2")
segments(x0=optima(ftComTol, sd.errors = FALSE)[,2],x1 = optima(ftComTol, sd.errors = FALSE)[,2], y0 = rep(0, ncol(ftComTol$y)), y1 = apply(preds,2,max), col = "red", lty = "dashed", lwd = 2)
rug(getLV(ftComTol)[,2])
sapply(1:ncol(ftComTol$y), function(j)lines(sort(newLV[,2]), preds[order(newLV[,2]),j], lwd = 2))

