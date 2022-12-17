## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size="footnotesize", fig.width=5, fig.height=5, fig.align="center",dev="png", code.frame = TRUE, warning = FALSE, fig.pos='H')

## ---- message=F, warning=F----------------------------------------------------
library(gllvm)
data("spider")
Y <- spider$abund

## ---- echo=FALSE--------------------------------------------------------------
load(file = "ftEqTol.RData")
load(file = "ftComTol.RData")
load(file = "ftUneqTol.RData")

## ---- eval = FALSE------------------------------------------------------------
#  ftEqTol <- gllvm(Y, family = "poisson", row.eff = "random", num.lv = 2)

## ---- eval = FALSE------------------------------------------------------------
#  ftComTol <- gllvm(Y, family = "poisson", num.lv = 2, quadratic = "LV")

## ---- eval = FALSE------------------------------------------------------------
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

