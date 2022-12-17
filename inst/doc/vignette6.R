## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size="footnotesize", fig.width=5, fig.height=5, fig.align="center",dev="png", code.frame = TRUE, warning = FALSE, fig.pos='H')

## ---- echo=FALSE, message=F, warning=F, result="hide"-------------------------
library(gllvm)
load(file = "ftCGLLVM.RData")

## ----data, eval=FALSE---------------------------------------------------------
#  library(gllvm)
#  
#  data(spider)
#  Y <- spider$abund
#  X <- spider$x
#  
#  #And scale the predictors
#  X <- scale(X)

## ----eval=FALSE---------------------------------------------------------------
#  MGLM <- gllvm(Y, X = X, family = "poisson", num.lv = 0)

## ----eval=FALSE---------------------------------------------------------------
#  RRGLM <- gllvm(Y, X = X, family = "poisson", num.RR = 2)

## ----eval=F, echo=F-----------------------------------------------------------
#  # the models are parameterized as (reduced) qr decomposition on the link scale
#  # which is, pending convergence and sign differences, easy to demonstrate
#  RRGLM <- update(RRGLM, num.RR = 6,starting.val="zero", reltol.c= 1e-15)
#  round(sweep(RRGLM$params$LvXcoef,2,apply(RRGLM$params$LvXcoef,2,function(x)sqrt(sum(x^2))),"/"),5)==round(sweep(qr.Q(qr(RRGLM$params$LvXcoef%*%t(RRGLM$params$theta))),2,sign(diag(qr.R(qr(RRGLM$params$LvXcoef%*%t(RRGLM$params$theta))))),"*"),5)

## ----eval=FALSE---------------------------------------------------------------
#  RRGLMb1 <- gllvm(Y, X = X, family="poisson", num.RR = 2, randomB = "LV")
#  RRGLMb1 <- gllvm(Y, X = X, family="poisson", num.RR = 2, randomB = "P")

## ----eval=FALSE---------------------------------------------------------------
#  CGLLVM <- gllvm(Y, X = X, family = "poisson", num.lv.c = 2)

## ----eval=FALSE---------------------------------------------------------------
#  PCGLLVM <- gllvm(Y, X = X, family = "poisson", num.lv.c = 2,
#                   lv.formula = ~bare.sand + fallen.leaves + moss+herb.layer + reflection,
#                   formula = ~soil.dry)

## -----------------------------------------------------------------------------
summary(CGLLVM)

## -----------------------------------------------------------------------------
ordiplot(CGLLVM, biplot = TRUE)

