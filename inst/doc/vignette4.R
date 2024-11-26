## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  warning = FALSE,
  fig.width=5, fig.height=5,
  fig.align = "center",
  dev = "png",
  fig.pos = 'H'
  )

## ----eval = FALSE, echo=TRUE, warning=FALSE-----------------------------------
#  # From CRAN
#  install.packages(gllvm)
#  # OR
#  # From GitHub using devtools package's function install_github
#  devtools::install_github("JenniNiku/gllvm")

## ----eval = FALSE, echo=TRUE--------------------------------------------------
#  gllvm(y = NULL, X = NULL, TR = NULL, family, num.lv = 2,
#   formula = NULL, method = "VA", row.eff = FALSE, n.init=1, starting.val ="res", ...)

## ----eval = TRUE, echo=TRUE---------------------------------------------------
library(gllvm)

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
library(gllvm)
data("spider", package = "mvabund")
fitx <- gllvm(y = spider$abund, X = spider$x, family = "negative.binomial", num.lv = 2)
fitx

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
X=spider$x
fitx1 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 1)
fitx2 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 2)
fitx3 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 3)
AIC(fitx1)
AIC(fitx2)
AIC(fitx3)

## ----eval = TRUE, echo=TRUE, fig.width=7, fig.height=3.5----------------------
par(mfrow = c(1,2))
plot(fitx1, which = 1:2)

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
library(gllvm)
#Package **mvabund** is loaded with **gllvm** so just load with a function `data()`.
data("spider")
# more info: 
# ?spider

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
# response matrix:
spider$abund
# Environmental variables
spider$x
# Plot data using boxplot:
boxplot(spider$abund)

## ----eval = FALSE, echo=TRUE, warning=FALSE-----------------------------------
#  # Take a look at the function documentation for help:
#  ?gllvm

## ----eval = TRUE, echo=TRUE, warning=FALSE, fig.width=8, fig.height=5---------
# Fit a GLLVM to data
fitp <- gllvm(y=spider$abund, family = poisson(), num.lv = 2)
fitp
fitnb <- gllvm(y=spider$abund, family = "negative.binomial", num.lv = 2)
fitnb

## ----eval = TRUE, echo=TRUE, warning=FALSE, fig.width=8-----------------------
# Fit a GLLVM to data
par(mfrow = c(1,2))
plot(fitp, which = 1:2)
plot(fitnb, which = 1:2)

## ----eval = FALSE, echo=TRUE, warning=FALSE-----------------------------------
#  fitLAp <- gllvm(y=spider$abund, family = poisson(), method = "LA", num.lv = 2)
#  fitLAnb <- gllvm(y=spider$abund, family = "negative.binomial", method = "LA", num.lv = 2)
#  fitLAzip <- gllvm(y=spider$abund, family = "ZIP", method = "LA", num.lv = 2)
#  AIC(fitLAp)
#  AIC(fitLAnb)
#  AIC(fitLAzip)

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
# `soil.dry` and `reflection` are in columns 1 and 6
X <- spider$x[,c(1,6)]
fitx1 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 1)
fitx2 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 2)
fitx3 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 3)
AIC(fitx1)
AIC(fitx2)
AIC(fitx3)
# Or alternatively using formula:
fitx1 <- gllvm(spider$abund, spider$x, formula = ~soil.dry + reflection, family = "negative.binomial", num.lv = 1)
fitx1

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
coef(fitx1)
# Coefficients for covariates are named as `Xcoef`
# Confidence intervals for these coefficients:
confint(fitx1, parm = "Xcoef")
# The first 12 intervals are for soil.dry and next 12 for reflection

## ----eval = TRUE, echo=TRUE, fig.width=4.5------------------------------------
fitnb <- gllvm(spider$abund, family = "negative.binomial", num.lv = 2)

## ----eval = FALSE, echo=TRUE, fig.width=4.5-----------------------------------
#  fitnb <- gllvm(spider$abund, family = "negative.binomial", num.lv = 2)
#  ordiplot(fitnb, biplot = TRUE)
#  abline(h = 0, v = 0, lty=2)

## ----eval = TRUE, echo=FALSE, fig.width=4.5, fig.height=3.8-------------------
par(mfrow=c(1,1), mar=c(4,4,0.1,0.1))
ordiplot(fitnb, biplot = TRUE)
abline(h = 0, v = 0, lty=2)

## ----eval = TRUE, echo=TRUE, fig.height=5-------------------------------------
fitnb <- gllvm(spider$abund, family = "negative.binomial", num.lv = 2)
cr <- getResidualCor(fitnb)
library(corrplot);
corrplot(cr, diag = FALSE, type = "lower", method = "square", tl.srt = 25)

## ----eval = FALSE, echo=TRUE, fig.width=5-------------------------------------
#  ordiplot(fitnb, biplot = TRUE)
#  abline(h = 0, v = 0, lty=2)

## ----eval = TRUE, echo=TRUE, fig.width=8, fig.height=4------------------------
rbPal <- c("#00FA9A", "#00EC9F", "#00DFA4", "#00D2A9", "#00C5AF", "#00B8B4", "#00ABB9", "#009DBF", "#0090C4", "#0083C9", "#0076CF", "#0069D4", "#005CD9", "#004EDF", "#0041E4", "#0034E9", "#0027EF", "#001AF4", "#000DF9", "#0000FF")
X <- spider$x[,c(1,6)]
par(mfrow = c(1,2), mar=c(4,4,2,2))
for(i in 1:ncol(X)){
Col <- rbPal[as.numeric(cut(X[,i], breaks = 20))]
ordiplot(fitnb, symbols = T, s.colors = Col, main = colnames(X)[i], biplot = TRUE)
abline(h=0,v=0, lty=2)

}

## ----eval = TRUE, echo=TRUE, fig.width=8, fig.height=3.5----------------------
fitx1 <- gllvm(spider$abund, X, family = "negative.binomial", num.lv = 1)
coefplot(fitx1, mfrow = c(1,2), cex.ylab = 0.8)

## ----eval = TRUE, echo=TRUE, fig.height=5-------------------------------------
crx <- getResidualCor(fitx1)
corrplot(crx, diag = FALSE, type = "lower", method = "square", tl.srt = 25)

