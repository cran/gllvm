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
data("spider", package = "mvabund")
library(gllvm)
fitnb <- gllvm(y = spider$abund, family = "negative.binomial", num.lv = 2)
fitnb

## ----eval = TRUE, echo=TRUE, fig.width=8--------------------------------------
par(mfrow = c(1,2))
plot(fitnb, which = 1:2)

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
fitp <- gllvm(y = spider$abund, family = poisson(), num.lv = 2)
fitnb <- gllvm(y = spider$abund, family = "negative.binomial", num.lv = 2)
AIC(fitp)
AIC(fitnb)

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
library(gllvm)
data("spider", package = "mvabund")
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
fitp <- gllvm(y = spider$abund, family = poisson(), num.lv = 2)
fitp
fitnb <- gllvm(y = spider$abund, family = "negative.binomial", num.lv = 2)
fitnb

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
# Fit a GLLVM to data
plot(fitp)
plot(fitnb)

## ----eval = FALSE, echo=TRUE, warning=FALSE-----------------------------------
#  fitLAp <- gllvm(y = spider$abund, family = poisson(), method = "LA", num.lv = 2)
#  fitLAnb <- gllvm(y = spider$abund, family = "negative.binomial", method = "LA", num.lv = 2)
#  fitLAzip <- gllvm(y = spider$abund, family = "ZIP", method = "LA", num.lv = 2)
#  AIC(fitLAp)
#  AIC(fitLAnb)
#  AIC(fitLAzip)

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
# Parameters:
coef(fitnb)
# Where are the predicted latent variable values? just fitp$lvs or
getLV(fitnb)
# Standard errors for parameters:
fitnb$sd

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
# In exercise 2, we fitted GLLVM with two latent variables 
fitnb
# How about 1 or 3 LVs
fitnb1 <- gllvm(y = spider$abund, family = "negative.binomial", num.lv = 1)
fitnb1
getLV(fitnb1)
fitnb3 <- gllvm(y = spider$abund, family = "negative.binomial", num.lv = 3)
fitnb3
getLV(fitnb3)

## ----eval = TRUE, echo=TRUE, warning=FALSE------------------------------------
fitnbx <- gllvm(y = spider$abund, X = spider$x, family = "negative.binomial", seed = 123, num.lv = 2)
fitnbx
coef(fitnbx)
# confidence intervals for parameters:
confint(fitnbx)

## ----eval = TRUE, echo=FALSE, fig.width=5, fig.height=5-----------------------
par(mfrow=c(1,1))
ordiplot(fitnb, predict.region = TRUE, ylim=c(-2.5,2.5), xlim=c(-2,3))

## ----eval = TRUE, echo=TRUE---------------------------------------------------
ordiplot(fitnb, biplot = TRUE)
abline(h = 0, v = 0, lty=2)

## ----eval = TRUE, echo=TRUE, fig.width=6, fig.height=9------------------------
# Arbitrary color palette, a vector length of 20. Can use, for example, colorRampPalette from package grDevices
rbPal <- c("#00FA9A", "#00EC9F", "#00DFA4", "#00D2A9", "#00C5AF", "#00B8B4", "#00ABB9", "#009DBF", "#0090C4", "#0083C9", "#0076CF", "#0069D4", "#005CD9", "#004EDF", "#0041E4", "#0034E9", "#0027EF", "#001AF4", "#000DF9", "#0000FF")
X <- spider$x
par(mfrow = c(3,2), mar=c(4,4,2,2))
for(i in 1:ncol(X)){
Col <- rbPal[as.numeric(cut(X[,i], breaks = 20))]
ordiplot(fitnb, symbols = T, s.colors = Col, main = colnames(X)[i], 
         biplot = TRUE)
}

