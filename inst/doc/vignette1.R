## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width=5, fig.height=5,
  fig.align = "center",
  dev = "png",
  fig.pos = 'H',
  code.frame=TRUE
)

## -----------------------------------------------------------------------------
library(mvabund)
library(gllvm)
data(antTraits)
y <- as.matrix(antTraits$abund)
X <- scale(as.matrix(antTraits$env))
TR <- antTraits$traits

## ---- eval = FALSE------------------------------------------------------------
#  gllvm(y = NULL, X = NULL, TR = NULL, data = NULL, formula = NULL, num.lv = 2, family,
#        method = "VA", row.eff = FALSE, offset = NULL, Power = 1.5, starting.val = "res", ...)

## ---- eval = FALSE------------------------------------------------------------
#  # Model without predictors:
#  gllvm(y, family = "negative.binomial")

## ---- eval = FALSE------------------------------------------------------------
#  # Model where environmental variables, that is, all variables in X are included:
#  gllvm(y, X, family = "negative.binomial")

## ---- eval = FALSE------------------------------------------------------------
#  # Fourth corner model, where all main effects for environmental variables in X and
#  # all interactions between variables in X and variables in TR are included:
#  gllvm(y, X, TR, family = "negative.binomial")

## -----------------------------------------------------------------------------
yX <- reshape(data.frame(cbind(y, X)), direction = "long", varying =
                colnames(y), v.names = "y", timevar = "sp")
TR2 <- data.frame(sp = 1:41, TR)
datalong <- merge(yX, TR2, by = "sp")
datalong[1:3, ]

## ---- eval = FALSE------------------------------------------------------------
#  # Model without predictors:
#  gllvm(formula = y ~ 1, data = datalong, family = "negative.binomial")

## ---- eval = FALSE------------------------------------------------------------
#  # Model with environmental variables Bare.ground and Shrub.cover as predictors
#  gllvm(formula = y ~ (Bare.ground + Shrub.cover), data = datalong,
#        family = "negative.binomial")

## -----------------------------------------------------------------------------
fitp <- gllvm(y, family = poisson())
fitp

## -----------------------------------------------------------------------------
fit_ord <- gllvm(y, family = "negative.binomial")
fit_ord

## ---- fig.show='hold'---------------------------------------------------------
# Plot residuals for the Poisson model
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fitp, var.colors = 1)

## ---- fig.show='hold'---------------------------------------------------------
# Plot residuals for the NB model
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
plot(fit_ord, var.colors = 1)

## ---- fig.show='hold', out.width='70%'----------------------------------------
ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Biplot")
ordiplot(fit_ord, biplot = FALSE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-3, 3), 
         main = "Ordination plot", predict.region = TRUE)

## ---- fig.show='hold', out.width='70%'----------------------------------------
rownames(fit_ord$params$theta) <- paste("spp", 1:ncol(fit_ord$y), sep = "")
ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, xlim = c(-3, 3), ylim = c(-2, 1.6), 
         main = "Biplot", jitter = TRUE, cex.spp = 0.8)

## ---- eval = FALSE, warning = FALSE-------------------------------------------
#  criterias <- NULL
#  for(i in 1:5){
#    fiti <- gllvm(y, X, family = "negative.binomial", num.lv = i, sd.errors = FALSE,
#                  formula = ~ Bare.ground + Canopy.cover + Volume.lying.CWD, seed = 1234)
#    criterias[i + 1] <- summary(fiti)$AICc
#    names(criterias)[i + 1] = i
#  }

## ---- eval = FALSE, warning = FALSE-------------------------------------------
#  # Compare AICc values
#  criterias
#  #>        1        2        3        4        5
#  #> 3399.419 3398.121 3357.129 3353.483 3354.003

## ---- warning = FALSE---------------------------------------------------------
fit_env <- gllvm(y, X, family = "negative.binomial", num.lv = 4,
                 formula = ~ Bare.ground + Canopy.cover +
                   Volume.lying.CWD, seed = 1234)

## ---- fig.show='hold', out.width='49%'----------------------------------------
coefplot(fit_env, cex.ylab = 0.7, mar = c(4, 9, 2, 1), 
         xlim.list = list(NULL, NULL, c(-4, 4)), mfrow=c(1,1))

## ---- fig.show='hold', out.width='49%'----------------------------------------
rownames(fit_env$params$Xcoef) <- paste("spp", 1:ncol(fit_env$y), sep = "")
coefplot(fit_env, cex.ylab = 0.7, xlim.list = list(NULL, NULL, c(-4, 4)), 
         mfrow=c(1,1))

## ---- echo=FALSE--------------------------------------------------------------
rownames(fit_env$params$Xcoef) <- colnames(fit_env$y)

## ---- eval = FALSE, warning = FALSE-------------------------------------------
#  library(MuMIn)
#  fit_table <- dredge(fiti,varying=list(num.lv=1:5), rank="AIC")
#  subset(fit_table, delta<2)
#  # Global model call: gllvm(y = y, X = X, formula = ~Bare.ground + Shrub.cover + Volume.lying.CWD,
#  #     num.lv = i, family = "negative.binomial", sd.errors = FALSE,
#  #     seed = 1234)
#  # ---
#  # Model selection table
#  #    (Int) Bar.grn Shr.cvr Vlm.lyn.CWD num.lv  df    logLik   AICc delta weight
#  # 39     +       +       +           +      4 363 -1793.923 3522.6  0.00   0.52
#  # 38     +       +       +           +      3 325 -1794.337 3522.8  0.16   0.48
#  # Models ranked by AICc(x)

## ---- fig.show='hold'---------------------------------------------------------
# Residual correlation matrix:
cr <- getResidualCor(fit_env)
library(corrplot); library(gclus)

## ---- fig.show='hold'---------------------------------------------------------
corrplot(cr[order.single(cr), order.single(cr)], diag = FALSE, type = "lower", 
         method = "square", tl.cex = 0.5, tl.srt = 45, tl.col = "red")

## ---- warning=FALSE-----------------------------------------------------------
# Fit GLLVM without environmental variables and 3 latent variables:
fit4lv <- gllvm(y, family = "negative.binomial", num.lv = 4, seed = 1234)

## ---- fig.show='hold', warning=FALSE------------------------------------------
# Correlation matrix
cr0 <- getResidualCor(fit4lv)
corrplot(cr0[order.single(cr0), order.single(cr0)], diag = FALSE, type = "lower", 
         method = "square", tl.cex = 0.5, tl.srt = 45, tl.col = "red")

## ----fig.height=7, fig.show='hold', fig.width=7, message=FALSE, warning=FALSE, out.width='49%'----
fit_env2 <- gllvm(y, X, family = "negative.binomial", num.lv = 2, 
                  formula = ~ Bare.ground + Canopy.cover + 
                    Volume.lying.CWD, seed = 1234)
rownames(fit_env2$params$theta) <- paste("sp", 1:ncol(fit_env2$y), sep = "")
ordiplot(fit_ord, biplot = TRUE, ind.spp = 15, jitter = TRUE, cex.spp = 1,
         xlim = c(-4, 3.5), ylim = c(-2.5, 2), main = "(a)")
ordiplot(fit_env2, biplot = TRUE, ind.spp = 15, jitter = TRUE, cex.spp = 1,
         xlim = c(-4, 3.5), ylim = c(-2.5, 2), main = "(b)")

## -----------------------------------------------------------------------------
rcov <- getResidualCov(fit_env, adjust = 0)
rcov0 <- getResidualCov(fit4lv, adjust = 0)
rcov0$trace; rcov$trace
1 - rcov$trace / rcov0$trace

## ---- warning=FALSE-----------------------------------------------------------
fit_4th <- gllvm(y, X, TR, family = "negative.binomial", num.lv = 2, 
                 formula = y ~ (Bare.ground + Canopy.cover + Volume.lying.CWD) +
                (Bare.ground + Canopy.cover + Volume.lying.CWD) : (Pilosity + 
                Polymorphism + Webers.length), seed = 123,
                row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
                randomX = ~ Bare.ground + Canopy.cover + Volume.lying.CWD)

## ---- fig.show='hold', out.width='49%'----------------------------------------
library(lattice)
coefplot(fit_4th, mar = c(4, 11, 1, 1), cex.ylab = 0.8)
fourth <- fit_4th$fourth.corner
a <- max( abs(fourth) )
colort <- colorRampPalette(c("blue", "white", "red"))
plot.4th <- levelplot((as.matrix(fourth)), xlab = "Environmental Variables", 
                      ylab = "Species traits", col.regions = colort(100), cex.lab = 1.3, 
                      at = seq(-a, a, length = 100), scales = list(x = list(rot = 45)))
plot.4th

## ---- warning=FALSE, eval=FALSE-----------------------------------------------
#  fit_4th2 <- gllvm(y, X, TR, family = "negative.binomial", num.lv = 2,
#          formula = y ~ (Bare.ground + Canopy.cover + Volume.lying.CWD), seed = 123,
#                   row.eff = "random", control.start =list(n.init = 3, jitter.var = 0.01),
#                   randomX = ~ Bare.ground + Canopy.cover + Volume.lying.CWD)
#  # Test interactions using likelihood ratio test:
#  anova(fit_4th, fit_4th2)
#  # Model  1 :  y ~ (Bare.ground + Canopy.cover + Volume.lying.CWD)
#  # Model  2 :  y ~ (Bare.ground + Canopy.cover + Volume.lying.CWD) + (Bare.ground +
#  # Canopy.cover + Volume.lying.CWD):(Pilosity + Polymorphism + Webers.length)
#  # Resid.Df        D Df.diff     P.value
#  # 1     1057  0.00000       0
#  # 2     1039 60.34953      18 1.79632e-06

