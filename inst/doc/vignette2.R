## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size="footnotesize", fig.width=5, fig.height=5, fig.align="center",dev="png", code.frame = TRUE, warning = FALSE, fig.pos='H')

## ------------------------------------------------------------------------
library(gllvm)
load(file = "microbialdata.RData")
Ysoil <- microbialdata$Ysoil
Xenv <- microbialdata$Xenv
dim(Ysoil)
head(Xenv, 3)

## ---- echo=FALSE---------------------------------------------------------
load(file = "ftXi.RData")
load(file = "ftXph.RData")
load(file = "ftX.RData")
load(file = "ftNULLpois.RData")
load(file = "ftNULL.RData")
ftXi$y<-ftXph$y<-ftX$y<-ftNULLpois$y<-ftNULL$y<-Ysoil

## ---- eval = FALSE-------------------------------------------------------
#  ftNULL <- gllvm(Ysoil, family = "negative.binomial", row.eff = "random", num.lv = 2)
#  ftNULLpois <- gllvm(Ysoil, family = "poisson", row.eff = "random", num.lv = 2)

## ------------------------------------------------------------------------
ftNULL
ftNULLpois

## ----fig.height=4, fig.width=8-------------------------------------------
par(mfrow = c(1, 2))
plot(ftNULL, which = 1:2, var.colors = 1, xlim = c(-9, 5), n.plot = 100)

## ----fig.height=4, fig.width=8-------------------------------------------
par(mfrow = c(1, 2))
plot(ftNULLpois, which = 1:2, var.colors = 1, xlim = c(-9, 5), n.plot = 100)

## ---- out.width='70%'----------------------------------------------------
# Define colors according to the values of pH, SOM and phosp
library(grDevices)
ph <- Xenv$pH
rbPal <- colorRampPalette(c('mediumspringgreen', 'blue'))
Colorsph <- rbPal(20)[as.numeric(cut(ph, breaks = 20))]
breaks <- seq(min(ph), max(ph), length.out = 30)
som <- Xenv$SOM
Colorssom <- rbPal(20)[as.numeric(cut(som, breaks = 20))]
breaks <- seq(min(som), max(som), length.out = 30)
phosp <- Xenv$Phosp
Colorsphosp <- rbPal(20)[as.numeric(cut(phosp, breaks = 20))]
breaks <- seq(min(phosp), max(phosp), length.out = 30)
# Define symbols for different sampling locations:
pchr = NULL
pchr[Xenv$Region == "Kil"] = 1
pchr[Xenv$Region == "NyA"] = 2
pchr[Xenv$Region == "Aus"] = 3

# Ordination plots. Dark color indicates high environmental covariate value.
ordiplot(ftNULL, main = "Ordination of sites, color: pH", xlim = c(-2.5, 2.5), 
         ylim = c(-2.5, 2.5), symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

ordiplot(ftNULL, main = "Ordination of sites, color: SOM", xlim = c(-2.5, 2.5), 
         ylim = c(-2.5, 2.5), symbols = TRUE, pch = pchr, s.colors = Colorssom)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

ordiplot(ftNULL, main = "Ordination of sites, color: phosphorous", xlim = c(-2.5, 2.5), 
         ylim = c(-2.5, 2.5), symbols = TRUE, pch = pchr, s.colors = Colorsphosp)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")


## ----fig.height=4, fig.width=6, out.width='70%'--------------------------
plot(ftNULL$params$row.params, xlab = "site", col = Xenv$Region, pch = pchr, 
     main = "Site effects", ylab = "Site effect")
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), 
       col = c(2, 3, 1), bty = "n")

## ----fig.height=8, fig.width=8-------------------------------------------
# Plot the species using column indices of the species:
rownames(ftNULL$params$theta) <- 1:ncol(Ysoil)
ordiplot(ftNULL, main = "Ordination of sites and species", xlim = c(-7, 5), 
         ylim = c(-4, 4), symbols = TRUE, pch = pchr, s.colors = Colorsph, 
         biplot = TRUE, ind.spp = 15, cex.spp = 0.9)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch=c(1, 2, 3), bty = "n")

## ------------------------------------------------------------------------
# Scale environmental variables
Xsoils <- scale(Xenv[, 1:3])

## ---- eval = FALSE-------------------------------------------------------
#  ftXph <- gllvm(Ysoil, Xsoils, formula = ~pH, family = "negative.binomial",
#                 row.eff = "random", num.lv = 2)

## ------------------------------------------------------------------------
ftXph

## ----fig.height=7, fig.width=7-------------------------------------------
coefplot(ftXph, cex.ylab = 0.5, y.label = FALSE)

## ---- out.width='70%'----------------------------------------------------
ordiplot(ftXph, main = "Ordination of sites", xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), 
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

## ---- eval = FALSE-------------------------------------------------------
#  ftX <- gllvm(Ysoil, Xsoils, family = "negative.binomial", row.eff = "random", num.lv = 2)

## ------------------------------------------------------------------------
ftX

## ----fig.height=10, fig.width=7------------------------------------------
coefplot(ftX, cex.ylab = 0.5, y.label = FALSE, mar = c(4, 2, 2, 1))

## ---- out.width='70%'----------------------------------------------------
ordiplot(ftX, main = "Ordination of sites", xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), 
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

## ---- eval = FALSE-------------------------------------------------------
#  Xenv <- data.frame(Xsoils, Region = factor(Xenv$Region), Site = factor(Xenv$Site),
#                     Soiltype = factor(Xenv$Soiltype))
#  ftXi <- gllvm(Ysoil, Xenv, formula = ~ SOM + pH + Phosp + Region,
#                family = "negative.binomial", row.eff = "random", num.lv = 2)

## ------------------------------------------------------------------------
ftXi

## ---- warning=FALSE, out.width='70%'-------------------------------------
ordiplot(ftXi, main = "Ordination of sites", xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5), 
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

## ------------------------------------------------------------------------
# Deviances
(dev1 <- (ftX$logL - ftNULL$logL) * 2)
(dev2 <- (ftXi$logL - ftX$logL) * 2)
1 - dev2/dev1

## ------------------------------------------------------------------------
1 - getResidualCov(ftX)$tr/getResidualCov(ftNULL)$tr

