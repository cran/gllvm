## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size="footnotesize", fig.width=5, fig.height=5, fig.align="center",dev="png", code.frame = TRUE, warning = FALSE, fig.pos='H')

## -----------------------------------------------------------------------------
library(gllvm)
data("microbialdata")
Ysoil <- microbialdata$Y
Xenv <- microbialdata$Xenv
dim(Ysoil)
head(Xenv, 3)

## ----echo=FALSE---------------------------------------------------------------
load(file = "ftXi.RData")
load(file = "ftXph.RData")
load(file = "ftX.RData")
load(file = "ftNULL.RData")
ftXi$randomB<-ftXph$randomB<-ftX$randomB<-ftNULL$randomB<-FALSE
ftXi$y<-ftXph$y<-ftX$y<-ftNULL$y<-Ysoil
ftXi$num.RR <- ftXi$num.lv.c <- ftXph$num.RR <- ftXph$num.lv.c <- 0
ftX$num.RR <- ftX$num.lv.c <- 0
ftNULL$num.RR <- ftNULL$num.lv.c <- 0
ftXi$quadratic<-ftXph$quadratic<-ftX$quadratic<-ftNULL$quadratic<-FALSE
#ftXi$num.lvcor<-ftXph$num.lvcor<-ftX$num.lvcor<-ftNULL$num.lvcor<-0
ftNULL$TMBfn$env$data$dr0=model.matrix(~Site-1, Xenv)
ftXi$col.eff$col.eff<-ftXph$col.eff$col.eff<-ftX$col.eff$col.eff<-ftNULL$col.eff$col.eff<-FALSE

## ----fig.height=4, fig.width=8------------------------------------------------
meanY <- apply(Ysoil,2, mean)
varY <- apply(Ysoil,2, var)
plot(log(meanY),varY, log = "y", main = "Species mean-variance relationship")

## -----------------------------------------------------------------------------
sDesign<-data.frame(Site=Xenv$Site)

## ----eval = FALSE-------------------------------------------------------------
#  ftNULL <- gllvm(Ysoil, studyDesign = sDesign, family = "negative.binomial", row.eff = ~(1|Site), num.lv = 2, sd.errors = FALSE)

## -----------------------------------------------------------------------------
ftNULL

## ----fig.height=4, fig.width=8------------------------------------------------
par(mfrow = c(1, 2))
plot(ftNULL, which = 1:2, var.colors = 1, n.plot = 100)

## ----out.width='70%'----------------------------------------------------------
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
ordiplot(ftNULL, main = "Ordination of sites, color: pH",
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

ordiplot(ftNULL, main = "Ordination of sites, color: SOM", 
         symbols = TRUE, pch = pchr, s.colors = Colorssom)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

ordiplot(ftNULL, main = "Ordination of sites, color: phosphorous",
         symbols = TRUE, pch = pchr, s.colors = Colorsphosp)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")


## ----fig.height=4, fig.width=6, out.width='70%'-------------------------------
# Sampling locations of the eight sampling sites:
locaSites<-c(3,3,1,1,2,2,2,1)
plot(ftNULL$params$row.params, xlab = "site", col = locaSites, pch = locaSites, 
     main = "Site effects", ylab = "Site effect", xaxt = 'n', ylim = c(-1,1.5))
axis(1, at=1:8, labels=levels(sDesign$Site))
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), 
       col = c(1, 2, 3), bty = "n")

## ----fig.height=8, fig.width=8------------------------------------------------
# Plot the species using column indices of the species:
rownames(ftNULL$params$theta) <- 1:ncol(Ysoil)
ordiplot(ftNULL, main = "Ordination of sites and species", xlim = c(-6, 5), 
         ylim = c(-4, 4), symbols = TRUE, pch = pchr, s.colors = Colorsph, 
         biplot = TRUE, ind.spp = 15, cex.spp = 0.9)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch=c(1, 2, 3), bty = "n")

## -----------------------------------------------------------------------------
# Scale environmental variables
Xsoils <- scale(Xenv[, 1:3])

## ----eval = FALSE-------------------------------------------------------------
#  ftXph <- gllvm(Ysoil, X = Xsoils, studyDesign = sDesign, formula = ~pH, family = "negative.binomial",
#                 row.eff = ~(1|Site), num.lv = 2)

## -----------------------------------------------------------------------------
ftXph

## ----fig.height=7, fig.width=7------------------------------------------------
coefplot(ftXph, cex.ylab = 0.5, y.label = FALSE)

## ----out.width='70%'----------------------------------------------------------
ordiplot(ftXph, main = "Ordination of sites", 
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

## ----eval = FALSE-------------------------------------------------------------
#  ftX <- gllvm(Ysoil, X = Xsoils, studyDesign = sDesign, family = "negative.binomial", row.eff = ~(1|Site), num.lv = 2)

## -----------------------------------------------------------------------------
ftX

## ----fig.height=10, fig.width=7-----------------------------------------------
coefplot(ftX, cex.ylab = 0.5, y.label = FALSE, mar = c(4, 2, 2, 1))

## ----out.width='70%'----------------------------------------------------------
ordiplot(ftX, main = "Ordination of sites", 
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

## ----eval = FALSE-------------------------------------------------------------
#  Xenv <- data.frame(Xsoils, Region = factor(Xenv$Region),
#                     Soiltype = factor(Xenv$Soiltype))
#  ftXi <- gllvm(Ysoil, X = Xenv, studyDesign = sDesign, formula = ~ SOM + pH + Phosp + Region,
#                family = "negative.binomial", row.eff = ~(1|Site), num.lv = 2,
#                sd.errors = FALSE)

## -----------------------------------------------------------------------------
ftXi

## ----warning=FALSE, out.width='70%'-------------------------------------------
ordiplot(ftXi, main = "Ordination of sites",  
         symbols = TRUE, pch = pchr, s.colors = Colorsph)
legend("topleft", legend = c("Kil", "NyA", "Mayr"), pch = c(1, 2, 3), bty = "n")

## -----------------------------------------------------------------------------
1 - getResidualCov(ftX)$trace/getResidualCov(ftNULL)$trace

