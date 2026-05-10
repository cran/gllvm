## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, size="footnotesize", fig.width=7, fig.height=7, 
fig.align="center",dev="png", code.frame = TRUE, warning = FALSE, fig.pos='H')

## -----------------------------------------------------------------------------
library(gllvm)
data("kelpforest")
Yabund <- kelpforest$Y
Xenv <- kelpforest$X
SPinfo <- kelpforest$SPinfo

# Data contains both algae and sessile invertebrates
table(SPinfo$GROUP)

# Select only the macroalgae:
Yalg <- Yabund[,SPinfo$GROUP=="ALGAE"]

# To demonstrate the models, use only the data from the year 2016:
Yalg <- Yalg[Xenv$YEAR==2016,]
Xenv <- Xenv[Xenv$YEAR==2016,]

# Remove species which have no observations or just one
Yalg <- Yalg[,-which(colSums(Yalg>0)<2)]

# Number of obs. and species:
dim(Yalg)

# Specify the covariates in the linear predictor
Xformulai = ~KELP_FRONDS + PERCENT_ROCKY

## ----echo=FALSE---------------------------------------------------------------
rownames(Yalg) <- interaction(Xenv$SITE, Xenv$TRANSECT)

## -----------------------------------------------------------------------------
fit <- gllvm(Yalg, X=Xenv, formula = Xformulai, family = "betaH", method="EVA", 
             num.lv = 2, link="logit", control=list(reltol=1e-12))

## -----------------------------------------------------------------------------
fit$params$Xcoef

## -----------------------------------------------------------------------------
ordiplot(fit, jitter = TRUE, s.cex = .8)

## -----------------------------------------------------------------------------
sum(Yalg ==1)

## -----------------------------------------------------------------------------
colSums(Yalg>0)

## -----------------------------------------------------------------------------
# save the number of species to object m
m <- ncol(Yalg)
fit_ob <- gllvm(Yalg, X=Xenv, formula = Xformulai, family = "orderedBeta", 
    method="EVA", num.lv = 2, link="logit",
    disp.formula = rep(1, m), zetacutoff = c(0, 20),
    setMap = list(zeta = factor(rbind(1:m, rep(NA, m)))) )
fit_ob

## -----------------------------------------------------------------------------
fit_ob$params$zeta

## -----------------------------------------------------------------------------
ordiplot(fit_ob, jitter = TRUE, s.cex = .8)

