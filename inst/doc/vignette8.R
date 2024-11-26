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

# Remove empty rows and rows with missing values from the data
rem0 <- which((rowSums(Yalg)==0) | is.na(rowSums(Yalg)))
Yalg <- Yalg[-rem0,]
Xenv <- Xenv[-rem0,]

# Use only the data from the year 2016:

Yalg <- Yalg[Xenv$YEAR==2016,]
Xenv <- Xenv[Xenv$YEAR==2016,]

# Remove species which have no observations
Yalg <- Yalg[,-which(colSums(Yalg>0)==0)]
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

