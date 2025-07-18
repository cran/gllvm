% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/randomCoefplot.R
\name{randomCoefplot.gllvm}
\alias{randomCoefplot.gllvm}
\alias{randomCoefplot}
\title{Plot random slope coefficients}
\usage{
\method{randomCoefplot}{gllvm}(
  object,
  y.label = TRUE,
  which.Xcoef = NULL,
  cex.ylab = 0.5,
  mfrow = NULL,
  mar = c(4, 6, 2, 1),
  xlim.list = NULL,
  order = FALSE,
  ind.spp = NULL,
  ...
)
}
\arguments{
\item{object}{an object of class 'gllvm'.}

\item{y.label}{logical, if \code{TRUE} (default) colnames of y with respect to coefficients are added to plot.}

\item{which.Xcoef}{fector indicating which covariate coefficients will be plotted. Can be vector of covariate names or numbers. Default is NULL when all covariate coefficients are plotted.}

\item{cex.ylab}{the magnification to be used for axis annotation relative to the current setting of cex.}

\item{mfrow}{same as \code{mfrow} in \code{par}. If \code{NULL} (default) it is determined automatically.}

\item{mar}{vector of length 4, which defines the margin sizes: \code{c(bottom, left, top, right)}. Defaults to \code{c(4,5,2,1)}.}

\item{xlim.list}{list of vectors with length of two to define the intervals for x axis in each covariate plot. Defaults to NULL when the interval is defined by the range of point estimates and confidence intervals}

\item{order}{logical, if \code{TRUE} (default), coefficients are sorted according to the point estimates}

\item{ind.spp}{vector of species indices to construct the caterpillar plot for}

\item{...}{additional graphical arguments.}
}
\description{
Plots random slopes and their prediction intervals.
}
\examples{
\dontrun{
## Load a dataset from the mvabund package
data(antTraits, package = "mvabund")
y <- as.matrix(antTraits$abund)
X <- as.matrix(antTraits$env)
TR <- antTraits$traits
# Fit model with random slopes
fitF <- gllvm(y = y, X = X, TR = TR,
 formula = ~ Bare.ground + Bare.ground : Webers.length,
 family = poisson(), randomX = ~ Bare.ground)
randomCoefplot(fitF)
}

}
\author{
Jenni Niku <jenni.m.e.niku@jyu.fi>, Francis K.C. Hui, Bert van der Veen, Sara Taskinen,
}
