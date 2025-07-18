% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coefplot.gllvm.R
\name{coefplot.gllvm}
\alias{coefplot.gllvm}
\alias{coefplot}
\title{Plot covariate coefficients and confidence intervals}
\usage{
\method{coefplot}{gllvm}(
  object,
  y.label = TRUE,
  which.Xcoef = NULL,
  order = TRUE,
  cex.ylab = 0.5,
  cex.xlab = 1.3,
  mfrow = NULL,
  mar = c(4, 6, 2, 1),
  xlim.list = NULL,
  ind.spp = NULL,
  ...
)
}
\arguments{
\item{object}{an object of class 'gllvm'.}

\item{y.label}{logical, if \code{TRUE} (default) colnames of y with respect to coefficients are added to plot.}

\item{which.Xcoef}{vector indicating which covariate coefficients will be plotted. Can be vector of covariate names or numbers. Default is \code{NULL} when all covariate coefficients are plotted.}

\item{order}{logical, whether or not coefficients are ordered, defaults to \code{TRUE}.}

\item{cex.ylab}{the magnification to be used for axis annotation relative to the current setting of cex.}

\item{cex.xlab}{the magnification to be used for axis annotation.}

\item{mfrow}{same as \code{mfrow} in \code{par}. If \code{NULL} (default) it is determined automatically.}

\item{mar}{vector of length 4, which defines the margin sizes: \code{c(bottom, left, top, right)}. Defaults to \code{c(4,5,2,1)}.}

\item{xlim.list}{list of vectors with length of two to define the intervals for an x axis in each covariate plot. Defaults to NULL when the interval is defined by the range of point estimates and confidence intervals}

\item{ind.spp}{vector of species indices to construct the caterpillar plot for}

\item{...}{additional graphical arguments.}
}
\description{
Plots covariate coefficients and their confidence intervals.
}
\examples{
# Extract subset of the microbial data to be used as an example
data(microbialdata)
X <- microbialdata$Xenv
y <- microbialdata$Y[, order(colMeans(microbialdata$Y > 0), 
                     decreasing = TRUE)[21:40]]
fit <- gllvm(y, X, formula = ~ pH + Phosp, family = poisson())
coefplot(fit)
\dontrun{
# Fit  gllvm model with environmental covariances and reduced rank
fitRR <- gllvm(y = y, X = X, num.RR = 2, family = "negative.binomial")
coefplot(fitRR, mfrow=c(2,3))
}
}
\author{
Jenni Niku <jenni.m.e.niku@jyu.fi>, Francis K.C. Hui, Sara Taskinen, Bert van der Veen
}
