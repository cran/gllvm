% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.gllvm.R
\name{simulate.gllvm}
\alias{simulate.gllvm}
\alias{simulate}
\title{Simulate data from gllvm fit}
\usage{
\method{simulate}{gllvm}(object, nsim = 1, seed = NULL, conditional = FALSE, ...)
}
\arguments{
\item{object}{an object of class 'gllvm'.}

\item{nsim}{an optional positive integer specifying the number of simulated datasets. Defaults to 1.}

\item{seed}{an optional integer to set seed number, passed to set.seed. Defaults to a random seed number.}

\item{conditional}{if \code{conditional = FALSE} simulates marginally over the latent variables.}

\item{...}{not used.}
}
\value{
A matrix containing generated data.
}
\description{
Generate new data using the fitted values of the parameters
}
\details{
simulate function for gllvm objects.
}
\examples{
 \donttest{
# Load a dataset from the mvabund package
data(antTraits, package = "mvabund")
y <- as.matrix(antTraits$abund)
X <- scale(antTraits$env[, 1:3])
# Fit gllvm model
fit <- gllvm(y = y, X, family = poisson())
# Simulate data
newdata <- simulate(fit)
}
}
\author{
David Warton, Jenni Niku <jenni.m.e.niku@jyu.fi>
}
