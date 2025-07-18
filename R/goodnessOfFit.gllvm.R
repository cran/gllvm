#' @title Goodness of fit measures for a gllvm
#' @description Several goodness-of-fit measure are currently available and can be calculated for a gllvm model fit and predicted values.
#'
#' @param y a response matrix
#' @param pred predicted values for response matrix y
#' @param measure a goodness-of-fit measure to be calculated. Options are \code{"cor"} (correlation between observed and predicted values), \code{"scor"} (Spearman correlation between observed and predicted values), \code{"RMSE"} (root mean squared error of prediction), \code{"MAE"} (Mean Absolute Error), \code{"MARNE"} (Mean Absolute Range Normalized Error), \code{"TjurR2"} (Tjur's R2 measure, only for binary data), \code{"R2"} (R-squared as the square of the correlation) and \code{"sR2"} (R-squared as the square of the spearman correlation)
#' @param object an object of class 'gllvm'.
#' @param species logical, if \code{TRUE}, goodness-of-fit measures are calculated for each species separately. If FALSE,  goodness-of-fit measures are calculated for all species together.
#'
#' @details
#' goodnessOfFit is used for evaluating the goodness-of-fit of a model or predictions. Available goodness-of-fit measures are correlation, RMSE, MARNE, and R2 measures. Definitions are below.
#' Denote an observed response j (species) at sample i, \eqn{i=1,...,n}, as \eqn{y_{ij}}, and predicted value as \eqn{\hat y_{ij}}.
#' 
#' \deqn{RMSE(\boldsymbol{y_{j}}, \boldsymbol{\hat y_{j}}) =  \sqrt{\frac{1}{n}\Sigma_{i=1}^{n} {(y_{ij} - \hat y_{ij})^2}} }
#' 
#' \deqn{MAE(\boldsymbol{y_{j}}, \boldsymbol{\hat y_{j}}) =  \frac{1}{n}\Sigma_{i=1}^{n} |y_{ij} - \hat y_{ij}| }
#' 
#' \deqn{MARNE(\boldsymbol{y_{j}}, \boldsymbol{\hat y_{j}}) =  \frac{1}{n}\Sigma_{i=1}^{n} \frac{|y_{ij} - \hat y_{ij}|}{max(\boldsymbol{y_{j}}) - min(\boldsymbol{y_{j}})} }
#' 
#' \deqn{Tjur's R2(\boldsymbol{y_{j}}, \boldsymbol{\hat y_{j}}) =  \frac{1}{n_1}\Sigma \hat y_{ij}\boldsymbol{1}_{y=1}(y_{ij}) - \frac{1}{n_0}\Sigma \hat y_{ij}\boldsymbol{1}_{y=0}(y_{ij}) }
#' 
#' 
#' 
#' @author Jenni Niku <jenni.m.e.niku@@jyu.fi>
#'
#'@seealso \code{\link{gllvm}}, \code{\link{predict.gllvm}}
#' @examples
#' \dontrun{
#'# Fit gllvm model with Poisson family
#'data(microbialdata)
#'X <- microbialdata$Xenv
#'y <- microbialdata$Y[, order(colMeans(microbialdata$Y > 0), 
#'                      decreasing = TRUE)[21:40]]
#'fit <- gllvm(y, X, formula = ~ pH + Phosp, family = poisson())
#'# Calculate metrics
#'goodnessOfFit(object = fit, measure = c("cor", "RMSE"))
#'
#'}
#'@export
goodnessOfFit <- function(y = NULL, pred = NULL, object = NULL, measure = c("cor", "RMSE", "MAE", "MARNE"), species = FALSE){
  if(is.null(pred)){
    if(is.null(object)) stop("If 'pred' is not given the model fit for 'object' need to be given.")
    pred <- predict(object, type = "response")
  }
  if(is.null(y)){
    if(is.null(object)) stop("If 'y' is not given the model fit for 'object' need to be given.")
    y <- object$y
  }else if(!is.matrix(y)){
    try(y <- as.matrix(y))
  }
  n <- NROW(y)
  p <- NCOL(y)
  
  out <- list()
  if("cor" %in% measure) {
    if(species) {
      out$cor <- rep(NA,p)
      for (j in 1:p) {
        out$cor[j] <- cor(na.omit(cbind(y[,j], pred[,j])))[2,1]
      }
    } else {
      out$cor <- cor(na.omit(cbind(unlist(c(y)), unlist(c(pred)))))[2,1]
    }
  }
  if("scor" %in% measure) {
    if(species) {
      out$cor <- rep(NA,p)
      for (j in 1:p) {
        out$scor[j] <- cor(na.omit(cbind(y[,j], pred[,j])), method = "spearman")[2,1]
      }
    } else {
      out$scor <- cor(na.omit(cbind(unlist(c(y)), unlist(c(pred)))), method = "spearman")[2,1]
    }
  }
  if("RMSE" %in% measure) {
    RMSE <- function(y,pred){ sqrt(mean((y- pred)^2, na.rm = TRUE))}
    if(species) {
      for (j in 1:p) {
        out$RMSE[j] <- RMSE(y[,j], pred[,j])
      }
    } else {
      out$RMSE <- RMSE(unlist(c(y)), unlist(c(pred)))
    }
  }

  if("MAE" %in% measure) {
    if(species) {
      for (j in 1:p) {
        out$MAE[j] <- mean(abs(y[,j]- pred[,j]), na.rm = TRUE)
      }
    } else {
      out$MAE <- mean(abs(y- pred), na.rm = TRUE)
    }
  }
  if("MARNE" %in% measure) {
    MARNE <- function(y,pred){ mean(abs(y- pred)/(max(y, na.rm = TRUE) - min(y, na.rm = TRUE)), na.rm = TRUE)}
    if(species) {
      for (j in 1:p) {
        out$MARNE[j] <- MARNE(y[,j], pred[,j])
      }
    } else {
      out$MARNE <- mean(t(abs(y- pred))/(apply(y,2,max, na.rm = TRUE) - apply(y,2,min, na.rm = TRUE)), na.rm = TRUE)
    }
  }
  if("TjurR2" %in% measure) {
    if(all(unique(y) %in% c(0,1,NA))){
      tjurR2 <- function(y,pred){mean(pred[y==1], na.rm = TRUE) - mean(pred[y==0], na.rm = TRUE)}
      if(species) {
        for (j in 1:p) {
          out$TjurR2[j] <- tjurR2(y[,j], pred[,j])
        }
      } else {
        out$TjurR2 <- tjurR2(unlist(c(y)), unlist(c(pred)))
      }
    }
  }
  if("R2" %in% measure) {
    if(species) {
      out$cor <- rep(NA,p)
      for (j in 1:p) {
        out$R2[j] <- cor(na.omit(cbind(y[,j], pred[,j])))[2,1]
        out$R2[j] <- sign(out$R2[j])*out$R2[j]^2
      }
    } else {
      out$R2 <- cor(na.omit(cbind(unlist(c(y)), unlist(c(pred)))))[2,1]
      out$R2 <- sign(out$R2)*out$R2^2
    }
  }
  if("sR2" %in% measure) {
    if(species) {
      out$cor <- rep(NA,p)
      for (j in 1:p) {
        out$sR2[j] <- cor(na.omit(cbind(y[,j], pred[,j])), method = "spearman")[2,1]
        out$sR2[j] <- sign(out$sR2[j])*out$sR2[j]^2
      }
    } else {
      out$sR2 <- cor(na.omit(cbind(unlist(c(y)), unlist(c(pred)))), method = "spearman")[2,1]
      out$sR2 <- sign(out$sR2)*out$sR2^2
    }
  }
  return(out)
}