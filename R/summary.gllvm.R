#' @title Summarizing gllvm model fits
#' @description A summary of the fitted 'gllvm' object, including function call, distribution family and model parameters.
#' 
#' @details Various options are available to include extra parameter estimates in the summary, which have been excluded by default, for readability.
#'
#' @param object an object of class 'gllvm'
#' @param x a summary object
#' @param by By = "all" (default) will return a Wald statistics per predictor and LV if the ordination includes predictors, by = "terms" will return a multivariate Wald statistic per predictor (displayed at first LV), and by = "LV" will do the same but per dimension (displayed at first predictors).
#' @param digits the number of significant digits to use when printing
#' @param signif.stars If \code{TRUE}, significance stars are printed for each coefficient, defaults to \code{TRUE}
#' @param dispersion option to return dispersion parameters, defaults to \code{FALSE}
#' @param spp.intercepts option to return species intercepts, defaults to \code{FALSE}
#' @param row.intercepts option to return row intercepts, defaults to \code{FALSE} 
#' @param Lvcoefs option to return species scores in the ordination, defaults to \code{FALSE}. Returns species optima for quadratic model.
#' @param rotate defaults to \code{TRUE}. If \code{TRUE} rotates the output of the latent variables to principal direction, so that it coincides with the ordiplot results. If both unconstrained and constrained latent variables are included, predictor slopes are not rotated.
#' @param type to match "type" in \code{\link{ordiplot.gllvm}}
#' @param ...	 not used.
#'
#' @author Jenni Niku <jenni.m.e.niku@@jyu.fi>, Bert van der Veen
#'
#' @examples
#' \dontrun{
#'## Load a dataset from the mvabund package
#' data(antTraits)
#' y <- as.matrix(antTraits$abund)
#'# Fit gllvm model
#' fit <- gllvm(y = y, family = poisson())
#' summary(fit)
#'}
#'@export
#'@export print.summary.gllvm 

summary.gllvm <- function(object, by = "all", digits = max(3L, getOption("digits") - 3L),
                          signif.stars = getOption("show.signif.stars"), dispersion = FALSE, spp.intercepts = FALSE, row.intercepts = FALSE, Lvcoefs = FALSE, rotate = TRUE, type = NULL,
                          ...) {
  
  n <- NROW(object$y)
  p <- NCOL(object$y)
  nX <- dim(object$X)[2]
  nTR <- dim(object$TR)[2]
  num.lv <- object$num.lv
  num.lv.c <- object$num.lv.c
  num.RR <- object$num.RR
  quadratic <- object$quadratic
  family <- object$family
  
  if(!by%in%c("all","terms","LV"))stop("'by' must be one of 'all', 'terms', or 'LV'.")
  #calculate rotation matrix
  if(rotate && (num.lv+num.lv.c+num.RR)>1){
    lv <- getLV(object, type = type)
    
    do_svd <- svd(lv)
    svd_rotmat_sites <- do_svd$v
    if(num.lv.c>0|(num.RR+num.lv)>0){
      do_svd$v <- svd(getLV(object))$v
    }
  }else{
    svd_rotmat_sites <- diag(num.lv.c+num.RR+num.lv)
  }
  if((num.RR+num.lv+num.lv.c)>0 && Lvcoefs){
    if(quadratic==FALSE){
      M <- cbind(object$params$beta0, object$params$theta[,1:(num.lv.c+num.RR+num.lv)]%*%svd_rotmat_sites)  
    }else{
      M <- cbind(object$params$beta0, optima(object,sd.errors=F)%*%svd_rotmat_sites)  
    }  
  }else{
    M <- cbind(object$params$beta0)
  }
  
  
  sumry <- list()
  sumry$digits <- digits
  sumry$signif.stars <- signif.stars
  sumry$dispersion <- dispersion
  sumry$spp.intercepts <- spp.intercepts
  sumry$row.intercepts <- row.intercepts
  sumry$Lvcoefs <- Lvcoefs
  sumry$num.lv <- num.lv
  sumry$num.lv.c <- num.lv.c
  sumry$num.RR <- num.RR
  sumry$quadratic <- quadratic
  sumry$formula <- object$formula
  sumry$lv.formula <- object$lv.formula
  sumry$'log-likelihood' <- object$logL
  crit <- inf.criteria(object)
  sumry$df <- crit$k
  sumry$AIC <- crit$AIC
  sumry$AICc <- crit$AICc
  sumry$BIC <- crit$BIC
  
  crit <-
    newnams <- c("Intercept")
  
  if((num.lv+num.lv.c+num.RR)>0 && Lvcoefs){
    newnams <- c(newnams, dimnames(object$params$theta)[[2]][1:(num.lv+num.lv.c+num.RR)])
  }
  
  if (!is.logical(object$sd)&!is.null(object$X)&is.null(object$TR)) {
    pars <- c(object$params$Xcoef)
    se <- c(object$sd$Xcoef)
    zval <- pars/se
    pvalue <- 2 * pnorm(-abs(zval))
    coef.table <- cbind(pars, se, zval, pvalue)
    dimnames(coef.table) <- list(paste(rep(colnames(object$X.design),each=ncol(object$y)),colnames(object$y),sep=":"), c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
  }else if(!is.logical(object$sd)&!is.null(object$X)){
    pars <- c(object$params$B)
    se <- c(object$sd$B)
    nX <- dim(object$X)[2]
    cnx <- rep(colnames(object$X.design), each = ifelse(!is.null(object$TR),1,p))
    
    if(!is.null(object$params$Xcoef)){
      rnc <- rep(rownames(object$params$Xcoef), nX)
      newnam <- paste(cnx, rnc, sep = ":")
    }else if(!is.null(object$X.design)){
      newnam <- colnames(object$X.design)
    }
    zval <- pars/se
    pvalue <- 2 * pnorm(-abs(zval))
    coef.table <- cbind(pars, se, zval, pvalue)
    dimnames(coef.table) <- list(newnam, c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
  }else{
    coef.table <- NULL
  }
  
  if (!is.logical(object$sd)&!is.null(object$lv.X)&object$randomB==FALSE) {
    if(!rotate|num.lv>0&(num.lv.c+num.RR)>0){
      pars <- c(object$params$LvXcoef)
      se <- c(object$sd$LvXcoef)
      zval <- pvalue <- rep(NA,length(pars))
      if(by=="all"){
      zval <- pars/se
      pvalue <- 2 * pnorm(-abs(zval))
      coef.table.constrained <- cbind(pars, se, zval, pvalue)
      dimnames(coef.table.constrained) <- list(paste(colnames(object$lv.X),"(CLV",rep(1:(object$num.lv.c+object$num.RR),each=ncol(object$lv.X)),")",sep=""), c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
      }else if(by=="terms"){
        covB <- object$Hess$cov.mat.mod
        colnames(covB) <- row.names(covB) <- names(object$TMBfn$par)[object$Hess$incl]
        covB <- covB[row.names(covB)=="b_lv",colnames(covB)=="b_lv"]
        for(i in 1:ncol(object$lv.X)){
          idx <- seq(i,ncol(object$lv.X)*(object$num.lv.c+object$num.RR),ncol(object$lv.X))
          b <- object$params$LvXcoef[i,]
          zval[i] <- b%*%MASS::ginv(covB[idx,idx])%*%b
          pvalue[i] <- 1-pchisq(zval[i],object$num.lv.c+object$num.RR)
        }
        coef.table.constrained <- cbind(pars, se, zval, pvalue)
        dimnames(coef.table.constrained) <- list(paste(colnames(object$lv.X),"(CLV",rep(1:(object$num.lv.c+object$num.RR),each=ncol(object$lv.X)),")",sep=""), c("Estimate", "Std. Error", "X2 value", "Pr(>X2)"))
      }else if(by=="LV"){
        covB <- object$Hess$cov.mat.mod
        colnames(covB) <- row.names(covB) <- names(object$TMBfn$par)[object$Hess$incl]
        covB <- covB[row.names(covB)=="b_lv",colnames(covB)=="b_lv"]
        for(i in 1:(object$num.RR+object$num.lv.c)){
          b <- object$params$LvXcoef[,i]
          zval[1+ncol(object$lv.X)*(i-1)] <- b%*%MASS::ginv(covB[(1:ncol(object$lv.X))+ncol(object$lv.X)*(i-1),(1:ncol(object$lv.X))+ncol(object$lv.X)*(i-1)])%*%b
          pvalue[1+ncol(object$lv.X)*(i-1)] <- 1-pchisq(zval[1+ncol(object$lv.X)*(i-1)],ncol(object$lv.X))
        }
        coef.table.constrained <- cbind(pars, se, zval, pvalue)
        dimnames(coef.table.constrained) <- list(paste(colnames(object$lv.X),"(CLV",rep(1:(object$num.lv.c+object$num.RR),each=ncol(object$lv.X)),")",sep=""), c("Estimate", "Std. Error", "X2 value", "Pr(>X2)"))
      }
    }else{
      LVcoef <- (object$params$LvXcoef%*%svd_rotmat_sites)
      pars <- c(LVcoef)
      covB <- object$Hess$cov.mat.mod
      colnames(covB) <- row.names(covB) <- names(object$TMBfn$par)[object$Hess$incl]
      covB <- covB[row.names(covB)=="b_lv",colnames(covB)=="b_lv", drop=FALSE]
      zval <- pvalue <- rep(NA,length(pars))
      rotSD <- matrix(0,ncol=num.RR+num.lv.c,nrow=ncol(object$lv.X)) 
      for(i in 1:ncol(object$lv.X)){
        rotSD[i,] <- sqrt(abs(diag(t(svd_rotmat_sites[1:(num.lv.c+num.RR),1:(num.lv.c+num.RR)])%*%covB[seq(i,(num.RR+num.lv.c)*ncol(object$lv.X),by=ncol(object$lv.X)),seq(i,(num.RR+num.lv.c)*ncol(object$lv.X),by=ncol(object$lv.X))]%*%svd_rotmat_sites[1:(num.lv.c+num.RR),1:(num.lv.c+num.RR)])))
      }
      se <- c(rotSD)
      if(by=="all"){
        pars <- c(LVcoef)
        zval <- pars/se
        pvalue <- 2 * pnorm(-abs(zval))
        coef.table.constrained <- cbind(pars, se, zval, pvalue)
        dimnames(coef.table.constrained) <- list(paste(colnames(object$lv.X),"(CLV",rep(1:(object$num.lv.c+object$num.RR),each=ncol(object$lv.X)),")",sep=""), c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
      }else if(by=="terms"){
        # This test is invariant to rotation, but the rotation matrix is included for posterity
        for(i in 1:ncol(object$lv.X)){
          idx <- seq(i,ncol(object$lv.X)*(object$num.lv.c+object$num.RR),ncol(object$lv.X))
          b <- LVcoef[i,]
          zval[i] <- b%*%MASS::ginv(t(svd_rotmat_sites[1:(num.lv.c+num.RR),1:(num.lv.c+num.RR)])%*%covB[idx,idx]%*%svd_rotmat_sites[1:(num.lv.c+num.RR),1:(num.lv.c+num.RR)])%*%b
          pvalue[i] <- 1-pchisq(zval[i],object$num.lv.c+object$num.RR)
        }
        coef.table.constrained <- cbind(pars, se, zval, pvalue)
        dimnames(coef.table.constrained) <- list(paste(colnames(object$lv.X),"(CLV",rep(1:(object$num.lv.c+object$num.RR),each=ncol(object$lv.X)),")",sep=""), c("Estimate", "Std. Error", "X2 value", "Pr(>X2)"))
      }else if(by=="LV"){
        covB <- object$Hess$cov.mat.mod
        colnames(covB) <- row.names(covB) <- names(object$TMBfn$par)[object$Hess$incl]
        covB <- covB[row.names(covB)=="b_lv",colnames(covB)=="b_lv"]

        for(q in 1:(object$num.RR+object$num.lv.c)){
         # Rotated cov matrix for the qth LV
          covBnew <- matrix(0,ncol=ncol(object$lv.X),nrow=ncol(object$lv.X))        
        for(k in 1:ncol(object$lv.X)){
          for(k2 in 1:ncol(object$lv.X)){
            for(q2 in 1:(object$num.RR+object$num.lv.c)){
              for(q3 in 1:(object$num.RR+object$num.lv.c)){
                covBnew[k,k2] <- covBnew[k,k2]+svd_rotmat_sites[1:(object$num.RR+object$num.lv.c),1:(object$num.RR+object$num.lv.c)][q2,q]*svd_rotmat_sites[1:(object$num.RR+object$num.lv.c),1:(object$num.RR+object$num.lv.c)][q3,q]*covB[(object$num.RR+object$num.lv.c)*(k-1)+q2,(object$num.RR+object$num.lv.c)*(k2-1)+q3]
              }
            }
          }
        }
          b <- LVcoef[,q]
          zval[1+ncol(object$lv.X)*(q-1)] <- b%*%solve(covBnew)%*%b
          pvalue[1+ncol(object$lv.X)*(q-1)] <- 1-pchisq(zval[1+ncol(object$lv.X)*(q-1)],ncol(object$lv.X))
        }
        coef.table.constrained <- cbind(pars, se, zval, pvalue)
        dimnames(coef.table.constrained) <- list(paste(colnames(object$lv.X),"(CLV",rep(1:(object$num.lv.c+object$num.RR),each=ncol(object$lv.X)),")",sep=""), c("Estimate", "Std. Error", "X2 value", "Pr(>X2)"))
        }
      }
  }else{
    coef.table.constrained <- NULL
  }
  if(!is.null(type)){
    if(type=="residual"){
      coef.table.constrained <- NULL
    }
  }
  
  colnames(M) <- newnams
  rownames(M) <- colnames(object$y)
  sumry$Call <- object$call
  sumry$family <- object$family
  sumry$Coefficients <- M
  
  if (!is.null(object$TR)) {
    # if (!is.null(object$X)) {
    #   sumry$'Covariate coefficients' <- object$params$B
    # }
  } else {
    # if (!is.null(object$X)) {
    #   sumry$'Environmental coefficients' <- object$params$Xcoef
    # }
  }
  if (!is.null(object$params$row.params)) {
    sumry$'Row intercepts' <- object$params$row.params
  }
  sumry$rstruc <- object$rstruc
  if (object$row.eff == "random") {
    object$params$sigma2 = object$params$sigma[1] ^ 2
    names(object$params$sigma2) = "sigma^2"
    sumry$'Variance of random row intercepts' <- object$params$sigma2
  }
  
  if (object$family == "negative.binomial") {
    sumry$'Dispersion parameters' <- object$params$phi
  }
  if (object$family == "gamma") {
    sumry$'Shape parameters' <- object$params$phi
  }
  if (object$family == "tweedie") {
    sumry$'Dispersion parameters' <- object$params$phi
  }
  if (object$family == "ZIP") {
    sumry$'Zero inflation p' <- object$params$phi
  }
  if(object$family == "gaussian"){
    sumry$'Standard deviations' <- object$params$phi
  }
  if(!is.null(object$X)){
    sumry$'Coef.tableX' <- coef.table
  }
  if((num.lv+num.lv.c)>0){
    
    #add zeros for num.rr
    object$params$sigma.lv <- c(object$params$sigma.lv[1:num.lv.c],rep(0,num.RR), if(num.lv>0)object$params$sigma.lv[-c(1:(num.lv.c+num.RR))])
    
    if(rotate){
      object$params$sigma.lv <- sqrt(diag(t(svd_rotmat_sites)%*%diag(object$params$sigma.lv^2, ncol = num.lv+num.lv.c, nrow = num.lv+num.lv.c)%*%svd_rotmat_sites))
    }
    sumry$sigma.lv <- object$params$sigma.lv
  }
  
  if((object$num.lv.c+object$num.RR)>0&!is.null(coef.table.constrained)){
    sumry$'Coef.tableLV' <- coef.table.constrained
  }
  class(sumry) <- "summary.gllvm"
  return(sumry)
}

#'@export
#'@rdname summary.gllvm 
print.summary.gllvm <- function (x, ...) 
{
  cat("\nCall:\n", paste(deparse(x$Call), sep = "\n", 
                         collapse = "\n"), "\n\n", sep = "")
  cat("Family: ", x$family, "\n\n")
  
  AIC <- round(x$AIC,x$digits)
  BIC <- round(x$BIC,x$digits)
  AICc <- round(x$AICc,x$digits)
  
  cat("AIC: ", AIC, "AICc: ", AICc, "BIC: ", BIC, "LL: ", zapsmall(x$`log-likelihood`, x$digits), "df: ", x$df, "\n\n")
  
  cat("Informed LVs: ", x$num.lv.c, "\n")
  cat("Constrained LVs: ", x$num.RR,"\n")
  cat("Unconstrained LVs: ", x$num.lv, "\n")
  
  #this scenario we don't want the SD from num.lv as it is meaningless
  if(x$num.lv>0&(x$num.RR+x$num.lv.c)>0 & isFALSE(x$quadratic))x$sigma.lv <- x$sigma.lv[1:(x$num.lv.c+x$num.RR)]
  
  #only print SD from LV if model is quadratic or if (hybrid) concurrent
  if((x$num.lv.c)>0|!isFALSE(x$quadratic)){cat("Residual standard deviation of LVs: ", zapsmall(x$sigma.lv,x$digits),"\n\n")}else{cat("\n")}
  
  cat("Formula: ", paste(x$formula,collapse=""), "\n")
  cat("LV formula: ", ifelse(is.null(x$lv.formula),"~ 0", paste(x$lv.formula,collapse="")), "\n")
  
  df <- x[["df"]]
  if(!is.null(x$Coef.tableX)){
    cat("\nCoefficients predictors:\n")
    coefs <- x$Coef.tableX
    
    printCoefmat(coefs, digits = x$digits, signif.stars = x$signif.stars, 
                 na.print = "NA")
  }
  if(x$Lvcoefs){
    if((x$num.lv+x$num.lv.c+x$num.RR)>0){
      if(x$quadratic==F){
        cat("\nCoefficients LVs: \n")  
      }else{
        cat("\nOptima LVs: \n")
      }
      
      print(x$Coefficients[,-1,drop=F])
    }
  }
  
  if(!is.null(x$Coef.tableLV)){
    cat("\nCoefficients LV predictors:\n")
    coefs <- x$Coef.tableLV
    
    printCoefmat(coefs, digits = x$digits, signif.stars = x$signif.stars, 
                 na.print = "")
  }
  if(x$spp.intercepts){
    cat("\n Species Intercepts: \n")
    print(zapsmall(x$Coefficients[,1],x$digits))
  }
  if(x$row.intercepts){
    if(!is.null(x$`Row intercepts`)){
      cat("\n Row intercepts with variance", zapsmall(x$'Variance of random row intercepts',x$digits), ":\n")
      print(zapsmall(x$`Row intercepts`,x$digits))
    }
  }
  if(x$dispersion){
    
    if (x$family == "negative.binomial") {
      phi <- x$'Dispersion parameters'
    }
    if (x$family == "gamma") {
      phi <- x$'Shape parameters'
    }
    if (x$family == "tweedie") {
      phi <- x$'Dispersion parameters'
    }
    if (x$family == "ZIP") {
      phi <- x$'Zero inflation p'
    }
    if(x$family == "gaussian"){
      phi <- x$'Standard deviations'
    }
    if(x$family%in%c("negative.binomial","gamma","tweedie","ZIP","gaussian")){
      # names(phi) <- row.names(x$Coefficients)
      cat("\n(Dispersion estimates for ", x$family, ":\n")
      print(phi)
    }
  }
  
  invisible(x)
}