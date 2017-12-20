#'@export

print.gllvm <- function(x, ...) {
  cat("Call: \n")
  print(x$call)
  cat("family: \n")
  print(x$family)
  cat("method: \n")
  print(x$method)
  cat("\n")
  cat("log-likelihood: ",x$logL,"\n")
  if(!is.null(x$params$inv.phi)){ x$params$inv.phi<-NULL; }
  df=length(unlist(x$params))-x$num.lv*(x$num.lv-1)/2
  if(x$row.eff %in% c("fixed",TRUE) ) df=df-1
  cat("Degrees of freedom: ",df,"\n")
  cat("AIC: ",AIC(x),"\n")
  cat("BIC: ",BIC(x),"\n")
}
