#'@export


coef.gllvm <- function(object, ...)
{
  names(object$params)[names(object$params)=="beta0"]="Intercept"
  if(object$row.eff) names(object$params)[names(object$params)=="row.params"]="Row.Intercept"
  return(object$params)
}
