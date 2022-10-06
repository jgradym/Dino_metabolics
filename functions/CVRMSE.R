cvrmse <- function(res){
  res.cv <- sqrt(mean(res$residuals^2))
  
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  
  cvrmse <- (1/mean(res[["model"]][[1]]))*res.cv
  return(cvrmse)

}

