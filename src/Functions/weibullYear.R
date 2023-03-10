require(cardidates)
weibull.year <- function(df, var, find = 'max', datacutoff = 8) {
  
  usedata = df[!is.na(df[,var]),]
  
  if (nrow(usedata) < datacutoff) {
    output = data.frame(dayWeibull = NA, weibull.r2 = NA)
    return(output)
  }
  
  if (sum(usedata[,var]) <= 0) {
    output = data.frame(dayWeibull = NA, weibull.r2 = NA)
    return(output)
  }
  
  if (find == 'min') {
    usedata[,var] = usedata[,var] * -1 + max(usedata[,var], na.rm = T)
  }
  
  # plot(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)
  
  res <- fitweibull6(usedata$daynum, pull(usedata[,var]))
  
  possibleError <- tryCatch({
    CDW(res, xmin=0, xmax=365, quantile=0.05, symmetric=FALSE)
  }, error=function(e) e)
  
  if(inherits(possibleError, "error")) {
    output = data.frame(dayWeibull = -999, weibull.r2 = -999)
  } else {
    r2 = res$r2
    dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
    # plot(res, xmin = 50, xmax = 365)
    # plot(res$fit$x, res$fit$y)
    # points(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)
    output = data.frame(dayWeibull = dayWeibull, weibull.r2 = r2)
  }
  
  return(output)
}