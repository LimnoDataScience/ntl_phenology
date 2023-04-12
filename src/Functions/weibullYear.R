# https://link.springer.com/article/10.1007/s00442-007-0783-2
# Identifying cardinal dates in phytoplankton time series to enable the analysis of long-term trends

require(cardidates)
source('src/Functions/weibullCDW.R')

weibull.year <- function(df, var, find = 'max', cardinal = 'mid', datacutoff = 8) {
  
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
    # CDW(res, xmin=0, xmax=365, quantile=0.05, symmetric=FALSE)
    weibullCDW(res, quantile = 0.05)
  }, error=function(e) e)
  
  if(inherits(possibleError, "error")) {
    output = data.frame(dayWeibull = -999, weibull.r2 = -999)
  } else {
    ## identify cardinal dates from fitted curves
    # smd <- CDW(res, xmin=0, xmax=365, quantile=0.05, symmetric=FALSE)
    smd = weibullCDW(res, quantile = 0.05)
    
    r2 = res$r2
    # dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
    
    if (cardinal == 'begin') {
      dayWeibull = smd$x['tBegin']
    } else if (cardinal == 'mid') {
      dayWeibull = smd$x['tMid']
    } else if (cardinal == 'end') {
      dayWeibull = smd$x['tEnd']
    }
    
    weibull.max = smd$y[1] > smd$y[2] & smd$y[1] > smd$y[3]
    
    #### Adjustment if cardinal dates don't match up
    ## Adds value 30 days prior to first observation. 
    if(weibull.max == FALSE) {
      usedata = usedata[1,] |> bind_rows(usedata)
      usedata$daynum[1] = usedata$daynum[1] - 30
      
      if (find == 'min') {
        usedata[,var] = usedata[,var] * -1 + max(usedata[,var], na.rm = T)
        usedata[,var][1,] = max(usedata[,var])
      } else {
        usedata[,var][1,] = min(usedata[,var])
      }
      res <- fitweibull6(usedata$daynum, pull(usedata[,var]))
      smd = weibullCDW(res, quantile = 0.05)
      
      r2 = res$r2
      # dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
      
      if (cardinal == 'begin') {
        dayWeibull = smd$x['tBegin']
      } else if (cardinal == 'mid') {
        dayWeibull = smd$x['tMid']
      } else if (cardinal == 'end') {
        dayWeibull = smd$x['tEnd']
      }
      
      weibull.max = smd$y[1] > smd$y[2] & smd$y[1] > smd$y[3]
      weibull.adjust = TRUE
    } else {
      weibull.adjust = FALSE
    }
    
    # plot(res, xmin = 50, xmax = 365)
    # plot(res$fit$x, res$fit$y)
    # points(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)
    output = data.frame(dayWeibull = dayWeibull, weibull.r2 = r2, weibull.max = weibull.max, weibull.adjust = weibull.adjust)
  }
  
  return(output)
}