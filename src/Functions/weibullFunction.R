require(cardidates)
weibull <- function(uselake, useyear, df, var, find = 'max', datacutoff = 8) {
    
  usedata = df |> filter(lakeid == uselake, year == useyear) 
    if (nrow(usedata) < datacutoff) {
      output = data.frame(lakeid = uselake, year = useyear, dayWeibull = NA, weibull.r2 = NA)
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
      output = data.frame(lakeid = uselake, year = useyear, dayWeibull = -999, weibull.r2 = -999)
    } else {
      r2 = res$r2
      dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
      # plot(res, xmin = 50, xmax = 365)
      # plot(res$fit$x, res$fit$y)
      # points(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)
      output = data.frame(lakeid = uselake, year = useyear, dayWeibull = dayWeibull, weibull.r2 = r2)
    }

    return(output)
}

# weibull(df = secchi |> filter(yday(sampledate) < 200), uselake = 'ME', useyear = 2008, var = 'secnview', find = 'max')
# weibull(df = secchi, uselake = 'ME', useyear = 2012, var = 'secnview', find = 'max')
