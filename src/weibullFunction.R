secchi

lakenames = unique(secchi$lakeid)
years = unique(secchi$year4)
weibull.secchi.list = list()

weibull <- function(lakeid, year, df, var) {
    usedata = df |> filter(lakeid == lakeid, year4 == year) 


    if (nrow(usedata) < 8) {
      output = data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = NA, r2 = NA)
      next
    }

    # plot(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)

    res <- fitweibull6(usedata$daynum, pull(usedata[,var]))
    
    possibleError <- tryCatch({
      CDW(res, xmin=0, xmax=365, quantile=0.05, symmetric=FALSE)
    }, error=function(e) e)

    if(inherits(possibleError, "error")) {
      output = data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = -999, r2 = -999)
    } else {
      r2 = res$r2
      dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
      # plot(res, xmin = 50, xmax = 365)
      # plot(res$fit$x, res$fit$y)
      # points(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)
      output = data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = dayWeibull, r2 = r2)
    }

    return(output)
}

weibull(df = secchi, lakeid = 'ME', year = 2012, var = 'secnview')
