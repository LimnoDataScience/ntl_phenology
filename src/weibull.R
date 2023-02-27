library(cardidates)
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2427.2009.02240.x?casa_token=jxvs0LjoYvkAAAAA%3AeJajQFL8mWMHvxTZy0m5ymdyfveUGoY-9Jk9QW0AtFz5KaS1juIAB_d_jrwKvyf93-FmOoz7M3G2fg
# https://search.r-project.org/CRAN/refmans/cardidates/html/fitweibull.html
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2427.2011.02614.x?casa_token=Fo45BZMbi5UAAAAA%3AaCMjoJXMotdbGniisG3h9rx6pXmq4snfUNPdDz5Qs_Y0y2yJOjXC70ySXJAA_mWUjSJEmekQwjgbFw
secchi

secchi |> 
  group_by(lakeid, year4) |> 
  summarise(C = sum(daynum*secnview)/sum(secnview)) |> 
  ggplot() +
  geom_point(aes(x = year4, y = C)) +
  facet_wrap(~lakeid)

ggplot(secchi) +
  geom_point(aes(x = daynum, y = secnview)) +
  facet_wrap(~lakeid, scales = 'free')

C = sum(D*xd) / sum(xd)

usedata = secchi |> filter(lakeid == 'ME', year4 == 2017)
res <- fitweibull6(usedata$daynum, usedata$secnview)
res$r2
plot(res)
points(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)



usedata = epi_min |> filter(lakeid == 'BM', item == 'totpuf', year4 == 2012) |> 
  filter(daynum <350)
plot(usedata$daynum, usedata$value, col = 'blue', pch = 16)

res <- fitweibull6(usedata$daynum, 1/usedata$value)
res$r2
plot(res)
points(usedata$daynum, usedata$value, col = 'blue', pch = 16)


######### Zooplankton density #####################
lakenames = unique(zoopDensity.cc$lakeid)
years = unique(zoopDensity.cc$year4)
weibull.zoop.list = list()
for (i in 1:length(lakenames)) {
  for (ii in 1:length(years)) {
    usedata = zoopDensity.cc |> filter(lakeid == lakenames[i], year4 == years[ii]) |> 
      mutate(daynum = yday(sample_date))
    
    if (nrow(usedata) < 8) {
      weibull.zoop.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = NA, r2 = NA)
      next
    }
    
    plot(usedata$daynum, usedata$density, col = 'blue', pch = 16)
    
    res <- fitweibull6(usedata$daynum, usedata$density)
    
    possibleError <- tryCatch({
      CDW(res, xmin=0, xmax=365, quantile=0.05, symmetric=FALSE)
    }, error=function(e) e)
    
    if(inherits(possibleError, "error")) {
      weibull.zoop.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = -999, r2 = -999)
    } else {
      r2 = res$r2
      dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
      plot(res, xmin = 50, xmax = 365)
      plot(res$fit$x, res$fit$y)
  
      points(usedata$daynum, usedata$density, col = 'blue', pch = 16)
      weibull.zoop.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = dayWeibull, r2 = r2)
    }
    
  } 
}

weibull.zoop = do.call(rbind.data.frame, weibull.zoop.list)


weibull.zoop |> 
  filter(dayWeibull > 0) |> 
  ggplot() +
  geom_point(aes(x = year, y = dayWeibull)) +
  facet_wrap(~lakeid)

weibull.zoop |> 
  filter(dayWeibull > 0) |> 
  ggplot() +
  geom_density(aes(x = dayWeibull)) +
  facet_wrap(~lakeid)

######### Secchi #####################
secchi

lakenames = unique(secchi$lakeid)
years = unique(secchi$year4)
weibull.secchi.list = list()
for (i in 1:length(lakenames)) {
  for (ii in 1:length(years)) {
    usedata = secchi |> filter(lakeid == lakenames[i], year4 == years[ii]) 
    
    usedata = secchi |> filter(lakeid == 'ME', year4 == 2017) 
    
    if (nrow(usedata) < 8) {
      weibull.secchi.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = NA, r2 = NA)
      next
    }
    
    plot(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)
    
    res <- fitweibull6(usedata$daynum, usedata$secnview)
    
    possibleError <- tryCatch({
      CDW(res, xmin=0, xmax=365, quantile=0.05, symmetric=FALSE)
    }, error=function(e) e)
    
    if(inherits(possibleError, "error")) {
      weibull.secchi.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = -999, r2 = -999)
    } else {
      r2 = res$r2
      dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
      plot(res, xmin = 50, xmax = 365)
      plot(res$fit$x, res$fit$y)
      
      points(usedata$daynum, usedata$secnview, col = 'blue', pch = 16)
      weibull.secchi.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = dayWeibull, r2 = r2)
    }
    
  } 
}

weibull.secchi = do.call(rbind.data.frame, weibull.secchi.list)


weibull.secchi |> 
  filter(dayWeibull > 0) |> 
  ggplot() +
  geom_point(aes(x = year, y = dayWeibull)) +
  facet_wrap(~lakeid)

weibull.secchi |> 
  filter(dayWeibull > 0) |> 
  ggplot() +
  geom_density(aes(x = dayWeibull)) +
  facet_wrap(~lakeid)


######### hyponuts #####################
hyponuts = hypo_max |> filter(item == 'totpuf')

lakenames = unique(secchi$lakeid)
years = unique(secchi$year4)
weibull.hyponuts.list = list()
for (i in 1:length(lakenames)) {
  for (ii in 1:length(years)) {
    usedata = hyponuts |> filter(lakeid == lakenames[i], year4 == years[ii])
    
    if (nrow(usedata) < 8) {
      weibull.hyponuts.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = NA, r2 = NA)
      next
    }
    
    plot(usedata$daynum, usedata$value, col = 'blue', pch = 16)
    
    res <- fitweibull6(usedata$daynum, usedata$value)
    
    possibleError <- tryCatch({
      CDW(res, xmin=0, xmax=365, quantile=0.05, symmetric=FALSE)
    }, error=function(e) e)
    
    if(inherits(possibleError, "error")) {
      weibull.hyponuts.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = -999, r2 = -999)
    } else {
      r2 = res$r2
      dayWeibull = res$fit |> filter(f == max(f)) |> pull(x)
      plot(res, xmin = 50, xmax = 365)
      plot(res$fit$x, res$fit$y)
      
      points(usedata$daynum, usedata$value, col = 'blue', pch = 16)
      weibull.hyponuts.list[[paste0(lakenames[i], years[ii])]] = 
        data.frame(lakeid = lakenames[i], year = years[ii], dayWeibull = dayWeibull, r2 = r2)
    }
    
  } 
}

weibull.hyponuts = do.call(rbind.data.frame, weibull.hyponuts.list)


weibull.hyponuts |> 
  filter(dayWeibull > 0) |> 
  ggplot() +
  geom_point(aes(x = year, y = dayWeibull)) +
  facet_wrap(~lakeid)

weibull.hyponuts |> 
  filter(dayWeibull > 0) |> 
  ggplot() +
  geom_density(aes(x = dayWeibull)) +
  facet_wrap(~lakeid)

