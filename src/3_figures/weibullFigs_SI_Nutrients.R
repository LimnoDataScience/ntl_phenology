library(tidyverse)
library(cardidates)
library(lubridate)
library(patchwork)
library(scales)
source('src/Functions/weibullCDW.R')

#################### LOAD Nutrient DATA ####################
lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")

nuts = read_csv('Data/derived/nutrients.csv')

plotWeibull <- function(usedf, find = 'max', r2size = 3) {
  # fit weibull, estimate new fit for plotting
  
  df = usedf 
  
  # Flip data if min
  if (find == 'min') {
    df$value = df$value * -1 + max(df$value, na.rm = T)
  }
  
  res <- fitweibull6(df$daynum, df$value)
  fit <- res$fit |> 
    mutate(newy = fweibull6(x, res$p) * res$ymax)
  # r2
  res$r2
  
  # identify maximum
  secchi.max = df |> filter(value == max(value)) |> slice(1)
  
  ## identify cardinal dates from fitted curves
  smd <- weibullCDW(res, quantile=0.05)
  smd.out = data.frame(x = smd[[1]], y = smd[[2]]* res$ymax)
  
  ## is smd Begin > smd End?
  weibull.max = smd$y[1] > smd$y[2] & smd$y[1] > smd$y[3] 
  
  if(weibull.max == FALSE){
    df2 = df[1,] |> bind_rows(df)
    df2$daynum[1] = df2$daynum[1] - 30
    df2$value[1] = min(df2$value)
    
    res2 <- fitweibull6(df2$daynum, df2$value)
    fit2 <- res2$fit |> 
      mutate(newy = fweibull6(x, res2$p) * res2$ymax)
    # r2
    res2$r2
    
    ## identify cardinal dates from fitted curves
    smd2 <- weibullCDW(res2, quantile=0.05)
    smd2.out = data.frame(x = smd2[[1]], y = smd2[[2]]* res2$ymax)
  }
  
  # Turn back around 
  if (find == 'min') {
    df = usedf
    smd.out$y = (smd.out$y -  max(df$value, na.rm = T)) / -1
    fit$newy = (fit$newy -  max(df$value, na.rm = T)) / -1
  }
  
  if (find == 'min' & weibull.max == FALSE) {
    smd2.out$y = (smd2.out$y -  max(df$value, na.rm = T)) / -1
    fit2$newy = (fit2$newy -  max(df$value, na.rm = T)) / -1
    df2$value = (df2$value -  max(df$value, na.rm = T)) / -1
  }
  
  # Plotting
  p1 = ggplot(df) +
    geom_vline(data = secchi.max, aes(xintercept = as.Date(daynum, origin = as.Date('2020-01-01'))), linetype = 2, linewidth = 0.4) +
    geom_point(aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = value), size = 0.8) +
    geom_path(data = fit, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'lightblue3') +
    geom_point(data = smd.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
               fill = 'lightblue3', shape = 21, stroke = 0.2, size = 2) +
    annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-01-01')), y = Inf, 
             label = paste0('r^2 == ', round(res$r2, 2)), hjust = 0, vjust = 2, parse = TRUE, color = 'lightblue4', size = r2size) + 
    scale_x_date(labels = date_format("%b"), breaks = '4 month', minor_breaks = '1 month') +
    theme_bw(base_size = 7) +
    theme(axis.title.x = element_blank(),plot.title = element_text(size = 7))
  
  if(weibull.max == FALSE){
    p1 = p1 + 
      geom_point(data = df2[1,], aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = value), size = 0.8, color = 'salmon1') +
      geom_path(data = fit2, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'salmon1') +
      geom_point(data = smd2.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
                 fill = 'salmon1', shape = 21, stroke = 0.2, size = 2) +
      annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-01-01')), y = Inf, 
               label = paste0('r^2 == ', round(res2$r2, 2)), hjust = 0, vjust = 3, parse = TRUE, color = 'salmon1', size = r2size)
  }
  
  return(p1)
  
}

############ ALL lakes Nutrients - Surface ##############
### max vars ###
vars = c('ph', 'doc')

for (k in 1:length(vars)) {
  nuts.vars = nuts |> filter(item == vars[k]) |> filter(layer == 'surf')
  
  for (j in 1:length(lakes_order)) {
    df.plots = list()
    useyears = unique(nuts.vars |> filter(lakeid == lakes_order[j]) |> pull(year))
    
    if(length(useyears) == 0) {next}
    for (i in 1:length(useyears)) {
      df = nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i])
      
      if (nrow(df) > 0 & sum(df$value > 0)) {
        df.plots[[i]] = plotWeibull(nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i]), find = 'max', r2size = 2) + 
          labs(title = paste0(lakes_order[j],': ',useyears[i])) +
          ylab(vars[k])
      } else {
        df.plots[[i]] = ggplot() + theme_void()
      }
    }
    patchwork::wrap_plots(df.plots)
    ggsave(paste0('Figures_manuscript/Weibull_Nutrients_Surf/',vars[k],'_max_',lakes_order[j],'.png'), width = 6, height = 9, dpi = 500)
  }
}

### min vars ###
vars = c('drsif', 'ph', 'doc',  'totnuf', 'totpuf', 'totnf', 'totpf', 'drp', 'nh4', 'no3no2')
# vars = 'totnuf'
for (k in 1:length(vars)) {
  nuts.vars = nuts |> filter(item == vars[k]) |> filter(layer == 'surf')
  
  for (j in 1:length(lakes_order)) {
    df.plots = list()
    useyears = unique(nuts.vars |> filter(lakeid == lakes_order[j]) |> pull(year))
    
    if(length(useyears) == 0) {next}
    for (i in 1:length(useyears)) {
      df = nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i])
      
      if (nrow(df) > 0 & sum(df$value > 0)) {
        df.plots[[i]] = plotWeibull(nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i]), find = 'min', r2size = 2) + 
          labs(title = paste0(lakes_order[j],': ',useyears[i])) +
          ylab(vars[k])
      } else {
        df.plots[[i]] = ggplot() + theme_void()
      }
    }
    patchwork::wrap_plots(df.plots)
    ggsave(paste0('Figures_manuscript/Weibull_Nutrients_Surf/',vars[k],'_min_',lakes_order[j],'.png'), width = 6, height = 9, dpi = 500)
  }
}

### min vars - SPRING ###
vars = c('drsif')

for (k in 1:length(vars)) {
  nuts.vars = nuts |> filter(item == vars[k]) |> filter(layer == 'surf')
  
  for (j in 1:length(lakes_order)) {
    df.plots = list()
    useyears = unique(nuts.vars |> filter(lakeid == lakes_order[j]) |> pull(year))
    
    if(length(useyears) == 0) {next}
    for (i in 1:length(useyears)) {
      df = nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i]) |> 
        filter(yday(sampledate) <= 274)
      
      if (nrow(df) > 0 & sum(df$value > 0)) {
        df.plots[[i]] = plotWeibull(df, find = 'min', r2size = 2) + 
          labs(title = paste0(lakes_order[j],': ',useyears[i])) +
          ylab(vars[k])
      } else {
        df.plots[[i]] = ggplot() + theme_void()
      }
    }
    patchwork::wrap_plots(df.plots)
    ggsave(paste0('Figures_manuscript/Weibull_Nutrients_Surf/',vars[k],'_SpringMin_',lakes_order[j],'.png'), width = 6, height = 9, dpi = 500)
  }
}


