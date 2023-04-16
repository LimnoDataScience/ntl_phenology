library(tidyverse)
library(cardidates)
library(lubridate)
library(patchwork)
library(scales)
source('src/Functions/weibullCDW.R')

#################### LOAD Nutrient DATA ####################
lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")

nuts = read_csv('Data/derived/nutrients.csv')

fitWeibull <- function(usedf, find = 'max') {
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
  fit$r2 = res$r2
  fit$year = usedf$year[1]
  fit$adjust = FALSE
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
    fit <- res2$fit |> 
      mutate(newy = fweibull6(x, res2$p) * res2$ymax)
    
    fit$r2 = res2$r2
    fit$year = usedf$year[1]
    fit$adjust = TRUE
  }
  
  # Turn back around 
  if (find == 'min') {
    df = usedf
    fit$newy = (fit$newy -  max(df$value, na.rm = T)) / -1
  }
  
  return(fit)
  
  # # Plotting
  # p1 = ggplot(df) +
  #   geom_vline(data = secchi.max, aes(xintercept = as.Date(daynum, origin = as.Date('2020-01-01'))), linetype = 2, linewidth = 0.4) +
  #   geom_point(aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = value), size = 0.8) +
  #   geom_path(data = fit, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'lightblue3') +
  #   geom_point(data = smd.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
  #              fill = 'lightblue3', shape = 21, stroke = 0.2, size = 2) +
  #   annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-01-01')), y = Inf, 
  #            label = paste0('r^2 == ', round(res$r2, 2)), hjust = 0, vjust = 2, parse = TRUE, color = 'lightblue4', size = r2size) + 
  #   scale_x_date(labels = date_format("%b"), breaks = '4 month', minor_breaks = '1 month') +
  #   theme_bw(base_size = 7) +
  #   theme(axis.title.x = element_blank(),plot.title = element_text(size = 7))
  # 
  # return(p1)
  
}

############ ALL lakes Nutrients - Surface ##############
### max vars ###
vars = c('ph', 'doc')

for (k in 1:length(vars)) {
  nuts.vars = nuts |> filter(item == vars[k]) |> filter(layer == 'surf')
  df.plots = list()
  
  for (j in 1:length(lakes_order)) {
    fit.list = list()
    useyears = unique(nuts.vars |> filter(lakeid == lakes_order[j]) |> pull(year))
    
    if(length(useyears) == 0) {next}
    for (i in 1:length(useyears)) {
      df = nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i])
      
      if (nrow(df) > 0 & sum(df$value > 0)) {
        fit.list[[i]] = fitWeibull(df, find = 'max')
      } 
    }
    
    fit.df = do.call(rbind.data.frame, fit.list)
    
    df.plots[[j]] = ggplot(fit.df) + #|> filter(r2 >= 0.7)) +
      geom_path(aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy, group = year), alpha = 0.6, linewidth = 0.2) +
      geom_smooth(aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = '#e88361', linewidth = 2) +
      ylab(vars[k]) +
      labs(title = lakes_order[j]) + 
      theme_bw(base_size = 7) +
      theme(axis.title.x = element_blank(), plot.title = element_text(size = 7))
  }
  df.plots = df.plots[lengths(df.plots) != 0]
  
  patchwork::wrap_plots(df.plots)
  ggsave(paste0('Figures_manuscript/Weibull_Fits_Nutrients/',vars[k],'_max.png'), width = 6, height = 9, dpi = 500)
}

### min vars ###
vars = c('drsif', 'ph', 'doc',  'totnuf', 'totpuf', 'totnf', 'totpf', 'drp', 'nh4', 'no3no2')

for (k in 1:length(vars)) {
  nuts.vars = nuts |> filter(item == vars[k]) |> filter(layer == 'surf')
  df.plots = list()
  
  for (j in 1:length(lakes_order)) {
    fit.list = list()
    useyears = unique(nuts.vars |> filter(lakeid == lakes_order[j]) |> pull(year))
    
    if(length(useyears) == 0) {next}
    for (i in 1:length(useyears)) {
      df = nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i])
      
      if (nrow(df) > 0 & sum(df$value > 0)) {
        fit.list[[i]] = fitWeibull(df, find = 'min')
      } 
    }
    
    fit.df = do.call(rbind.data.frame, fit.list)
    
    df.plots[[j]] = ggplot(fit.df) + #|> filter(r2 >= 0.7)) +
      geom_path(aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy, group = year), alpha = 0.6, linewidth = 0.2) +
      geom_smooth(aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = '#e88361', linewidth = 2) +
      ylab(vars[k]) +
      labs(title = lakes_order[j]) + 
      theme_bw(base_size = 7) +
      theme(axis.title.x = element_blank(), plot.title = element_text(size = 7))
  }
  df.plots = df.plots[lengths(df.plots) != 0]
  
  patchwork::wrap_plots(df.plots)
  ggsave(paste0('Figures_manuscript/Weibull_Fits_Nutrients/',vars[k],'_min.png'), width = 6, height = 9, dpi = 500)
}

### min vars - SPRING ###
vars = c('drsif')

for (k in 1:length(vars)) {
  nuts.vars = nuts |> filter(item == vars[k]) |> filter(layer == 'surf')
  df.plots = list()
  
  for (j in 1:length(lakes_order)) {
    fit.list = list()
    useyears = unique(nuts.vars |> filter(lakeid == lakes_order[j]) |> pull(year))
    
    if(length(useyears) == 0) {next}
    for (i in 1:length(useyears)) {
      df = nuts.vars |> filter(lakeid == lakes_order[j], year == useyears[i]) |> 
        filter(yday(sampledate) <= 274)
      
      if (nrow(df) > 0 & sum(df$value > 0)) {
        fit.list[[i]] = fitWeibull(df, find = 'min')
      } 
    }
    
    fit.df = do.call(rbind.data.frame, fit.list)
    
    df.plots[[j]] = ggplot(fit.df) + #|> filter(r2 >= 0.7)) +
      geom_path(aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy, group = year), alpha = 0.6, linewidth = 0.2) +
      geom_smooth(aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = '#e88361', linewidth = 2) +
      ylab(vars[k]) +
      labs(title = lakes_order[j]) + 
      theme_bw(base_size = 7) +
      theme(axis.title.x = element_blank(), plot.title = element_text(size = 7))
  }
  
  df.plots = df.plots[lengths(df.plots) != 0]
  
  patchwork::wrap_plots(df.plots)
  ggsave(paste0('Figures_manuscript/Weibull_Fits_Nutrients/',vars[k],'_Springmin.png'), width = 6, height = 9, dpi = 500)
}


