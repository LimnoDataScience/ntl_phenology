library(tidyverse)
library(cardidates)
library(lubridate)
library(patchwork)
library(scales)
source('src/Functions/weibullCDW.R')

#################### LOAD Nutrient DATA ####################
lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")

### Get Secchi ###
ice_file = "Data/final_metric_files/ice.csv"
strat_file = 'Data/final_metric_files/physics.csv'

#################### FUNCTIONS ####################
# filtering function - turns outliers into NAs to be removed
filter_lims <- function(x){
  # l = quantile(x, probs = 0.001)
  # u = quantile(x, probs = 0.999)
  stdx = sd(x)
  
  u = mean(x, na.rm = T) + 3*stdx
  l = mean(x, na.rm = T) - 3*stdx
  for (i in 1:length(x)){
    x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
  }
  return(x)
}

#################### LOAD DATA ####################
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/57/802d63a4c35050b09ef6d1e7da3efd3f"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "curl")

# There is an offset between state lab of hygiene (SLOH) drsif and NLT drsif. 
# A linear model of sloh ~ ntl has a slope of 2.28 with r2 of 0.94. 
LTERnutrients <- read_csv(infile1) |> 
  mutate(drsif = 2.28 * drsif)

# removed flagged data
lternuts.flagged = LTERnutrients %>%
  mutate(across(everything(), ~replace(., .<0 , NA))) %>%
  rename_all( ~ str_replace(., "_sloh", '.sloh')) %>%
  rename_all( ~ str_replace(., "_n", '.n')) %>%
  rename_at(vars(ph:drsif.sloh), ~ str_c("value_",.)) %>%
  rename_at(vars(flagdepth:flagdrsif.sloh), ~ str_c("error_",.)) %>%
  rename_all(~str_replace_all(.,"flag","")) %>%
  pivot_longer(-(lakeid:event), names_to = c('.value','item'), names_sep = '_') %>%
  filter(!is.na(value) & value>= 0) %>%
  # filter(!str_detect(error,'A|K|L|H|Q') | is.na(error)) %>%
  filter(!str_detect(error,'A|K|H') | is.na(error)) %>% #Removed L and Q
  dplyr::select(-error) %>% 
  mutate(value = case_when(str_detect(item, ".sloh") ~ value*1000, #change sloh from mg to µg
                           TRUE ~ value)) %>% 
  mutate(item = case_when(str_detect(item, ".sloh") ~  str_remove(item, ".sloh"),
                          TRUE ~ item))

# Exclude outliers based on statistics. Remove < 5th and > 95th percentile
lternuts.flagged = lternuts.flagged |> 
  group_by(lakeid, item) |> 
  mutate(value = filter_lims(value)) |> 
  ungroup() |> 
  filter(!is.na(value))

# get ice on/off dates
ice0 = read_csv(ice_file) |> 
  filter(metric == 'iceoff') |> 
  dplyr::select(lakeid, year4 = year, lastice = sampledate)

# Which depths to use? 
maxDepths = lternuts.flagged |> 
  group_by(lakeid, depth) %>% tally() %>% 
  filter(if_else(lakeid %in% c('ME',"MO","WI","FI"), n >= 2500, n>= 4000)) %>% 
  group_by(lakeid) |> 
  filter(depth == max(depth)) |> 
  rename(maxDepth = depth)

# Limit to surface or bottom and exclude years with < 8 measurements in that year
surfNuts = lternuts.flagged |> filter(depth <= 1) |> 
  group_by(lakeid, year4, daynum, sampledate, item) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  mutate(layer = 'surf') |> 
  group_by(lakeid, year4, item) |> 
  filter(n() >= 8)

botNuts = lternuts.flagged |> left_join(maxDepths) |> 
  dplyr::filter(depth == maxDepth) |> 
  group_by(lakeid, year4, daynum, sampledate, item) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  mutate(layer = 'bot') |> 
  group_by(lakeid, year4, item) |> 
  filter(n() >= 8)

#################### MANIPULATE DATA ####################
# restrict to surf/bot and stratification period and choose variables of interest
nuts = surfNuts %>% bind_rows(botNuts) |> 
  left_join(ice0) |> 
  filter(sampledate > lastice) |> 
  rename(year = year4) |> 
  filter(item %in% c('drsif', 'no3no2', 'ph', 'doc', 'totnf', 'totnuf', 'topf', 'totpuf', 'drp', 'nh4', 'no3no2'))



plotWeibull <- function(df, find = 'max', r2size = 3) {
  # fit weibull, estimate new fit for plotting
  
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
vars = c('drsif', 'ph', 'doc',  'totnuf', 'totpuf', 'drp', 'nh4', 'no3no2')

for (k in 6:length(vars)) {
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

