library(tidyverse)
library(cardidates)
library(lubridate)
library(patchwork)
library(scales)
source('src/Functions/weibullCDW.R')

#################### LOAD ZOOP DATA ####################
lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")

### Get ZOOP ###
ice_file = "Data/final_metric_files/ice.csv"
strat_file = 'Data/final_metric_files/physics.csv'

#### Download southern lake zooplankton data from EDI ####
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/90/33/5880c7ba184589e239aec9c55f9d313b"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "curl")
dt1 <- read_csv(infile1)

#### Download northern lake zooplankton data from EDI ####
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")
dt2 <-read_csv(infile1)

# get ice on/off dates
iceOff = read_csv(ice_file) |> 
  filter(metric == 'iceoff') |> 
  select(lakeid, year4 = year, lastice = sampledate)

stratOff = read_csv(strat_file) |> 
  filter(metric == 'stratoff') |> 
  select(lakeid, year4 = year, stratoff = sampledate)

#Combine files
zoops = dt1 |> dplyr::select(-towdepth) |> 
  bind_rows(dt2) |> 
  left_join(iceOff) |> 
  filter(sample_date > lastice) |> # filter dates after ice off
  left_join(stratOff) |> 
  filter(sample_date < stratoff) |>  # filter dates before fall mixing
  mutate(code = floor(species_code/10000), daynum = yday(sample_date)) |>
  mutate(zoopGroup = case_when(code == 1 ~ 'copepod nauplii',
                               code == 2 ~ 'copepod',
                               code == 3 ~ 'calanoid',
                               code == 4 ~ 'harpacticoid',
                               code == 5 ~ 'cladocera',
                               code == 6 ~ 'rotifer',
                               code == 7 ~ 'unknown',
                               code == 8 ~ 'unknown',
                               code == 9 ~ 'unknown')) |> 
  filter(code %in% c(2,3,4,5)) |>  # cladocera and copepods
  rename(year = year4, sampledate = sample_date) |> 
  group_by(lakeid, year, sampledate, daynum) |> 
  summarize(density = sum(density, na.rm = T)) |> 
  ungroup()


plotWeibull <- function(df, r2size = 3) {
  # fit weibull, estimate new fit for plotting
  res <- fitweibull6(df$daynum, df$density)
  fit <- res$fit |> 
    mutate(newy = fweibull6(x, res$p) * res$ymax)
  # r2
  res$r2
  
  # identify maximum
  zoops.max = df |> filter(density == max(density)) |> slice(1)
  
  ## identify cardinal dates from fitted curves
  smd <- weibullCDW(res, quantile=0.05)
  smd.out = data.frame(x = smd[[1]], y = smd[[2]]* res$ymax)
  
  ## is smd Begin > smd End?
  weibull.max = smd$y[1] > smd$y[2] & smd$y[1] > smd$y[3] 
  
  if(weibull.max == FALSE){
    df2 = df[1,] |> bind_rows(df)
    df2$daynum[1] = df2$daynum[1] - 30
    df2$density[1] = min(df2$density)
    
    res2 <- fitweibull6(df2$daynum, df2$density)
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
    geom_vline(data = zoops.max, aes(xintercept = as.Date(daynum, origin = as.Date('2020-01-01'))), linetype = 2, linewidth = 0.4) +
    geom_point(aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = density), size = 0.8) +
    geom_path(data = fit, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'lightblue3') +
    geom_point(data = smd.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
               fill = 'lightblue3', shape = 21, stroke = 0.2, size = 2) +
    annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-01-01')), y = Inf, 
             label = paste0('r^2 == ', round(res$r2, 2)), hjust = 0, vjust = 2, parse = TRUE, color = 'lightblue4', size = r2size) +
    ylab('Zoop Density (#/L)') + 
    
    scale_x_date(labels = date_format("%b"), breaks = '4 month', minor_breaks = '1 month') +
    theme_bw(base_size = 7) +
    theme(axis.title.x = element_blank(),plot.title = element_text(size = 7))
  
  if(weibull.max == FALSE){
    p1 = p1 + 
      geom_point(data = df2[1,], aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = density), size = 0.8, color = 'salmon1') +
      geom_path(data = fit2, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'salmon1') +
      geom_point(data = smd2.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
                 fill = 'salmon1', shape = 21, stroke = 0.2, size = 2) +
      annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-01-01')), y = Inf, 
               label = paste0('r^2 == ', round(res2$r2, 2)), hjust = 0, vjust = 3, parse = TRUE, color = 'salmon1', size = r2size)
  }
  
  return(p1)
  
}

############ ALL lakes Zoops ##############
for (j in 1:length(lakes_order)) {
  df.plots = list()
  useyears = unique(zoops |> filter(lakeid == lakes_order[j]) |> pull(year))
  for (i in 1:length(useyears)) {
    df = zoops |> filter(lakeid == lakes_order[j], year == useyears[i])
    
    if (nrow(df) > 5) {
      df.plots[[i]] = plotWeibull(zoops |> filter(lakeid == lakes_order[j], year == useyears[i]), r2size = 2) + 
        labs(title = paste0(lakes_order[j],': ',useyears[i])) 
    } else {
      df.plots[[i]] = ggplot() + theme_void()
    }
  }
  patchwork::wrap_plots(df.plots)
  ggsave(paste0('Figures_manuscript/Weibull_Zoops/','Zoop_max_',lakes_order[j],'.png'), width = 6, height = 9, dpi = 500)
}
