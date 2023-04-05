library(tidyverse)
library(cardidates)
library(lubridate)
library(patchwork)
library(scales)
source('src/Functions/weibullCDW.R')
secchi = read_csv('')

#################### LOAD SECCHI DATA ####################
inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/29/5a5a5606737d760b61c43bc59460ccc9"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "libcurl")
LTERsecchi <- read_csv(infile1)

### Get Secchi ###
ice_file = "Data/final_metric_files/ice.csv"
strat_file = 'Data/final_metric_files/physics.csv'

LTERsecchi |> filter(month(sampledate) %in% c(7,8)) |> 
  group_by(lakeid) |> 
  summarise(mean.Secchi = mean(secnview, na.rm = T)) |> 
  mutate(TSI.SD = 60-14.41*log(mean.Secchi))

# get ice on/off dates
iceOff = read_csv(ice_file) |> 
  filter(metric == 'iceoff') |> 
  select(lakeid, year4 = year, lastice = sampledate)

iceOn = read_csv(ice_file) |> 
  filter(metric == 'iceon') |> 
  select(lakeid, year4 = year, firstice = daynum)

stratOff = read_csv(strat_file) |> 
  filter(metric == 'stratoff') |> 
  select(lakeid, year4 = year, stratoff = daynum)


secchi = LTERsecchi |> select(lakeid:sampledate, secnview, ice) |> 
  filter(!is.na(secnview)) |> 
  left_join(iceOff) |> 
  filter(sampledate > lastice) |> # filter dates after ice off
  left_join(stratOff) |> 
  filter(daynum < stratoff) |>  # filter dates before fall mixing
  left_join(iceOn) |> 
  filter(daynum < firstice) |>  # using daynum because ice-on can switch years
  group_by(lakeid, year4) |> 
  mutate(n = n()) |> 
  filter(!n < 10) |> # filter out low year
  ungroup() |> select(-n) |> 
  rename(year = year4)


plotWeibull <- function(df) {
  # fit weibull, estimate new fit for plotting
  res <- fitweibull6(df$daynum, df$secnview)
  fit <- res$fit |> 
    mutate(newy = fweibull6(x, res$p) * res$ymax)
  # r2
  res$r2
  
  # identify maximum
  secchi.max = df |> filter(secnview == max(secnview)) |> slice(1)
  
  ## identify cardinal dates from fitted curves
  smd <- weibullCDW(res, quantile=0.05)
  smd.out = data.frame(x = smd[[1]], y = smd[[2]]* res$ymax)
  
  # Plotting
  p1 = ggplot(df) +
    geom_vline(data = secchi.max, aes(xintercept = as.Date(daynum, origin = as.Date('2020-01-01'))), linetype = 2) +
    geom_point(aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = secnview)) +
    geom_path(data = fit, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'lightblue3') +
    geom_point(data = smd.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
               fill = 'lightblue3', shape = 21, stroke = 0.2, size = 3) +
    annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-01-01')), y = Inf, 
             label = paste0('r^2 == ', round(res$r2, 2)), hjust = 0, vjust = 2, parse = TRUE, color = 'lightblue4', size = 3) +
    ylab('Secchi (m)') + 

    scale_x_date(labels = date_format("%b"), breaks = 'month') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(),plot.title = element_text(size = 9))
  
  return(p1)

}
plotWeibull(secchi |> filter(lakeid == 'ME', year == 2012)) + labs(title = 'Lake Mendota, 2012')

as.Date(189, origin = as.Date('2019-01-01'))

dat |> filter(metric == 'secchi_max') |> arrange(weibull.r2) |> filter(weibull.r2 > 0.5)

p1 = plotWeibull(secchi |> filter(lakeid == 'ME', year == 2012)) + labs(title = 'Lake Mendota, 2012')
p2 = plotWeibull(secchi |> filter(lakeid == 'TB', year == 2019)) + labs(title = 'Trout Bog, 2019')
p3 = plotWeibull(secchi |> filter(lakeid == 'CR', year == 1984)) + labs(title = 'Crystal Lake, 1984')

plotWeibull(nuts |> filter(layer == 'surf', item == 'drsif') |> 
              filter(lakeid == 'TR', year == 1985)) + labs(title = 'Crystal Lake, 1984')

plotWeibull(secchi |> filter(lakeid == 'CB', year == 1998)) + labs(title = 'Crystal Lake, 1984')

# patchwork
p1/p3/p2 + #plot_layout()
  plot_annotation(tag_levels = 'a', tag_suffix = ')', 
                  caption = 'Figure X. Weibull distributions (blue line) fit to observed Secchi 
depths in three lake-years. The blue dots represent the start, maximum, and end 
points of a theoretical clear-water phase. r2 values are given for each Weibull fit.
Dashed vertical lines indicate the day of year of maximum observed secchi depth.',
                  theme = theme(plot.caption = element_text(hjust = 0))) 

ggsave('Figures_manuscript/FigureSI_weibull.png', width = 6, height = 6, dpi = 500)
