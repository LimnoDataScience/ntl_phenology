library(tidyverse)
library(cardidates)
library(lubridate)
library(patchwork)
library(scales)
source('src/Functions/weibullCDW.R')

### Figure 1 ###

figure1 <- function(path_out) {
  
  #################### LOAD SECCHI DATA ####################
  lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")
  
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
    filter(!n < 8) |> # filter out low year
    ungroup() |> select(-n) |> 
    rename(year = year4)
  
  
  plotWeibull <- function(df, r2size = 3) {
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
    
    ## is smd Begin > smd End?
    weibull.max = smd$y[1] > smd$y[2] & smd$y[1] > smd$y[3] 
    
    print(secchi.max$daynum - smd.out$x[1])
    
    if(weibull.max == FALSE){
      df2 = df[1,] |> bind_rows(df)
      df2$daynum[1] = df2$daynum[1] - 30
      df2$secnview[1] = min(df2$secnview)
      
      res2 <- fitweibull6(df2$daynum, df2$secnview)
      fit2 <- res2$fit |> 
        mutate(newy = fweibull6(x, res2$p) * res2$ymax)
      # r2
      res2$r2
      
      ## identify cardinal dates from fitted curves
      smd2 <- weibullCDW(res2, quantile=0.05)
      smd2.out = data.frame(x = smd2[[1]], y = smd2[[2]]* res2$ymax)
      
      print(secchi.max$daynum - smd2.out$x[1])
    }
    
    # Plotting
    p1 = ggplot(df) +
      geom_vline(data = secchi.max, aes(xintercept = as.Date(daynum, origin = as.Date('2020-01-01'))), linetype = 2) +
      geom_point(aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = secnview)) +
      geom_path(data = fit, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'lightblue3') +
      geom_point(data = smd.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
                 fill = 'lightblue3', shape = 23, stroke = 0.2, size = c(4,2,2)) +
      annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-03-01')), y = Inf, 
               label = paste0('r^2 == ', round(res$r2, 2)), hjust = 0, vjust = 1.5, parse = TRUE, color = 'lightblue4', size = r2size) +
      ylab('Secchi (m)') + 
      scale_y_continuous(expand = expansion(0.1)) +
      scale_x_date(labels = date_format("%b"), breaks = 'month', 
                   limits = c(as.Date('2020-03-01'), as.Date('2020-10-15'))) +
      theme_bw(base_size = 9) +
      theme(axis.title.x = element_blank(), 
            plot.title = element_text(size = 9))
    
    if(weibull.max == FALSE){
      p1 = p1 + 
        geom_point(data = df2[1,], aes(x = as.Date(daynum, origin = as.Date('2020-01-01')), y = secnview), color = 'salmon1', shape = 15) +
        geom_path(data = fit2, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = newy), color = 'salmon1') +
        geom_point(data = smd2.out, aes(x = as.Date(x, origin = as.Date('2020-01-01')), y = y), 
                   fill = 'salmon1', shape = 23, stroke = 0.2, size = c(4,2,2)) +
        annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-03-01')), y = Inf, 
                 label = paste0('r^2 == ', round(res2$r2, 2)), hjust = 0, vjust = 3, parse = TRUE, color = 'salmon1', size = r2size)
    }
    
    return(p1)
    
  }
  
  # 
  # plotWeibull(secchi |> filter(lakeid == 'CB', year == 1998)) + labs(title = 'Crystal Lake, 1984')
  # plotWeibull(secchi |> filter(lakeid == 'BM', year == 1986))
  # 
  # 
  # plotWeibull(secchi |> filter(lakeid == 'BM', year == 1998))
  # df2 = secchi |> filter(lakeid == 'BM', year == 2003) 
  # df2 = df2[1,] |> bind_rows(df2)
  # df2$daynum[1] = df2$daynum[1] - 30
  # df2$secnview[1] = min(df2$secnview)
  # plotWeibull(df2)
  # 
  # plotWeibull(secchi |> filter(lakeid == 'BM', year == 1999))
  # plotWeibull(secchi |> filter(lakeid == 'BM', year == 2003))
  # plotWeibull(secchi |> filter(lakeid == 'BM', year == 2011))
  # plotWeibull(secchi |> filter(lakeid == 'BM', year == 2015))
  # plotWeibull(secchi |> filter(lakeid == 'BM', year == 2018))
  # 
  
  ## Example figures for manuscript 
  p1 = plotWeibull(secchi |> filter(lakeid == 'ME', year == 2012)) + 
    annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-06-20')), y = Inf, 
             label = 'Lake Mendota, 2012', hjust = 0, vjust = 2, color = 'lightblue4', size = 3) 
  
  # p2 = plotWeibull(secchi |> filter(lakeid == 'TB', year == 2019)) + #labs(title = 'Trout Bog, 2019')
  #   annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-08-20')), y = 0.3, 
  #            label = 'Trout Bog, 2019', hjust = 0, vjust = 0, color = 'lightblue4', size = 3) 
  
  p3 = plotWeibull(secchi |> filter(lakeid == 'CR', year == 1984)) + 
    annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-07-04')), y = Inf,
             label = 'Crystal Lake, 1984', hjust = 0, vjust = 2, color = 'lightblue4', size = 3)
  
  p4 = plotWeibull(secchi |> filter(lakeid == 'BM', year == 1986)) + 
    annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-06-10')), y = Inf, 
             label = 'Big Muskellunge, 1986', hjust = 0, vjust = 2, color = 'lightblue4', size = 3) 
  
  p5 = plotWeibull(secchi |> filter(lakeid == 'CB', year == 2006)) + 
    annotate(geom = 'text', x = as.Date(5, origin = as.Date('2020-06-26')), y = 0.8, 
             label = 'Crystal Bog, 2006', hjust = 0, vjust = 2, color = 'lightblue4', size = 3)
  
  # patchwork
  p1/p3/p4/p5 + plot_layout(ncol = 2) +
    plot_annotation(tag_levels = 'a', tag_suffix = ')') 
  
  #                   caption = 'Figure X. Weibull distributions (blue line) fit to observed Secchi 
  # depths in three lake-years. The blue dots represent the start, maximum, and end 
  # points of a theoretical clear-water phase. r2 values are given for each Weibull fit.
  # Dashed vertical lines indicate the day of year of maximum observed secchi depth.',
  #                   theme = theme(plot.caption = element_text(hjust = 0))) 
  
  ggsave(path_out, width = 6, height = 4, dpi = 500)
  
}
