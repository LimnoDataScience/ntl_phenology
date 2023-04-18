library(tidyverse)
library(broom)
library(wql)

figureSI_MK <- function(path_in, path_out) {
  
  vars_order = c("iceoff", "straton", "energy", "schmidt", "stratoff", "iceon",
                 "drsif_springSurfMin", 
                 "totnuf_surfMin",
                 "totpuf_surfMin", 
                 "ph_surfMax",
                 "minimum_oxygen", "secchi_max", "zoop_max")
  
  vars_labels = c("Ice-off", "Strat onset", "Energy", "Schmidt", 'Strat offset','Ice-on',
                  "Silica min",  
                  "TN min",
                  "TP min", 
                  "pH max",
                  'Oxygen min',  'Secchi max', 'Zoop max')
  
  dat = read_csv(path_in) |> filter(lakeid != 'FI') |> 
    mutate(diffDays = abs(daynum - dayWeibull)) |> 
    # mutate(weibull.r2 = if_else(weibull.max == FALSE, NA_real_, weibull.r2)) |> # filter out dates when peak is greater than beginning and end
    filter(weibull.r2 > 0.7 | diffDays <= 30)
  
  # simple linear trend with Weibull day 
  lm_slopes = dat %>% 
    filter(!is.na(daynum)) %>% 
    group_by(lakeid, metric) %>% 
    do(tidy(lm(daynum ~ year, data = .))) %>% 
    filter(term == "year" & metric %in% vars_order) 
  
  lm_slopes %>% 
    mutate(estimate = ifelse(p.value < 0.05, estimate, NA)) %>% 
    mutate(lakeid = factor(lakeid, 
                           levels = c("AL", "BM", "CB", "CR", "SP", "SR", "TB", "TR", "ME", "MO", "WI"),
                           ordered = T)) %>% 
    mutate(metric = factor(metric, levels = vars_order, ordered=T)) %>% 
    ggplot(aes(x=metric, y=lakeid, fill=estimate)) +
    geom_tile(color="transparent") + 
    scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                                        '#d1e5f0','#67a9cf','#2166ac'),
                                        n.breaks=9, na.value = "transparent") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme(axis.title.x = element_blank())
    # labs(x="", title = "lm() sig. trends (p < 0.05)", fill="lm slope\n days/year")
  
  # try with Mann-Kendall
  mannken_sen_slope = dat %>% 
    dplyr::select(lakeid, metric) %>% 
    distinct() %>% 
    filter(metric %in% vars_order) %>% 
    mutate(sen.slope.Weibull = NA, p.value.Weibull=NA,
           sen.slope.daynum = NA, p.value.daynum=NA)
  
  for(i in 1:nrow(mannken_sen_slope)){
    cur_dat = dat %>% 
      filter(lakeid == mannken_sen_slope[[i, "lakeid"]] & metric == mannken_sen_slope[[i, "metric"]]) %>% 
      arrange(year) %>% 
      pull(dayWeibull)
    hold = mannKen(cur_dat)
    mannken_sen_slope[[i, "sen.slope.Weibull"]] = hold[["sen.slope"]]  
    mannken_sen_slope[[i, "p.value.Weibull"]] = hold[["p.value"]]  
    
    cur_dat = dat %>% 
      filter(lakeid == mannken_sen_slope[[i, "lakeid"]] & metric == mannken_sen_slope[[i, "metric"]]) %>% 
      arrange(year) %>% 
      pull(daynum)
    hold = mannKen(cur_dat)
    mannken_sen_slope[[i, "sen.slope.daynum"]] = hold[["sen.slope"]]  
    mannken_sen_slope[[i, "p.value.daynum"]] = hold[["p.value"]]  
  }
  
  mkss_sig = mannken_sen_slope %>% 
    mutate(sen.slope.Weibull = ifelse(p.value.Weibull < 0.05, sen.slope.Weibull, NA)) |> 
    mutate(sen.slope.daynum = ifelse(p.value.daynum < 0.05, sen.slope.daynum, NA))
  
  lmt = max(abs(mkss_sig$sen.slope.Weibull), na.rm=T)
  
  # # Plot sig. trends

  mkss_sig %>% 
    mutate(lakeid = factor(lakeid, 
                           levels = rev(c("AL", "BM", "CB", "CR", "SP", "SR", "TB", "TR", "ME", "MO", "WI")),
                           ordered = T)) %>% 
    mutate(metric = factor(metric, levels = vars_order, ordered=T)) %>% 
    ggplot(aes(x=metric, y=lakeid, fill=sen.slope.Weibull)) +
    geom_tile(color="black") + 
    scale_fill_stepsn(colors=c('#b2182b','#ef8a62','#fddbc7',
                                        '#d1e5f0','#67a9cf','#2166ac'),
                                        n.breaks=9, limits=c(-lmt,lmt), na.value = "grey98") +
    geom_point(data = . %>% filter(!is.na(sen.slope.daynum)), aes(x=metric, y=lakeid, fill=sen.slope.daynum), shape = 21, size = 2, stroke = 0.2) +
    geom_hline(yintercept = 3.5) +
    geom_vline(xintercept = 6.5) +
    geom_vline(xintercept = 9.5) +
    scale_x_discrete(expand = c(0,0), labels = vars_labels) +
    scale_y_discrete(expand = c(0,0)) +
    theme_bw(base_size = 8) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(x="", y = 'Lake', title = "Mann-Kendall sig. trends (p < 0.05)", fill="Sen Slope\n days/year")
  
  ggsave(filename = path_out, width = 6, height = 3, dpi = 500)
  
  # plot the significant timeseries
  tssig = mkss_sig %>% 
    filter(p.value.Weibull < 0.05 | p.value.daynum < 0.05) %>%
    left_join(dat) %>% 
    mutate( sen_slope = paste("Sen Slope =", round(sen.slope.Weibull, 1), "d/yr"),
            metric = factor(metric, levels=vars_order, labels = vars_labels),
            lake_metric = paste(lakeid, metric, sep=" : "))
  
  ggplot(tssig) +
    geom_point(data = tssig |> filter(!is.na(sen.slope.Weibull)), aes(x=year, y=dayWeibull)) +
    geom_point(data = tssig |> filter(!is.na(sen.slope.daynum)), aes(x=year, y=daynum), fill = 'red3', stroke = 0.2, shape = 21) +
    theme_bw() +
    facet_wrap(~lake_metric+sen_slope, scale="free_y") +
    geom_smooth(data = tssig |> filter(!is.na(sen.slope.Weibull)), aes(x=year, y=dayWeibull), method = 'lm', color = 'black') +
    geom_smooth(data = tssig |> filter(!is.na(sen.slope.daynum)), aes(x=year, y=dayWeibull), method = 'lm', color = 'red3')
  
 
}


