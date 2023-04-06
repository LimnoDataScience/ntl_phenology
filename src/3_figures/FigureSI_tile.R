
figureSI_tile <- function(path_in, path_out) {
  dat = read_csv(path_in)
  vars_order = c("iceoff", "straton", "stability", "energy", "schmidt", "stratoff", "iceon",
                 "drsif_surfMin",  "drsif_surfMax", 
                 
                 "totnuf_surfMin", 'totnuf_surfMax', 
                 'nh4_surfMin', 'nh4_surfMax',
                 'no3no2_surfMin', 'no3no2_surfMax',
                 
                 "totpuf_surfMin", 'totpuf_surfMax', 
                 'drp_surfMin','drp_surfMax',
                 'doc_surfMin','doc_surfMax', 
                 "minimum_oxygen", "secchi_max", "secchi_min", "zoop_max",
                 "drsif_springSurfMin","zoop_springmax","secchi_springmax")
  
  vars_labels = c("Ice off", "Strat onset", "Stability", "Energy", "Schmidt", 'Strat offset','Ice on',
                  "drsif_surfMin",  "drsif_surfMax", 
                  
                  "totnuf_surfMin", 'totnuf_surfMax', 
                  'nh4_surfMin', 'nh4_surfMax',
                  'no3no2_surfMin', 'no3no2_surfMax',
                  
                  "totpuf_surfMin", 'totpuf_surfMax', 
                  'drp_surfMin','drp_surfMax',
                  'doc_surfMin','doc_surfMax', 
                  'Oxygen min',  'Secchi max', 'Secchi min', 'Zoop max', 
                  'Si spring min', 'Zoop spring max', 'Secchi spring max')
  
  
  lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")
  
  test = dat |> 
    filter(lakeid != 'FI') |> 
    # group_by(lakeid, metric) |> 
    # filter(sum(!is.na(dayWeibull)) > 5) |> # we need some data!
    # ungroup() |> 
    filter(!is.na(dayWeibull)) |> #filter out actual NAs (not enough data, not to be confused with -999)
    mutate(dayWeibull = if_else(dayWeibull == -999, NA_real_, dayWeibull)) |> 
    mutate(weibull.r2 = if_else(weibull.max == FALSE, NA_real_, weibull.r2)) |> # filter out dates when peak is greater than beginning and end
    group_by(lakeid, metric) |> 
    summarise(n = n(), real = sum(weibull.max == TRUE), per = real/n, r2.mean = mean(weibull.r2, na.rm = T)) |> 
    mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
    mutate(metric = factor(metric, levels = rev(vars_order), labels = rev(vars_labels))) |> 
    filter(!is.na(metric)) |> 
    # mutate(per = real/n) |> 
    mutate(fit = case_when(r2.mean >= 0.9 ~ '0.9 - 1.0',
                           r2.mean >= 0.8 ~ '0.8 - 0.9',
                           r2.mean < 0.8 ~ '< 0.8')) |> 
    mutate(fit = factor(fit, levels = c('< 0.8', '0.8 - 0.9', '0.9 - 1.0'))) |> 
    mutate(perCase = case_when(per >= 0.9 ~ '0.9 - 1.0',
                               per >= 0.8 ~ '0.8 - 0.9',
                               per < 0.8 ~ '< 0.8')) |> 
    mutate(perCase = factor(perCase, levels = c('< 0.8', '0.8 - 0.9', '0.9 - 1.0')))
  
  
  makeTile <- function(usevars) {
    ggplot(test |> filter(metric %in% usevars)) +
      geom_tile(aes(x = lakeid, y = metric, fill = fit), alpha = 0.8, color = NA) +
      geom_tile(data = test |> filter(per >= 0.8, metric %in% usevars), aes(x = lakeid, y = metric), 
                fill = NA, color = 'black', size = 1) +
      geom_text(aes(x = lakeid, y = metric, label = round(r2.mean,2)), size = 2.2) +
      scale_fill_manual(values = c('#f0a689','#e3c54f','#63ab7f'), 
                        drop = F, na.translate = F, name = 'Mean r<sup>2</sup>') +
      theme_minimal(base_size = 9) +
      theme(panel.grid.major = element_blank(), 
            axis.title.x = element_blank(), 
            legend.title = element_markdown())
  }
  
  usevars = c("Ice off", "Strat onset", "Stability", "Energy", "Schmidt", 'Strat offset','Ice on')
  p1 = makeTile(usevars); p1
  
  usevars = c("drsif_surfMin",  "drsif_surfMax", 
              
              "totnuf_surfMin", 'totnuf_surfMax', 
              'nh4_surfMin', 'nh4_surfMax',
              'no3no2_surfMin', 'no3no2_surfMax',
              
              "totpuf_surfMin", 'totpuf_surfMax', 
              'drp_surfMin','drp_surfMax',
              'doc_surfMin','doc_surfMax')
  p2 = makeTile(usevars)
  
  usevars = c('Oxygen min',  'Secchi max', 'Secchi min', 'Zoop max', 
              'Secchi spring max', 'Zoop spring max', 'Si spring min')
  p3 = makeTile(usevars)
  
  ################################ Join ################################
  p1/p2/p3 + plot_layout(heights = c(5, 14, 7), guides = 'collect')
  
  ggsave(filename = path_out, width = 6, 
         height = 6, dpi = 500)        
  
}

figureSI_tile2 <- function(path_in, path_out) {
  dat = read_csv(path_in)
  vars_order = c("iceoff", "straton", "stability", "energy", "schmidt", "stratoff", "iceon",
                 "drsif_surfMin",  
                 "totnuf_surfMin",
                 "totpuf_surfMin", 
                 "minimum_oxygen", "secchi_max", "secchi_min", "zoop_max",
                 "drsif_springSurfMin","zoop_springmax","secchi_springmax")
  
  vars_labels = c("Ice off", "Strat onset", "Stability", "Energy", "Schmidt", 'Strat offset','Ice on',
                  "Silica min",  
                  "TN min",
                  "TP min", 
                  'Oxygen min',  'Secchi max', 'Secchi min', 'Zoop max', 
                  'Si spring min', 'Zoop spring max', 'Secchi spring max')
  
  
  lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")
  
  test = dat |> 
    filter(lakeid != 'FI') |> 
    # group_by(lakeid, metric) |> 
    # filter(sum(!is.na(dayWeibull)) > 5) |> # we need some data!
    # ungroup() |> 
    filter(!is.na(dayWeibull)) |> #filter out actual NAs (not enough data, not to be confused with -999)
    mutate(dayWeibull = if_else(dayWeibull == -999, NA_real_, dayWeibull)) |> 
    mutate(weibull.r2 = if_else(weibull.max == FALSE, NA_real_, weibull.r2)) |> # filter out dates when peak is greater than beginning and end
    group_by(lakeid, metric) |> 
    summarise(n = n(), real = sum(weibull.max == TRUE), per = real/n, r2.mean = mean(weibull.r2, na.rm = T)) |> 
    mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
    mutate(metric = factor(metric, levels = rev(vars_order), labels = rev(vars_labels))) |> 
    filter(!is.na(metric)) |> 
    # mutate(per = real/n) |> 
    mutate(fit = case_when(r2.mean >= 0.9 ~ '0.9 - 1.0',
                           r2.mean >= 0.8 ~ '0.8 - 0.9',
                           r2.mean < 0.8 ~ '< 0.8')) |> 
    mutate(fit = factor(fit, levels = c('< 0.8', '0.8 - 0.9', '0.9 - 1.0'))) |> 
    mutate(perCase = case_when(per >= 0.9 ~ '0.9 - 1.0',
                               per >= 0.8 ~ '0.8 - 0.9',
                               per < 0.8 ~ '< 0.8')) |> 
    mutate(perCase = factor(perCase, levels = c('< 0.8', '0.8 - 0.9', '0.9 - 1.0')))
  
  
  makeTile <- function(usevars) {
    ggplot(test |> filter(metric %in% usevars)) +
      geom_tile(aes(x = lakeid, y = metric, fill = fit), alpha = 0.8, color = NA) +
      geom_tile(data = test |> filter(per >= 0.8, metric %in% usevars), aes(x = lakeid, y = metric), 
                fill = NA, color = 'black', size = 1) +
      geom_text(aes(x = lakeid, y = metric, label = round(r2.mean,2)), size = 2.2) +
      scale_fill_manual(values = c('#f0a689','#e3c54f','#63ab7f'), 
                        drop = F, na.translate = F, name = 'Mean r<sup>2</sup>') +
      theme_minimal(base_size = 9) +
      theme(panel.grid.major = element_blank(), 
            axis.title.x = element_blank(), 
            legend.title = element_markdown())
  }
  
  usevars = c("Ice off", "Strat onset", "Stability", "Energy", "Schmidt", 'Strat offset','Ice on')
  p1 = makeTile(usevars); p1
  
  usevars = c("Silica min",  
              "TN min",
              "TP min")
  p2 = makeTile(usevars)
  
  usevars = c('Oxygen min', 'Secchi max', 'Zoop max')
  p3 = makeTile(usevars)
  
  ################################ Join ################################
  p1/p2/p3 + plot_layout(heights = c(5, 3, 3), guides = 'collect')
  
  ggsave(filename = path_out, width = 6, height = 6, dpi = 500)        
  
}