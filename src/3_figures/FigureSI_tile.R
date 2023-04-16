
figureSI_tile <- function(path_in, path_out) {
  dat = read_csv(path_in) 

  vars_order = c("iceoff", "straton", "stability", "energy", "schmidt", "stratoff", "iceon",
                 
                 "drsif_surfMin", "drsif_springSurfMin",
                 "totnuf_surfMin", 
                 "totnf_surfMin",
                 'nh4_surfMin', 
                 'no3no2_surfMin', 
                 
                 'totpf_surfMin',
                 "totpuf_surfMin", 
                 'drp_surfMin',
                 'doc_surfMin','doc_surfMax', 
                 "ph_surfMin","ph_surfMax",
                 
                 "minimum_oxygen", "secchi_max", "secchi_min", "zoop_max")
  
  vars_labels = c("Ice off", "Strat onset", "Stability", "Energy", "Schmidt", 'Strat offset','Ice on',
                  
                  "drsif_surfMin", "drsif_springSurfMin",
                  "totnuf_surfMin", 
                  "totnf_surfMin",
                  'nh4_surfMin', 
                  'no3no2_surfMin', 
                  
                  'totpf_surfMin',
                  "totpuf_surfMin", 
                  'drp_surfMin',
                  'doc_surfMin','doc_surfMax', 
                  "ph_surfMin","ph_surfMax",
                  
                  'Oxygen min',  'Secchi max', 'Secchi min', 'Zoop max')
  
  
  lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")
  
  dat.summary = dat |> 
    filter(lakeid != 'FI') |> 
    filter(!is.na(dayWeibull)) |> #filter out actual NAs (not enough data, not to be confused with -999)
    group_by(lakeid, metric) |> 
    summarise(n = n(), real = sum(weibull.max == TRUE), per = real/n, r2.median = median(weibull.r2, na.rm = T)) |> 
    filter(n > 10) |> 
    mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
    mutate(metric = factor(metric, levels = rev(vars_order), labels = rev(vars_labels))) |> 
    filter(!is.na(metric)) |> 
    # mutate(per = real/n) |> 
    mutate(fit = case_when(r2.median >= 0.9 ~ '0.9 - 1.0',
                           r2.median >= 0.8 ~ '0.8 - 0.9',
                           r2.median >= 0.7 ~ '0.7 - 0.8',
                           r2.median < 0.7 ~ '< 0.7')) |> 
    mutate(fit = factor(fit, levels = c('< 0.7', '0.7 - 0.8', '0.8 - 0.9', '0.9 - 1.0'))) |> 
    mutate(perCase = case_when(per >= 0.9 ~ '0.9 - 1.0',
                               per >= 0.8 ~ '0.8 - 0.9',
                               per >= 0.7 ~ '0.7 - 0.8',
                               per < 0.7 ~ '< 0.7')) |> 
    mutate(perCase = factor(perCase, levels = c('< 0.7','0.7 - 0.8','0.8 - 0.9', '0.9 - 1.0')))
  
  
  makeTile <- function(usevars) {
    ggplot(dat.summary |> filter(metric %in% usevars)) +
      geom_tile(aes(x = lakeid, y = metric, fill = fit), alpha = 0.8, color = NA) +
      geom_tile(data = dat.summary |> filter(per >= 0.7, metric %in% usevars), aes(x = lakeid, y = metric), 
                fill = NA, color = 'black', linewidth = 0.5) +
      geom_text(aes(x = lakeid, y = metric, label = round(r2.median,2)), size = 2.2) +
      scale_fill_manual(values = c('#f5e3e1','#f0a689','#e3c54f','#63ab7f'), 
                        drop = F, na.translate = F, name = 'Median r<sup>2</sup>') +
      theme_minimal(base_size = 9) +
      theme(panel.grid.major = element_blank(), 
            axis.title.x = element_blank(), 
            legend.title = element_markdown())
  }
  
  usevars = c("Ice off", "Strat onset", "Energy", "Schmidt", 'Strat offset','Ice on')
  p1 = makeTile(usevars); p1
  
  usevars = c("drsif_surfMin", "drsif_springSurfMin",
         "totnuf_surfMin", 
         "totnf_surfMin",
         'nh4_surfMin', 
         'no3no2_surfMin', 
         
         'totpf_surfMin',
         "totpuf_surfMin", 
         'drp_surfMin',
         
         'doc_surfMin','doc_surfMax', 
         "ph_surfMin","ph_surfMax")
  p2 = makeTile(usevars)
  
  usevars = c('Oxygen min',  'Secchi max', 'Secchi min', 'Zoop max')
  p3 = makeTile(usevars)
  
  ################################ Join ################################
  p1/p2/p3 + plot_layout(heights = c(6, 12, 4), guides = 'collect')
  
  ggsave(filename = path_out, width = 6, 
         height = 6, dpi = 500)        
  
}

figureSI_tile2 <- function(path_in, path_out) {
  dat = read_csv(path_in) 
  
  vars_order = c("iceoff", "straton", "stability", "energy", "schmidt", "stratoff", "iceon",
                 "drsif_surfMin", 
                 "drsif_springSurfMin",  
                 "totnuf_surfMin",
                 "totpuf_surfMin", 
                 "minimum_oxygen", "secchi_max", "zoop_max")
  
  vars_labels = c("Ice off", "Strat onset", "Stability", "Energy", "Schmidt", 'Strat offset','Ice on',
                  "Silica min",  'Si spring min',
                  "TN min",
                  "TP min", 
                  'Oxygen min',  'Secchi max', 'Zoop max')
  
  
  lakes_order = c("AL", "BM", "CR", "SP","TR", "CB", "TB", "ME", "MO", "WI")
  
  dat.summary = dat |> 
    filter(lakeid != 'FI') |> 
    filter(!is.na(dayWeibull)) |> #filter out actual NAs (not enough data, not to be confused with -999)
    group_by(lakeid, metric) |> 
    summarise(n = n(), real = sum(weibull.max == TRUE), per = real/n, r2.median = median(weibull.r2, na.rm = T)) |> 
    filter(n > 10) |> 
    mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
    mutate(metric = factor(metric, levels = rev(vars_order), labels = rev(vars_labels))) |> 
    filter(!is.na(metric)) |> 
    # mutate(per = real/n) |> 
    mutate(fit = case_when(r2.median >= 0.9 ~ '0.9 - 1.0',
                           r2.median >= 0.8 ~ '0.8 - 0.9',
                           r2.median >= 0.7 ~ '0.7 - 0.8',
                           r2.median < 0.7 ~ '< 0.7')) |> 
    mutate(fit = factor(fit, levels = c('< 0.7', '0.7 - 0.8', '0.8 - 0.9', '0.9 - 1.0'))) |> 
    mutate(perCase = case_when(per >= 0.9 ~ '0.9 - 1.0',
                               per >= 0.8 ~ '0.8 - 0.9',
                               per >= 0.7 ~ '0.7 - 0.8',
                               per < 0.7 ~ '< 0.7')) |> 
    mutate(perCase = factor(perCase, levels = c('< 0.7','0.7 - 0.8','0.8 - 0.9', '0.9 - 1.0')))
  
  
  makeTile <- function(usevars) {
    ggplot(dat.summary |> filter(metric %in% usevars)) +
      geom_tile(aes(x = lakeid, y = metric, fill = fit), alpha = 0.8, color = NA) +
      geom_tile(data = dat.summary |> filter(per >= 0.7, metric %in% usevars), aes(x = lakeid, y = metric), 
                fill = NA, color = 'black', linewidth = 0.5) +
      geom_text(aes(x = lakeid, y = metric, label = round(r2.median,2)), size = 2.2) +
      scale_fill_manual(values = c('#f5e3e1','#f0a689','#e3c54f','#63ab7f'), 
                        drop = F, na.translate = F, name = 'Median r<sup>2</sup>') +
      theme_minimal(base_size = 9) +
      theme(panel.grid.major = element_blank(), 
            axis.title.x = element_blank(), 
            legend.title = element_markdown())
  }
  
  usevars = c("Strat onset", "Energy", "Schmidt", 'Strat offset')
  p1 = makeTile(usevars); p1
  
  usevars = c('Si spring min',
              "TN min",
              "TP min")
  p2 = makeTile(usevars)
  
  usevars = c('Oxygen min', 'Secchi max', 'Zoop max')
  p3 = makeTile(usevars)
  
  ################################ Join ################################
  p1/p2/p3 + plot_layout(heights = c(4, 3, 3), guides = 'collect')
  
  ggsave(filename = path_out, width = 6, height = 3, dpi = 500)        
  
}