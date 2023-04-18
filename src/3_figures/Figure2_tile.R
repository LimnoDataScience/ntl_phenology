
figure2_tile <- function(path_in, path_out, path_out2) {
  dat = read_csv(path_in) 

  vars_order = c("iceoff", "straton", "stability", "energy", "schmidt", "stratoff", "iceon",
                 
                 "drsif_springSurfMin",
                 "totnuf_surfMin", 
                 "totnf_surfMin",
                 'nh4_surfMin', 
                 'no3no2_surfMin', 
                 
                 "totpuf_surfMin",
                 'totpf_surfMin',
                 'drp_surfMin',
                 'doc_surfMin','doc_surfMax', 
                 "ph_surfMin","ph_surfMax",
                 
                 "minimum_oxygen", "secchi_max", "secchi_min", "zoop_max")
  
  vars_labels = c("Ice-off", "Strat onset", "Stability", "Energy", "Schmidt", 'Strat offset','Ice-on',
                  
                  'Silica min',
                  "TN min", 
                  "TN-filtered min",
                  'NH4 min', 
                  'NO3-NO2 min', 
                  
                  'TP min',
                  "TP-filtered min", 
                  'DRP min',
                  'DOC min','DOC max', 
                  "pH min","pH max",
                  
                  'Oxygen min',  'Secchi max', 'Secchi min', 'Zoop max')
  
  
  lakes_order = c("BM", "TR", "CR", "SP", "CB", "TB", "ME", "MO", "AL", "WI")
  
  dat.summary = dat |> 
    filter(lakeid != 'FI') |> 
    mutate(diffDays = abs(daynum - dayWeibull)) |> 
    filter(!is.na(dayWeibull)) |> #filter out actual NAs (not enough data, not to be confused with -999)
    group_by(lakeid, metric) |> 
    summarise(n = n(), real = sum(weibull.max == TRUE), 
              per = real/n, r2.median = median(weibull.r2, na.rm = T), 
              diffDays.median = median(diffDays, na.rm = T)) |> 
    filter(n > 10) |> 
    mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
    mutate(metric = factor(metric, levels = rev(vars_order), labels = rev(vars_labels))) |> 
    filter(!is.na(metric)) |> 
    mutate(fit = case_when(r2.median >= 1.0 ~ 'No Weibull',
                           r2.median >= 0.9 ~ '0.9 - 1.0',
                           r2.median >= 0.8 ~ '0.8 - 0.9',
                           r2.median >= 0.7 ~ '0.7 - 0.8',
                           r2.median < 0.7 ~ '< 0.7')) |> 
    mutate(fit = factor(fit, levels = c('< 0.7', '0.7 - 0.8', '0.8 - 0.9', '0.9 - 1.0','No Weibull'))) 
  
  
  makeTile <- function(usevars) {
    usedata = dat.summary |> filter(metric %in% usevars)
    
    ggplot(usedata) +
      geom_tile(aes(x = lakeid, y = metric, fill = fit), alpha = 0.8, color = 'black', linewidth = 0.1) +
      geom_tile(data = usedata |> filter(diffDays.median > 20), aes(x = lakeid, y = metric), 
                fill = NA, color = 'black', linewidth = 0.4, width = 1, height = 1) +
      geom_text(data = usedata |> filter(r2.median < 1), aes(x = lakeid, y = metric, 
                label = round(r2.median,2)), size = 2.2) +
      scale_fill_manual(values = c('#f5e3e1','#f0a689','#e3c54f','#63ab7f','grey10'), 
                        drop = F, na.translate = F, name = 'Median r<sup>2</sup>') +
      theme_minimal(base_size = 9) +
      theme(panel.grid.major = element_blank(), 
            axis.title.x = element_blank(), 
            legend.title = element_markdown())
  }
  
  ################# SI FIGURE TILE #################
  usevars = c("Ice-off", "Strat onset", "Energy", "Schmidt", 'Strat offset','Ice-on')
  p1 = makeTile(usevars) + ylab('Physical')
  
  usevars = c('Silica min',
          "TN min", 
          "TN-filtered min",
          'NH4 min', 
          'NO3-NO2 min', 
          
          'TP min',
          "TP-filtered  min", 
          'DRP min',
          'DOC min','DOC max', 
          "pH min","pH max")
  p2 = makeTile(usevars) + ylab('Chemical')
  
  usevars = c('Oxygen min',  'Secchi max', 'Secchi min', 'Zoop max')
  p3 = makeTile(usevars) + ylab('Biological') +
    scale_x_discrete(labels = c('Big Muskellunge', 'Trout', 'Crystal', 'Sparkling',
                                'Crystal Bog', 'Trout Bog', 'Mendota', 'Monona', 'Allequash', 'Wingra')) +
    theme(axis.text.x = element_text(angle = 45, hjust  = 1, vjust = 1))
  
  # Join
  p1/p2/p3 + plot_layout(heights = c(6, 10, 4), guides = 'collect')
  
  #save
  ggsave(filename = path_out, width = 6, height = 5, dpi = 500)        
  
  ################# MANUSCRIPT FIGURE TILE #################
  usevars = c("Ice-off", "Strat onset", "Energy", "Schmidt", 'Strat offset','Ice-on')
  p4 = makeTile(usevars) + ylab('Physical')
  
  usevars = c('Silica min', "TN min", "TP min", "pH max")
  p5 = makeTile(usevars) + ylab('Chemical')
  
  usevars = c('Oxygen min', 'Secchi max', 'Zoop max')
  p6 = makeTile(usevars) + ylab('Biological') +
    scale_x_discrete(labels = c('Big Muskellunge', 'Trout', 'Crystal', 'Sparkling',
                                         'Crystal Bog', 'Trout Bog', 'Mendota', 'Monona', 'Allequash', 'Wingra')) +
    theme(axis.text.x = element_text(angle = 45, hjust  = 1, vjust = 1))
  
  ################################ Join ################################
  p4/p5/p6 + plot_layout(heights = c(6, 4, 3), guides = 'collect')
  
  ggsave(filename = path_out2, width = 6, height = 3, dpi = 500)        
}
