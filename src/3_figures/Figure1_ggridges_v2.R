
### Figure 1 ###

figure1_v2 <- function(path_in, path_out) {
  dat = read_csv(path_in)
  
  vars_order = c("", "iceoff", "straton", "stability", "energy","stratoff", "iceon",
                 # "doc_epiMax", 
                 "drsif_epiMin",  "totnuf_epiMin", "totpuf_epiMin", 
                 "totnuf_hypoMax","totpuf_hypoMax", 
                 "minimum_oxygen", "secchi_max", "secchi_min", "zoopDensity")
  
  vars_label = c("","Ice off", "Strat onset", "Stability", "Energy", 'Strat offset','Ice on',
                 # 'DOC epi max', 
                 'Si epi min', 'TN epi min', 'TP epi min', 
                 'TN hypo max', 'TP hypo max',
                 'Oxygen min', 'Secchi max', 'Secchi min', 'Zoop Density')
  
  lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "ME", "MO", "WI")
  
  dat.iqr = dat |> 
    group_by(lakeid, metric) |> 
    summarise(day.mean = mean(daynum, na.rm = T), day.IQR = IQR(daynum, na.rm = T)) |> 
    ungroup() |> 
    mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
    mutate(metric = factor(metric, levels = vars_order)) |> 
    filter(!is.na(metric))
  
  plotridge <- function(uselakes) {
    dat %>% 
      filter(lakeid %in% uselakes) |> 
      filter(metric %in% vars_order) %>% 
      mutate(lakeid = factor(lakeid, levels = lakes_order),
             metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) %>% 
      ggplot() + 
      stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                              y= metric, col = metric, fill = metric), 
                          alpha = 0.5, quantile_lines = T, quantiles = 2, linewidth = 0.3) +
      # scale_fill_manual(values=met.brewer("Archambault", length(vars_order))) + 
      # scale_color_manual(values=met.brewer("Archambault", length(vars_order))) +
      scale_fill_manual(values = rev(c(rep('#e3d35d',6), rep('#97bab7',5), rep('#bf7058',4)))) +
      scale_color_manual(values = rev(c(rep('#e3d35d',6), rep('#97bab7',5), rep('#bf7058',4)))) +
      scale_x_date(labels = date_format("%b")) +
      scale_y_discrete(expansion(add = c(0, 2))) +
      facet_wrap(~lakeid, nrow = 1, strip.position = "top") +
      theme_minimal(base_size = 8) + 
      theme(axis.text.y = element_text(size=6),
            axis.title = element_blank(), 
            strip.text = element_text(size=10),
            panel.spacing.y = unit(0.4,"lines"),
            strip.background = element_blank(),
            panel.grid.major = element_line(size = 0.2)) +
      guides(fill="none", color="none")
  }
  
  
  plotiqr <- function(uselakes) {
    empty_metric = dat.iqr %>% 
      select(lakeid) %>% 
      mutate(metric = "") %>% 
      distinct()
    
    dat.iqr |> full_join(empty_metric) |>   
    mutate(lakeid = factor(lakeid, levels = lakes_order),
           metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) |> 
      filter(lakeid %in% uselakes) |> 
  
    ggplot() + 
      geom_vline(aes(xintercept = 28), linetype = 2) +
      geom_jitter(aes(y = metric, x = day.IQR, fill = metric), shape = 21, size = 1.5, width = 0.2, height = 0, stroke = 0.2) +
      xlab('IQR (days)') +
      scale_fill_manual(values = rev(c(rep('#e3d35d',7), rep('#97bab7',5), rep('#bf7058',4)))) +
      # scale_fill_manual(values=met.brewer("Archambault", length(vars_order))) + 
      theme_minimal(base_size = 8) +
      # labs(title = 'IQR (days)') +
      xlim(0,200) +
      theme(axis.text.y = element_blank(),
            axis.title = element_blank(),
            legend.position = 'none',
            legend.key.height = unit(0.3,'cm'),
            legend.title = element_blank(),
            panel.grid.major = element_line(size = 0.2)) 
  
  }
  
  
  
  p1 = plotridge(uselakes = c("BM", "TR"))
  p1.5 = plotridge(uselakes = c("CR", "SP"))
  p2 = plotridge(uselakes = c("CB","TB"))
  p3 = plotridge(uselakes = c("ME", "MO"))
  p4 = plotridge(uselakes = c("AL", "WI"))
  
  p11 = plotiqr(uselakes = c("BM", "CR", "SP", "TR"))
  p21 = plotiqr(uselakes = c("CB","TB"))
  p31 = plotiqr(uselakes = c("ME", "MO"))
  p41 = plotiqr(uselakes = c("AL", "WI"))
  
  
  layout <- '
  AAF
  BB#
  CCG
  DDH
  EEI
  '
  
  p1 + p1.5 + p2 + p3 + p4 + p11 + p21 + p31 + p41 +
    plot_layout(design = layout) &
    theme(
      panel.background = element_rect(fill='transparent', color = NA), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA) #transparent plot bg
    )
  
  # ggsave('Figures_manuscript/Figure1_v2.png',
  #        width=6, height=8, units="in", dpi=500, bg='transparent')
  
  ggsave(path_out,
         width=6, height=8, units="in", dpi=500, bg='transparent')
  
}
