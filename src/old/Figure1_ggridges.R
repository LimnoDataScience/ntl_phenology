
### Figure 1 ###

figure1 <- function(path_in, path_out) {
  dat = read_csv(path_in)
  # dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill
  
  vars_order = c("iceoff", "straton", "secchi_max", "secchi_min", "zoopDensity", "doc_epiMax", 
                 "drsif_epiMin", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", 
                 "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
  vars_label = c("ice off", "strat onset", "SecchiMax","SecchiMin", "zoopDensity", "DOC max", 
                 "Si Min", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", 
                 "TP epi min", "TP hypo max", "strat offset", "ice on")
  
  # add and extra lake in N
  lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "", "FI", "ME", "MO", "WI")
  
  empty_lake = dat %>% 
    select(metric) %>% 
    mutate(lakeid = "") %>% 
    distinct()
  
  pRidges = dat %>% 
    full_join(empty_lake) %>% 
    filter(metric %in% vars_order) %>% 
    mutate(lakeid = factor(lakeid, levels = lakes_order),
           metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) %>% 
    ggplot() + 
    stat_density_ridges(aes(x = as.Date(daynum, origin = as.Date('2019-01-01')), 
                            y= metric, col = metric, fill = metric), 
                        alpha = 0.5, quantile_lines = T, quantiles = 2) +
    scale_fill_manual(values=met.brewer("Archambault", length(vars_order))) + 
    scale_color_manual(values=met.brewer("Archambault", length(vars_order))) +
    scale_x_date(labels = date_format("%b")) +
    facet_wrap(~ (lakeid)) +
    xlab('') + ylab('')+
    theme_minimal(base_size = 6) + 
    theme(axis.text = element_text(size=6),
          strip.text=element_text(size=10),
          panel.spacing.y=unit(0.4,"lines")) +
    guides(fill="none", color="none"); pRidges
  
  ggsave(filename = path_out,
         pRidges, width=6, height=5, units="in", dpi=500, bg="white")

}
