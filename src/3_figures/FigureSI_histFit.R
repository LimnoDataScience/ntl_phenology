
vars_order = c("iceoff", "straton", "energy", "schmidt", "stratoff", "iceon",
               "drsif_springSurfMin", 
               "totnuf_surfMin",
               "totpuf_surfMin", 
               "ph_surfMax",
               "minimum_oxygen", "secchi_max", "zoop_max")

dat = read_csv(path_in) 

dat |> 
  filter(metric %in% vars_order) |> 
  mutate(diffDays = abs(daynum - dayWeibull)) |> 
  # filter(weibull.r2 > 0.7 | diffDays <= 30) |> 
  mutate(r7 = if_else(weibull.r2 >= 0.7, TRUE, FALSE)) |> 
  mutate(dayDiff = (dayWeibull - daynum)) |> 
  ggplot() +
  geom_histogram(aes(x = dayDiff, fill = r7), color = 'black', linewidth = 0.2, binwidth = 20) +
  # geom_abline(linetype = 2) +
  scale_fill_manual(values = c('#f5e3e1','#63ab7f'), 
                    drop = F, na.translate = F, name = 'r<sup>2</sup> > 0.7') +
  # facet_wrap(~lakeid) +
  xlab('Weibull - maximum observation (days)') +
  ylab('Count') +
  theme_bw(base_size = 9) +
  theme(legend.title = element_markdown(),
        legend.key.size = unit(0.3,'cm'),
        legend.position = c(0.2,0.8))

ggsave(filename = 'Figures_manuscript/FigureSI_histFit.png', width = 3, height = 2, dpi = 500)        
