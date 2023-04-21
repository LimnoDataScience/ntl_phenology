# Existence and coupling of phenological signals in north-temperate lakes
# driven by trophic status and morphometry

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")
dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

# Fig 1: Ridges
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
vars_label = c("ice off", "strat onset", "spring bloom", "clearwater", "daphnia", "DOC", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", "TP epi min", "TP hypo max", "strat offset", "ice on")

# add and extra lake in N
lakes_order = c("CB", "TB", "AL", "BM", "CR", "SP", "TR", "", "FI", "ME", "MO", "WI")
# vars_order = c("iceoff", "straton", "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc", "chlor_all", "anoxia_summer", "stability", "energy", "chlor_fall", "stratoff", "iceon")

datwide = dat |> select(-daynum, - sampledate, -filled_flag) |>
  pivot_wider(names_from = metric, values_from = daynum_fill) |> 
  mutate(lakeid = factor(lakeid, levels = rev(lakes_order)))


colors = c('brown4','brown4', rep('lightblue4',5), rep('seagreen4',3), 'gold2')

boxplots = list()
for (i in 1:length(vars_order)) {
  numlakes = datwide |> filter(!is.na(!!as.name(vars_order[i]))) |> group_by(lakeid) |> 
    summarise(first(lakeid)) |> nrow()
  
  # boxplots[[i]] = ggplot(datwide) +
  #   geom_boxplot(aes_string(x = vars_order[i], y = 'lakeid'), fill = rev(colors[1:numlakes])) +
  #   theme_bw(base_size = 9) +
  #   scale_x_continuous(limits = c(0,365), expand = c(0,0), name = 'day') +
  #   labs(title = vars_order[i])
  
  boxplots[[i]] = ggplot(datwide) +
    geom_boxplot(aes(x = as.Date(!!as.name(vars_order[i]), origin = as.Date('2019-01-01')), y = lakeid), fill = rev(colors[1:numlakes])) +
    theme_bw(base_size = 9) +
    # scale_y_discrete(limits=rev) +
    scale_x_date(limits = c(as.Date('2019-01-01'), as.Date('2019-12-31')), 
                 expand = c(0,0), name = 'Date', labels = date_format("%b")) +
    labs(title = vars_order[i]) 
  
}

wrap_plots(boxplots) 
ggsave('Figures/dugan/varBoxplots.png', width = 15, height = 12)

