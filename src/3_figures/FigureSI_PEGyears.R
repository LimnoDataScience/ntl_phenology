### Figure SI: PEG YEARS ###
figureSI_PEG <- function(path_in, path_out) {
  
  dat = read_csv(path_in) |> 
    mutate(diffDays = abs(daynum - dayWeibull)) |> 
    filter(weibull.r2 > 0.7 | diffDays <= 30)
  
  df = dat |> filter(lakeid %in% c('ME','MO'), metric %in% c('zoop_max', 'secchi_max')) |> 
    select(lakeid, metric, year, dayWeibull) |> 
    group_by(lakeid, year) |>
    mutate(n = n()) |> filter(n == 2) |>
    ungroup() |> 
    mutate(lakename = if_else(lakeid == 'ME', 'Lake Mendota','Lake Monona')) |> 
    mutate(lakename = if_else(lakeid == 'ME', 'Lake Mendota','Lake Monona'))
  
  
  # Find PEG years (difference between zoop and secchi is <= 21 days)
  PEGyears = df |> pivot_wider(names_from = metric, values_from = dayWeibull) |> 
    mutate(diff = abs(secchi_max - zoop_max)) |> 
    filter(diff <= 21)
  
  ggplot(df, aes(x = as.Date(dayWeibull, origin = as.Date('2019-01-01')), y = year)) +
    geom_segment(data = PEGyears, aes(x = as.Date(-Inf), xend = as.Date(Inf), y = year, yend = year), 
                 color = '#e5dded', linewidth = 2.5) +
    geom_path(aes(group = year)) +
    geom_point(aes(fill = metric), shape = 21, stroke = 0.2, size = 2) +
    scale_fill_manual(values = c('#2f8cad', '#bdbd31'), name = "", labels = c('Secchi max','Zoop max')) +
    scale_x_date(labels = date_format("%b")) +
    theme_bw(base_size = 9) +
    facet_wrap(~lakename) +
    theme(legend.position = 'bottom',
          axis.title = element_blank(), 
          strip.text = element_text(size=10),
          panel.spacing.y = unit(0.4,"lines"),
          strip.background = element_blank(),
          panel.grid.major = element_line(linewidth = 0.2),
      legend.key.width = unit(0.2, 'cm'),
      legend.key.height = unit(0.2, 'cm'),
      legend.margin=margin(t = 0, unit='cm')) 
  
  ggsave(filename = path_out, width = 4, height = 4, dpi = 500)       

}

