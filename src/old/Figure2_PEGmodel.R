
### Figure 2 ###
figure2 <- function(path_in, path_out, path_out2) {
  
  # Which spring metrics
  useorder = c("drsif_springSurfMin","zoop_springmax","secchi_springmax")
  useorder = c("drsif_surfMin","zoop_max","secchi_max")
  
  # lakeorder = c('BM','TR','CR','SP','CB','TB','ME','MO','AL','WI')
  # lakenames = data.frame(lakeid = lakeorder,
  #                        lakenames = c('Big Musky','Trout', 'Crystal', 'Sparkling', 'Crystal Bog','Trout Bog',
  #                                      'Mendota', 'Monona', 'Allequash', 'Wingra'))
  lakeorder = c('ME','MO')
  lakenames = data.frame(lakeid = lakeorder,
                         lakenames = c('Mendota', 'Monona'))
  
  # Read in data
  dat = read_csv(path_in) |> 
    mutate(weibull.r2 = if_else(weibull.max == FALSE, NA_real_, weibull.r2)) |> # filter out dates when peak is greater than beginning and end
    filter(weibull.r2 > 0.7) |> 
    filter(lakeid %in% lakeorder) |> 
    filter(metric %in% useorder) |> 
    group_by(lakeid, year) %>%
    mutate(missing = if_else(any(is.na(dayWeibull)), TRUE, FALSE)) |>
    ungroup() |> 
    mutate(dayWeibull = if_else(missing == TRUE, NA_real_, dayWeibull)) 
  
  # Find PEG years
  PEGyears = dat |> 
    # filter(missing == FALSE) |> 
    group_by(lakeid, year) |> 
    mutate(n = n()) |> filter(n == 3) |> 
    # mutate(order = case_when(metric == 'drsif_springSurfMin' ~ 1,
    #                          metric == 'zoop_springmax' ~ 2,
    #                          metric == 'secchi_springmax' ~ 3)) |> 
    mutate(order = case_when(metric == 'drsif_surfMin' ~ 1,
                             metric == 'zoop_max' ~ 2,
                             metric == 'secchi_max' ~ 3)) |> 
    arrange(order, .by_group = TRUE) |> 
    mutate(rank = data.table::frank(dayWeibull, ties.method = 'average')) |> 
    mutate(use = case_when(first(rank) == 1 & last(rank) == 3 ~ TRUE,
                           first(rank) == 1.5 & last(rank) == 3 ~ TRUE,
                           first(rank) == 1 & last(rank) == 2.5 ~ TRUE,
                           TRUE ~ FALSE)) |> 
    slice(1) |> 
    dplyr::select(lakeid, year, use)
  
  # Peg percent years 
  PEGyears |> group_by(lakeid) |> 
    summarise(total = n(), PEG = sum(use)) |> 
    mutate(PEGper = 100*PEG/total)
  
  # PEG days between silica min and max Secchi
  dat |> dplyr::select(lakeid, year, metric, dayWeibull) |> 
    pivot_wider(names_from = metric, values_from = dayWeibull) |> 
    mutate(diff = secchi_max - drsif_surfMin) |> 
    group_by(lakeid) |> 
    summarise(meandiff = mean(diff, na.rm = T), mediandiff = median(diff, na.rm = T))
  
  # t-test on days 
  stat.out = dat |> 
    filter(lakeid %in% c('ME','MO')) |> 
    dplyr::select(lakeid, year, metric, dayWeibull) |> 
    pivot_wider(names_from = metric, values_from = dayWeibull) |> 
    mutate(diff = secchi_max - drsif_surfMin) |> 
    dplyr::select(year, lakeid, diff) |> 
    mutate(lakeid = as.factor(lakeid)) |> 
  pairwise_t_test(
    diff ~ lakeid, paired = FALSE, 
    p.adjust.method = "bonferroni"
  )
  
  print(stat.out, n = 60)
  
  p2 = dat |> filter(lakeid %in% lakeorder) |>
    left_join(PEGyears) |> 
    mutate(lakeid = factor(lakeid, levels = lakeorder)) |> 
    filter(metric %in% useorder) |> 
    mutate(metric = factor(metric, levels = useorder)) |> 
    ggplot() +
    geom_segment(data = . %>% filter(use == TRUE), aes(x = structure(-Inf, class = "Date"), 
                                                       xend = structure(Inf, class = "Date"), y = year, yend = year),
                 color = 'grey80', size = 3) +
    geom_path(aes(x = as.Date(dayWeibull, origin = as.Date('2019-01-01')), y = year, col = metric)) +
    geom_jitter(aes(x = as.Date(dayWeibull, origin = as.Date('2019-01-01')), y = year, fill = metric), 
                shape = 21, stroke = 0.2,
                width = 0, height = 0.1) +
    scale_color_manual(values = c('#97bab7','#e0c3ba','#bf7058'), 
                       labels = c('Min Silica','Max Zooplankton', 'Max Secchi'), name = 'Spring Metrics') +
    scale_fill_manual(values = c('#97bab7','#e0c3ba','#bf7058'),
                      labels = c('Min Silica','Max Zooplankton', 'Max Secchi'), name = 'Spring Metrics') +
    scale_y_reverse(minor_breaks = seq(1995, 2018), breaks = seq(1985, 2015, by = 5), expand = c(0.01,0)) +
    scale_x_date(labels = date_format("%b"), minor_breaks = '1 month', breaks = '2 month') +
    facet_wrap(~lakeid, ncol = 10) +
    # xlab('Day of the Year') +
    theme_minimal(base_size = 9) +
    theme(axis.title = element_blank(),
          legend.justification = "left",
          legend.margin=margin(t = -0.2, unit='cm'),
          legend.position = 'bottom'); p2
  
  ggsave(filename = path_out,
         width=6, height=6, units="in", dpi=500, bg='white')
  
  
  # Histograms of PEG years vs ice-off date
  # Lake names
  names = lakenames |> pull(lakenames)
  
  read_csv(path_in) |> filter(metric == 'iceoff') |> 
    dplyr::select(lakeid, year, dayWeibull) |> 
    left_join(PEGyears) |>
    left_join(lakenames) |> 
    mutate(use = if_else(use == TRUE, 'PEG year', 'Other')) |> 
    # mutate(use = factor(use, levels = c('PEG year', 'Other'))) |> 
    mutate(lakenames = factor(lakenames, levels = names)) |> 
    filter(!is.na(use)) |> 
    ggplot() + 
    geom_histogram(aes(x = as.Date(dayWeibull, origin = as.Date('2019-01-01')), group = use, 
                       fill = use, color = use),  size = 0.2, 
                   binwidth = 7, alpha = 0.5, position="identity") + 
    scale_x_date(labels = date_format("%b"), minor_breaks = '1 month', 
                 breaks = '1 month', name = 'Ice-off Date') +
    scale_y_continuous(breaks = 5, minor_breaks = NULL) +
    ylab('Number of years') +
    scale_fill_manual(values = c('#25394d','white')) +
    scale_color_manual(values = c(alpha('#25394d', 0.5),'black')) +
    facet_wrap(~lakenames, ncol = 4) +
    theme_minimal(base_size = 9) +
    theme(legend.title = element_blank(), 
          # legend.justification = 'left',
          legend.margin=margin(t = -0.2, unit='cm'),
          legend.position = c(0.85,0.1))
  
  ggsave(filename = path_out2,
         width=6, height=4, units="in", dpi=500, bg='white')
  
}
