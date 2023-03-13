library(tidyverse)
library(patchwork)
library(scales)

figure3 <- function(path_in, path_out, path_out2) {
  
  dat = read_csv(path_in) |> filter(lakeid != 'FI')
  
  lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "ME", "MO", "WI")
  
  vars_order = c("iceoff", "straton", "stability", "energy","stratoff", "iceon",
                 "drsif_surfMin", "nh4_surfMin", "no3no2_surfMin", 'totpuf_surfMin', 'doc_surfMax',
                 "minimum_oxygen", "secchi_max", "secchi_springmax", "zoop_max")

  vars_labels = c("Ice off", "Strat onset", "Stability", "Energy", 'Strat offset','Ice on',
                  'Si surf min', 'NH4 surf min', 'NO3 surf min', 'TP surf min', 'DOC surf max',
                  'Oxygen min', 'Secchi max', 'Secchi spring max', 'Zoop max')
  
  ##### Functions #####
  all_na <- function(x) all(is.na(x))
  
  corrFunction <- function(x,y) {
    round(cor(x, y, use = "pairwise.complete.obs", method = 'pearson'), 2)
  }
  corrPFunction <- function(x,y) {
    round(cor.test(x, y, method = 'pearson')$p.value, 3)
  }
  
  customTheme <- theme_bw(base_size = 8)  +
    theme(legend.position = 'bottom', 
          legend.title = element_blank(), 
          axis.title = element_blank(),
          legend.key.width = unit(0.2, 'cm'),
          legend.key.height = unit(0.2, 'cm'),
          legend.margin=margin(t = 0, unit='cm'),
          plot.title = element_text(size = 8),
          panel.grid.major = element_line(size = 0.2))
  
  ################################ ME and MO ################################
  
  compareLakes <- function(useLakes, colors, useTitle) {
    
    df1 = dat %>%
      mutate(dayWeibull = if_else(metric %in% c('iceoff','iceon'), daynum, dayWeibull)) |> 
      mutate(dayWeibull = if_else(dayWeibull == -999, NA_real_, dayWeibull)) |> 
      filter(lakeid %in% useLakes) |> 
      filter(metric %in% vars_order) %>% 
      mutate(lakeid = factor(lakeid, levels = lakes_order),
             metric = factor(metric, levels = rev(vars_order), labels = rev(vars_labels))) 
    
    df1.Cor = df1 |> 
      dplyr::select(lakeid, year, metric, dayWeibull) |> 
      pivot_wider(names_from = lakeid, values_from = dayWeibull) |> 
      mutate(across(where(all_na), ~replace_na(.x, 0))) |> 
      group_by(metric) |>
      na.omit() |> 
      filter(n() >= 9) |> 
      rename(Lake1 = 3, Lake2 = 4) |> 
      summarise(cor = corrFunction(Lake1,Lake2), corP = corrPFunction(Lake1, Lake2))
    
    
    p1 = ggplot(df1) + 
      geom_boxplot(aes(x = as.Date(dayWeibull, origin = as.Date('2019-01-01')), 
                       y = metric, color = lakeid, fill = lakeid), 
                   alpha = 0.5, position = position_dodge(0.2), size = 0.2, outlier.size = 1, outlier.stroke = 0.2) +
      geom_point(data = df1.Cor |> filter(corP <= 0.05), aes(x =  as.Date('2019-03-01'), y = metric), size = 1, shape = 8) +
      geom_hline(aes(yintercept = 4.5), linetype = 2, size = 0.2) +
      geom_hline(aes(yintercept = 9.5), linetype = 2, size = 0.2) +
      scale_fill_manual(values = colors) + 
      scale_color_manual(values = colors) + 
      scale_x_date(labels = date_format("%b"), breaks = '3 months', minor_breaks = '1 month') +
      labs(title = useTitle) +
      customTheme
    
    return(p1) 
  }
  
  
  p1 = compareLakes(useLakes = c('SP','TR'), colors = c("#74a9cf", "#2b8cbe"), useTitle = 'a) Oligotrophic')
  p2 = compareLakes(useLakes = c('CB','TB'), colors =  c("#cc4c02","#8c2d04"), useTitle = 'b) Dystrophic')  
  p3 = compareLakes(useLakes = c('ME','MO'), colors = c("#74c476", "#238b45"), useTitle = 'c) Eutrophic')
  
  ################################ JOIN ################################
  p1 + p2 + p3
  
  ggsave(filename = path_out, width = 6, height = 3.5, dpi = 500)
  
  # All oligotrophic lake pairs 
  p10 = compareLakes(useLakes = c('CR','SP'), colors = c("#74a9cf", "#2b8cbe"), useTitle = NULL)
  p11 = compareLakes(useLakes = c('CR','BM'), colors = c("#74a9cf", "#2b8cbe"), useTitle = NULL)
  p12 = compareLakes(useLakes = c('CR','TR'), colors = c("#74a9cf", "#2b8cbe"), useTitle = NULL)
  p13 = compareLakes(useLakes = c('SP','BM'), colors = c("#74a9cf", "#2b8cbe"), useTitle = NULL)
  p14 = compareLakes(useLakes = c('SP','TR'), colors = c("#74a9cf", "#2b8cbe"), useTitle = NULL)
  p15 = compareLakes(useLakes = c('BM','TR'), colors = c("#74a9cf", "#2b8cbe"), useTitle = NULL)
  
  p10 + p11 + p12 + p13 + p14 + p15
  
  ggsave(filename = path_out2, width = 6, height = 5, dpi = 500)
  
}


