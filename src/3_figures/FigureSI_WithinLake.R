# This code uses the tidyverse, lubridate, ggcorrplot, and corrr packages to
# analyze phenology data among 11 lakes. First, the variables are labeled and
# ordered, and the required data is read from a ".csv" file. The code then
# creates a plot of the Interquartile Ranges (IQR) of the phenology data for the
# 11 lakes, and saves the plot as a ".png" file. Next, the code creates a
# correlation matrix of the phenology data for the 11 lakes. The correlation
# matrix is melted and stored as a dataframe. This dataframe is then used to
# create a plot of the correlation of the phenology data for the 11 lakes, and
# the plot is saved as a ".png" file.

figureSI_withinLake <- function(path_in, path_out) {
  
  
  vars_order = c("iceoff", "straton", "energy", "schmidt", "stratoff", "iceon",
                 "drsif_surfMin", "nh4_surfMin", "no3no2_surfMin", 'totpuf_surfMin', 'doc_surfMax',
                 "minimum_oxygen", "secchi_max", "secchi_springmax")
  
  vars_labels = c("Ice off", "Strat onset", "Energy", "Schdmit", 'Strat offset','Ice on',
                  'Si surf min', 'NH4 surf min', 'NO3 surf min', 'TP surf min', 'DOC surf max',
                  'Oxygen min', 'Secchi max', 'Secchi spring max')
  
  ################################ Correlation ################################
  # read in data
  dat = read_csv(path_in) |> filter(lakeid != 'FI') |> 
    filter(metric %in% vars_order)
  
  # Select data
  lakenames =  c("AL", "BM", "CR", "SP", "TR", "TB", "CB", "ME", "MO", "WI")
  c.plot.list = list()
  coff.df.list = list()
  for (i in 1:length(lakenames)) {
    useVars = dat |> 
      mutate(dayWeibull = if_else(metric %in% c('iceoff','iceon'), daynum, dayWeibull)) |> 
      mutate(dayWeibull = if_else(dayWeibull == -999, NA_real_, dayWeibull)) |> 
      filter(lakeid == lakenames[i]) |> 
      filter(!is.na(metric)) |> 
      dplyr::select(year, metric, dayWeibull) |> 
      arrange(metric) |> 
      pivot_wider(names_from = metric, values_from = dayWeibull) |> 
      dplyr::select(-year)
    
    ###### Compute a correlation matrix ######
    all_na <- function(x) all(is.na(x))
    na5 <- function(x) sum(!is.na(x)) < 5
    
    useVars.na = useVars |>  
      mutate(
        across(where(all_na), ~replace_na(.x, 0))
      ) |> 
      mutate(
        across(where(na5), ~0) # replace any data with less than 5 measurements with 0
      )
    
    usecorr <- round(cor(useVars.na,use = "pairwise.complete.obs", method = 'pearson'), 2)
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(useVars.na) 
  
    # Melt
    usecorr <- reshape2::melt(usecorr, na.rm = FALSE, value.name = 'corr')
    p.mat <- reshape2::melt(p.mat, na.rm = FALSE, value.name = 'p') |> 
      rename(Var1 = 1, Var2 = 2)
    # |> mutate(p = p.adjust(p, method = "holm"))
    
    coff.df = usecorr |> as_tibble() |> 
      left_join(p.mat) |> 
      mutate(corr.p = if_else(p <= 0.01, corr, NA_real_)) |> 
      mutate(lakeid = lakenames[i])
  
    coff.df.list[[i]] = coff.df
    
    # # Correlation plot at p < 0.05 sig
    # c.plot = ggcorrplotHD(usecorr, type = "full", hc.order = F,
    #                     lab = TRUE, p.mat = p.mat, insig = "blank",
    #                     outline.col = "white", tl.cex = 8, lab_size = 2,
    #                     ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
    #                     colors = c("#E46726", "grey95", "#6D9EC1")) +
    #   labs(title = lakenames[i])
    # c.plot.list[[i]] = c.plot
  
  }
  
  # wrap_plots(c.plot.list)
  
  # Bind list
  coff.df = do.call(rbind.data.frame, coff.df.list) |> 
    mutate(lakeid = factor(lakeid, levels = c("AL", "BM", "CR", "SP", "TR", "CB", "TB", "ME", "MO", "WI"))) |> 
    arrange(Var1) |> 
    mutate(Var1 =  factor(Var1, levels = vars_order)) |> 
    mutate(Var2 =  factor(Var2, levels = vars_order)) |> 
    mutate(lakeid = if_else(!is.na(corr.p), lakeid, as.factor(NA_character_))) |> 
    mutate(lakeid = if_else(corr.p < 1, lakeid, as.factor(NA_character_)))

  
  emptyDF = expand_grid(Var1 = vars_order, Var2 = vars_order) |> 
    mutate(corr = NA, p = NA, corr.p = NA, lakeid = NA) |> 
    mutate(Var1 = factor(Var1, levels = vars_order)) |> 
    mutate(Var2 = factor(Var2, levels = vars_order)) 
  
  
  plotcor <- function(uselakes, usecolors) {
    
    box1 = 6.5
    box2 = 11.5
    
    coff.df |> filter(lakeid %in% uselakes) |> 
      # need a row with ice on so axes match up in figure 
      bind_rows(emptyDF) |> 
    
    # coff.df |> mutate(if_else(lakeid %in% uselakes, ))  
    ggplot(mapping = aes(x = Var1, y = Var2, fill = lakeid, color = lakeid)) +
      geom_jitter(shape = 21, size = 2, width = 0.15, height = 0.15, alpha = 0.8, stroke = 0.2) +
      # geom_tile(color = 'gray') +
      scale_fill_manual(values = usecolors, na.translate = F) +
                                              scale_color_manual(values = rep('black', 10), na.translate = F) +
      scale_x_discrete(breaks = vars_order, labels = vars_labels) +
      scale_y_discrete(breaks = vars_order, labels = vars_labels) +
      geom_segment(aes(x = box1, y = -Inf, xend = box1, yend = box1), linetype = 2, linewidth = 0.2, show.legend=FALSE) +
      geom_segment(aes(x = -Inf, y = box1, xend = box1, yend = box1), linetype = 2, linewidth = 0.2, show.legend=FALSE) +
      geom_segment(aes(x = box2, y = -Inf, xend = box2, yend = box2), linetype = 2, linewidth = 0.2, show.legend=FALSE) +
      geom_segment(aes(x = -Inf, y = box2, xend = box2, yend = box2), linetype = 2, linewidth = 0.2, show.legend=FALSE) +
      theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 5),
            axis.text.y = element_text(size = 6),
            legend.position = 'bottom', 
            legend.title = element_blank(), 
            axis.title = element_blank(),
            legend.key.width = unit(0.2, 'cm'),
            legend.key.height = unit(0.2, 'cm'),
            legend.margin=margin(t = 0, unit='cm'),
            panel.grid.major = element_line(linewidth = 0.2))
  }
  
  p1 = plotcor(uselakes = c("BM", "CR", "SP", "TR"), 
          usecolors = c( "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d"))
  p2 = plotcor(uselakes = c("CB","TB"), usecolors = c("#cc4c02", "#8c2d04")) +
    theme(axis.text.y = element_blank())
  p3 = plotcor(uselakes = c("ME", "MO"), usecolors = c("#74c476", "#238b45")) +
    theme(axis.text.y = element_blank())
  
  ################################ Join ################################
  plot.Cor = p1 + p2 + p3 +
    plot_annotation(tag_levels = 'a', tag_suffix = ')') &
    theme(plot.tag = element_text(size  = 8))

  ggsave(plot = plot.Cor, filename = path_out, width = 6, 
         height = 3, dpi = 500)

}

# coff.table = coff.df |> 
#   dplyr::select(lakeid, Var1, Var2, corr, p) |> 
#   arrange(lakeid)
  
# print(xtable(coff.table), include.rownames = FALSE)
