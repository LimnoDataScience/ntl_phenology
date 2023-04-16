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
                 "drsif_springSurfMin",  
                 "totnuf_surfMin",
                 "totpuf_surfMin", 
                 "minimum_oxygen", "secchi_max", "zoop_max")
  
  vars_labels = c("Ice off", "Strat onset", "Energy", "Schmidt", 'Strat offset','Ice on',
                  "Silica min",  
                  "TN min",
                  "TP min", 
                  'Oxygen min',  'Secchi max', 'Zoop max')
  
  # Read in data #
  dat = read_csv(path_in) |> 
    mutate(weibull.r2 = if_else(weibull.max == FALSE, NA_real_, weibull.r2)) |> # filter out dates when peak is greater than beginning and end
    filter(weibull.r2 > 0.7) |> 
    filter(lakeid != 'FI') |> 
    filter(metric %in% vars_order)
  
  ################################ Correlation ################################
  # Select data
  lakenames =  c("AL", "BM", "CR", "SP", "TR", "TB", "CB", "ME", "MO", "WI")
  c.plot.list = list()
  coff.df.list = list()
  
  for (i in 1:length(lakenames)) {
    useVars = dat |> 
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
    
    #### corr function that accepts NA values 
    myCorr <- function(x,y) {
      my.df <- data.frame(x,y)
      my.df.cmpl <- my.df[complete.cases(my.df), ]
      
      # 3 complete obs is the minimum for cor.test
      if (nrow(my.df.cmpl)<=2) {
        return(c(estimate.cor = NA, 
                 p.value = NA))
      } else {
        my.test <- cor.test(my.df.cmpl$x,my.df.cmpl$y)
        return(c(estimate = round(my.test$estimate,3), 
                 p.value = round(my.test$p.value,3)))
      }
    }
    
    ### Map correlation function to all combinations of metrics 
    usecorr = expand.grid(v1 = names(useVars.na),      # create combinations of names
                v2 = names(useVars.na)) %>%
      as_tibble() %>% 
      mutate(cor_test = map2(v1, v2, ~myCorr(unlist(useVars.na[,.x]),    # perform the correlation test for each pair and store it
                                               unlist(useVars.na[,.y]))), 
             corr = map_dbl(cor_test, "estimate.cor"),   # get the correlation value from the test
             p = map_dbl(cor_test, "p.value")) |> # get the p value from the test
      select(-cor_test) 
    
    ###############################################################################
    
    # usecorr <- round(cor(useVars.na,use = "pairwise.complete.obs", method = 'pearson'), 2)
    # # Compute a matrix of correlation p-values
    # p.mat <- cor_pmat(useVars.na) 
    # 
    # # Melt
    # usecorr <- reshape2::melt(usecorr, na.rm = FALSE, value.name = 'corr')
    # p.mat <- reshape2::melt(p.mat, na.rm = FALSE, value.name = 'p') |> 
    #   rename(Var1 = 1, Var2 = 2)
    # # |> mutate(p = p.adjust(p, method = "holm"))
     
    
    coff.df = usecorr |> 
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
    arrange(v1) |> 
    mutate(v1 =  factor(v1, levels = vars_order)) |> 
    mutate(v2 =  factor(v2, levels = vars_order)) |> 
    mutate(lakeid = if_else(!is.na(corr.p), lakeid, as.factor(NA_character_))) |> 
    mutate(lakeid = if_else(corr.p < 1, lakeid, as.factor(NA_character_)))

  
  emptyDF = expand_grid(v1 = vars_order, v2 = vars_order) |> 
    mutate(corr = NA, p = NA, corr.p = NA, lakeid = NA) |> 
    mutate(v1 = factor(v1, levels = vars_order)) |> 
    mutate(v2 = factor(v2, levels = vars_order)) 
  
  
  plotcor <- function(uselakes, usecolors) {
    
    box1 = 6.5
    box2 = 9.5
    
    coff.df |> filter(lakeid %in% uselakes) |> 
      # need a row with ice on so axes match up in figure 
      bind_rows(emptyDF) |> 
    
    # coff.df |> mutate(if_else(lakeid %in% uselakes, ))  
    ggplot(mapping = aes(x = v1, y = v2, fill = lakeid, color = lakeid)) +
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
