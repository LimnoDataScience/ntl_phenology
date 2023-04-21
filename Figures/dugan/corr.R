library(tidyverse)
library(lubridate)
library(ggcorrplot)
library(corrr)
library(patchwork)

# Load modified corr function, for better visualization
source('~/Documents/Rpackages/SSB-snow-removal-plankton-response/SSBcode/Functions//network_plot2.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source('SSBcode/00_LoadData.R')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")
dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

datwide = dat |> select(-daynum, - sampledate, -filled_flag) |> 
  pivot_wider(names_from = metric, values_from = daynum_fill)

lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "FI", "ME", "MO", "WI")
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")


# plot two vars
ggplot(datwide) +
  geom_point(aes(x = chlor_spring, y = secchi_openwater)) +
  geom_abline() +
  facet_wrap(~lakeid)


ggplot(datwide)  +
  geom_line(aes(x = year, y = straton)) +
  geom_line(aes(x = year, y = iceoff)) +
  facet_wrap(~lakeid)

as.numeric(ccf(datwide$iceoff,datwide$straton, plot = F, lag.max = 0)$acf)

datwide %>%
  group_by(lakeid) %>%
  summarise(ccfout = as.numeric(ccf(iceoff, straton, lag.max = 0)$acf))


plots = list()
corrlist = list()
for (i in 1:length(lakes_order)) {
  useLake = lakes_order[i]
  
  useVars = datwide |> filter(lakeid == useLake) |> 
    select(-lakeid, -year)
  
  ###### Compute a correlation matrix ######
  corr <- round(cor(useVars,use = "pairwise.complete.obs"), 2)
  # Compute a matrix of correlation p-values
  p.mat <- round(cor_pmat.hd(useVars), 2)
  
  corrlist[[i]] = reshape2::melt(corr, na.rm = F, value.name = 'corr') |> 
    left_join(reshape2::melt(p.mat, na.rm = F, value.name = 'pval')) |> 
    mutate(corr = if_else(pval <= 0.05, corr, NA_real_)) |> 
    mutate(lakeid = useLake)
    
  
  # Correlation plot at p < 0.05 sig
  c.plot = ggcorrplot(corr, type = "upper", hc.order = F,
                      lab = TRUE, p.mat = p.mat, insig = "blank",
                      outline.col = "white", tl.cex = 8, lab_size = 2,
                      ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
                      colors = c("#E46726", "grey95", "#6D9EC1"),
                      title = useLake)
  
  ### correlation network map
  varCorr = correlate(useVars)
  c2.plot = network_plot2.cor_df(varCorr, min_cor = 0.5,
                                 colours = c("#E46726", "grey95", "#6D9EC1"))
  
  # Join figures together
  plots[[i]] = c.plot + theme(legend.position = 'none') + #c2.plot + 
    # plot_layout(widths = c(0.4,0.5)) +
    plot_annotation(title = useLake, tag_levels = 'a', tag_suffix = ')') & 
    theme(plot.tag = element_text(size= 8),
          axis.title = element_blank(),
          # legend.title = element_blank(),
          legend.key.width = unit(0.3, 'cm'),
          legend.text = element_text(size = 8),
          plot.margin = margin(l = 0.05, t = 0.05, r = 0.05, unit='cm'))
}

wrap_plots(plots, tag_level = 'keep')
ggsave('Figures/dugan/corrplots.png', width = 12, height = 12)

corrdf = bind_rows(corrlist) |> 
  mutate(lakeid = factor(lakeid, levels = rev(lakes_order))) |> 
  mutate(Var2 = factor(Var2, levels = vars_order))

useVar = 'secchi_openwater'

plots.cor = list()
for (i in 1:length(vars_order)) {
plots.cor[[i]]  = ggplot(corrdf |> filter(Var1 == vars_order[i]) |> filter(Var2 !=  vars_order[i])) +
  geom_tile(aes(x = Var2, y = lakeid, fill = corr), col = 'black') +
  scale_fill_gradientn(colours = rev(met.brewer('Homer1')), na.value = 'white', limits=c(-1, 1)) +
  labs(title = vars_order[i]) +
  theme_bw(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}
wrap_plots(plots.cor, tag_level = 'keep') + plot_layout(guides = 'collect')
ggsave('Figures/dugan/corrplots2.png', width = 15, height = 12)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function that can handle NAs
cor_pmat.hd <- function (x, ...) {
  insufData = FALSE
  mat <- as.matrix(x)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tryCatch(stats::cor.test(mat[, i], mat[, j], ...), error = function(e) {insufData <<- TRUE})
      if(insufData) { 
        p.mat[i, j] <- p.mat[j, i] <- 1
      } else {
        tmp <- stats::cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

