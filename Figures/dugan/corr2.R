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
  pivot_wider(names_from = lakeid, values_from = daynum_fill) |> 
  select(-WI)

lakes_order = c("CB","TB", "AL", "BM", "CR", "SP", "TR", "FI", "ME", "MO", "WI")
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")


plots = list()
corrlist = list()
for (i in 1:length(vars_order)) {
  useVar = vars_order[i]
  
  useVars = datwide |> filter(metric == useVar) |> 
    select(-metric, -year)
  
  ###### Compute a correlation matrix ######
  corr <- round(cor(useVars,use = "pairwise.complete.obs"), 2)
  # Compute a matrix of correlation p-values
  p.mat <- round(cor_pmat.hd(useVars), 2)
  
  corrlist[[i]] = reshape2::melt(corr, na.rm = F, value.name = 'corr') |> 
    left_join(reshape2::melt(p.mat, na.rm = F, value.name = 'pval')) |> 
    mutate(corr = if_else(pval <= 0.05, corr, NA_real_)) |> 
    mutate(var = useVar)
  
  
  # Correlation plot at p < 0.05 sig
  plots[[i]] = ggcorrplot(corr, type = "upper", hc.order = F,
                      lab = TRUE, p.mat = p.mat, insig = "blank",
                      outline.col = "white", tl.cex = 8, lab_size = 2,
                      ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
                      colors = c("#E46726", "grey95", "#6D9EC1"),
                      title = useVar)
}

wrap_plots(plots)
ggsave('Figures/dugan/corrplots3.png', width = 12, height = 12)

corrdf = bind_rows(corrlist) |> 
  mutate(Var1 = factor(Var1, levels = rev(lakes_order))) |> 
  mutate(Var2 = factor(Var2, levels = rev(lakes_order))) |> 
  mutate(var = factor(var, levels = vars_order))

plots.cor = list()
for (i in 1:length(vars_order)) {
  plots.cor[[i]]  = ggplot(corrdf |> filter(var == vars_order[i])) +
    geom_tile(aes(x = Var1, y = Var2, fill = corr), col = 'black') +
    scale_fill_gradientn(colours = rev(met.brewer('Homer1')), na.value = 'white', limits=c(-1, 1)) +
    labs(title = vars_order[i]) +
    theme_bw(base_size = 8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}
wrap_plots(plots.cor, tag_level = 'keep') + plot_layout(guides = 'collect')
ggsave('Figures/dugan/corrplots4.png', width = 15, height = 12)

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

