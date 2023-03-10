library(ggcorrplot)
library(tidyverse)

plotCorrplot <- function(inputCorr) {
  ###### Compute a correlation matrix ######
  corr <- round(cor(inputCorr,use = "pairwise.complete.obs"), 2)
  # Compute a matrix of correlation p-values
  p.mat <- cor_pmat(inputCorr)
  # Correlation plot at p < 0.05 sig
  c.plot = ggcorrplot(corr, type = "full", hc.order = F,
                      lab = TRUE, p.mat = p.mat, insig = "blank",
                      outline.col = "white", tl.cex = 8, lab_size = 2,
                      ggtheme = ggplot2::theme_bw(base_size = 8), sig.level = 0.05,
                      colors = c("#6D9EC1", "grey95", "#E46726"))
  return(c.plot)
}

