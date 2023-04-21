# make and save final figures for manuscript

library(tidyverse)
library(ggridges)
library(lubridate)
library(scales)
library(corrplot)
library(factoextra)
library(roll)
library(RolWinMulCor)
library(BINCOR)
library(corrr)
library(patchwork)
library(MetBrewer)
library(kohonen)
library(RColorBrewer)

# read in data
dat = read_csv("Data/analysis_ready/final_combined_dates_filled_v2.csv")
dat$sampledate = as.Date(paste0(dat$year-1, "-12-31")) + dat$daynum_fill

# Fig 1: Ridges
vars_order = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")
vars_label = c("ice off", "strat onset", "spring bloom", "clearwater", "daphnia", "DOC", "TP hypo min", "TP epi max",  "anoxia",  "stability", "energy", "TP epi min", "TP hypo max", "strat offset", "ice on")

# add and extra lake in N
lakes_order = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "", "FI", "ME", "MO", "WI")
# vars_order = c("iceoff", "straton", "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc", "chlor_all", "anoxia_summer", "stability", "energy", "chlor_fall", "stratoff", "iceon")

datwide = dat |> select(-daynum, - sampledate, -filled_flag) |>
  pivot_wider(names_from = metric, values_from = daynum_fill)

ggplot(datwide) +
  geom_point(aes(x = iceoff, y = iceon, color = year)) +
  scale_color_gradientn(colours = met.brewer('Homer1')) +
  facet_wrap(~lakeid) +
  theme_bw()

####### Northern lakes #######
# Fig 2: PCA loading
dat_pca = dat %>% filter(!lakeid %in% c('ME','MO','FI','WI')) |> 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) %>% 
  select(lakeid, year, metric, daynum_fill) %>% 
  pivot_wider(names_from = "metric", values_from="daynum_fill") %>% 
  filter(lakeid != "WI")

pca = prcomp(dat_pca[, 3:ncol(dat_pca)], scale=T, center=T)
lakeid_pca = lakes_order[!(lakes_order %in% c("", "WI"))]

# fviz_eig(pca)
p1 = fviz_pca_var(pca,col.var = "contrib", # Color by contributions to the PC
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE,     # Avoid text overlapping
                  arrowsize=1,
                  labelsize=3) +
  labs(x="PC 1", y="PC 2", title="", color="Contribution") +
  theme_minimal(base_size = 9) +
  theme(legend.position = 'none')

# Fig 3: PCA correlations
var = get_pca_var(pca)
inds = get_pca_ind(pca)

pca_res = dat_pca[, c("lakeid", "year")]
pca_res = pca_res |> bind_cols(pca$x[,1:4])

pca_res_cor = pca_res %>% 
  pivot_longer(cols = c("PC1", "PC2", "PC3", "PC4")) %>% 
  pivot_wider(names_from = "lakeid", values_from="value") %>% 
  arrange(name, year)

pca_matrix_p1 = pca_res_cor %>% 
  filter(name == "PC1") %>% 
  select(-year, -name)

pca_matrix_p2 = pca_res_cor %>% 
  filter(name == "PC2") %>% 
  select(-year, -name)

###### Compute a correlation matrix ######
corr <- round(cor(pca_matrix_p1,use = "pairwise.complete.obs"), 2)
# Compute a matrix of correlation p-values
p.mat <- round(cor_pmat.hd(pca_matrix_p1), 2)

# Correlation plot at p < 0.05 sig
p1.corrPC1 = ggcorrplot(corr, type = "upper", hc.order = F,
                        lab = TRUE, p.mat = p.mat, insig = "blank",
                        outline.col = "white", tl.cex = 8, lab_size = 2,
                        ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
                        colors = c("#E46726", "grey95", "#6D9EC1"), title = 'PC1')

corr <- round(cor(pca_matrix_p2,use = "pairwise.complete.obs"), 2)
# Compute a matrix of correlation p-values
p.mat <- round(cor_pmat.hd(pca_matrix_p2), 2)

# Correlation plot at p < 0.05 sig
p1.corrPC2 = ggcorrplot(corr, type = "upper", hc.order = F,
                    lab = TRUE, p.mat = p.mat, insig = "blank",
                    outline.col = "white", tl.cex = 8, lab_size = 2,
                    ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
                    colors = c("#E46726", "grey95", "#6D9EC1"), title = 'PC2')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SOUTHERN LAKES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fig 2: PCA loading
dat_pca = dat %>% filter(lakeid %in% c('ME','MO','FI','WI')) |> 
  filter(metric %in% vars_order) %>% 
  mutate(lakeid = factor(lakeid, levels = lakes_order),
         metric = factor(metric, levels = rev(vars_order), labels = rev(vars_label))) %>% 
  select(lakeid, year, metric, daynum_fill) %>% 
  pivot_wider(names_from = "metric", values_from="daynum_fill") %>% 
  filter(lakeid != "WI")

pca = prcomp(dat_pca[, 3:ncol(dat_pca)], scale=T, center=T)
lakeid_pca = lakes_order[!(lakes_order %in% c("", "WI"))]

# fviz_eig(pca)
p2 = fviz_pca_var(pca,col.var = "contrib", # Color by contributions to the PC
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                             repel = TRUE,     # Avoid text overlapping
                             arrowsize=1,
                             labelsize=3) +
  labs(x="PC 1", y="PC 2", title="", color="Contribution") +
  theme_minimal(base_size = 9) +
  theme(legend.position = 'none')

# Fig 3: PCA correlations
var = get_pca_var(pca)
inds = get_pca_ind(pca)

pca_res = dat_pca[, c("lakeid", "year")]
pca_res = pca_res |> bind_cols(pca$x[,1:4])

pca_res_cor = pca_res %>% 
  pivot_longer(cols = c("PC1", "PC2", "PC3", "PC4")) %>% 
  pivot_wider(names_from = "lakeid", values_from="value") %>% 
  arrange(name, year)

pca_matrix_p1 = pca_res_cor %>% 
  filter(name == "PC1") %>% 
  select(-year, -name)

pca_matrix_p2 = pca_res_cor %>% 
  filter(name == "PC2") %>% 
  select(-year, -name)

###### Compute a correlation matrix ######
corr <- round(cor(pca_matrix_p1,use = "pairwise.complete.obs"), 2)
# Compute a matrix of correlation p-values
p.mat <- round(cor_pmat.hd(pca_matrix_p1), 2)

# Correlation plot at p < 0.05 sig
p2.corrPC1 = ggcorrplot(corr, type = "upper", hc.order = F,
                        lab = TRUE, p.mat = p.mat, insig = "blank",
                        outline.col = "white", tl.cex = 8, lab_size = 2,
                        ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
                        colors = c("#E46726", "grey95", "#6D9EC1"), title = 'PC1')

corr <- round(cor(pca_matrix_p2,use = "pairwise.complete.obs"), 2)
# Compute a matrix of correlation p-values
p.mat <- round(cor_pmat.hd(pca_matrix_p2), 2)

# Correlation plot at p < 0.05 sig
p2.corrPC2 = ggcorrplot(corr, type = "upper", hc.order = F,
                        lab = TRUE, p.mat = p.mat, insig = "blank",
                        outline.col = "white", tl.cex = 8, lab_size = 2,
                        ggtheme = ggplot2::theme_bw(base_size = 9), sig.level = 0.05,
                        colors = c("#E46726", "grey95", "#6D9EC1"), title = 'PC2')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

layout <- "
AC
AD
BE
BF
"
p1 + p2 + p1.corrPC1 + p2.corrPC1 + p1.corrPC2 + p2.corrPC2 + 
  plot_layout(design = layout)

p1 + p2

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

