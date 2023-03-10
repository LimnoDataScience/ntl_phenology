# remove everything from workspace
rm(list = ls())

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)
library(zoo)
library(patchwork)
library(gganimate)
library(ggridges)
library(pracma)
library(scales)
library(Matrix)
library(RColorBrewer)

source('coolBlueHotRed.R')
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

df <- read_csv('../../Data/analysis_ready/final_combined_dates_filled_v2.csv')

str(df)

library(kohonen)

# df.s <- df %>%
#   filter(variable%in%c('straton', 'stratoff', 'energy', 'stability','anoxia_summer','daphnia','iceon',
#                       'iceoff', 'chla'))
# 
# df.wide <- pivot_wider(df.s, names_from = variable,
#                        values_from = value)
# 
# 
# idx <- df.wide[complete.cases(df.wide),]
# 
# df.matrix <- as.matrix(idx[c('straton', 'stratoff', 'energy', 'stability','anoxia_summer','daphnia','iceon',
#                                  'iceoff', 'chla')])


dates = df# %>% 
 # filter(metric %in% c('straton', 'stratoff', 'energy', 'stability', 'anoxia',
 #                        'iceon', 'iceoff', 'daphnia', 'chla', 'doc')) 

dates_wide = dates %>% 
  pivot_wider(id_cols = c("lakeid", "year"), names_from = "metric", values_from = "daynum_fill")

dates_wide_complete = na.omit(dates_wide)

unique(dates_wide_complete$lakeid)

vars_analyze = c("iceoff", "straton",  "chlor_spring", "secchi_openwater", "daphnia_biomass", "doc_epiMax", "totpuf_hypoMin",  "totpuf_epiMax", "anoxia_summer", "stability", "energy", "totpuf_epiMin", "totpuf_hypoMax", "stratoff", "iceon")

df.matrix = as.matrix(dates_wide_complete %>%
  # select(c('chlor_fall','chlor_spring','secchi_openwater','daphnia_biomass',
  #          'total_zoop_biomass','straton', 'stratoff', 'energy', 'stability', 'anoxia_summer',
  #         'iceon', 'iceoff', 'doc')))
    select(vars_analyze))

# names <- paste0(idx$id)#,'_',df.wide$year)
names <- paste0(dates_wide_complete$lakeid)
n.colors <- c(rep(col_vector[1],sum(!is.na(match(names, 'AL')))),
              rep(col_vector[2],sum(!is.na(match(names, 'BM')))),
              rep(col_vector[3],sum(!is.na(match(names, 'CB')))),
              rep(col_vector[4],sum(!is.na(match(names, 'CR')))),
              rep(col_vector[5],sum(!is.na(match(names, 'FI')))),
              rep(col_vector[6],sum(!is.na(match(names, 'ME')))),
              rep(col_vector[7],sum(!is.na(match(names, 'MO')))),
              rep(col_vector[8],sum(!is.na(match(names, 'SP')))),
              rep(col_vector[9],sum(!is.na(match(names, 'TB')))),
              rep(col_vector[10],sum(!is.na(match(names, 'TR')))),
              rep(col_vector[11],sum(!is.na(match(names, 'WI')))))

set.seed(222)
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular" )

map <- som(df.matrix,
           grid = g,
           alpha = c(0.05, 0.01),
           radius = 1)

plot(map, type='codes',palette.name = rainbow, labels = names)

plot(map, type='mapping',col = n.colors,
     label = names,pchs = names)

#https://www.r-bloggers.com/self-organising-maps-for-customer-segmentation-using-r/



data_train_matrix <- as.matrix(scale(df.matrix))

som_grid <- somgrid(xdim =7, ydim=7, topo="hexagonal")

som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

layout(1)
plot(som_model, type="changes")

plot(som_model, type="count")

plot(som_model, type="codes")

plot(som_model, type="mapping",col = n.colors,
     label = names,pchs = names)

layout(matrix(1:4,ncol=2,nrow=2,byrow=T))

# for(i in 1:13){
#   plot(som_model, type = "property", property=som_model$codes[[1]][,i], main=sprintf(
#     c("iceoff", "straton", "secchi_openwater", "daphnia_biomass", "doc", "chlor_spring",  "anoxia_summer", "stability", "energy", "stratoff", "iceon")[i]), palette.name=coolBlueHotRed)
# }

vars_plot = c("iceoff", "secchi_openwater", "anoxia_summer", "iceon")
vars_lab = c("ice off", "secchi", "anoxia", "ice on")
for(i in 1:4){
  par(cex.main=2)
  var <- vars_plot[i] #define the variable to plot 
  var_unscaled <- aggregate(as.numeric(df.matrix[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
  plot(som_model, type = "property", property=var_unscaled, 
       main=sprintf(
         vars_lab[i]), 
       palette.name=coolBlueHotRed, cex=1.75)
}
# 
# for(i in 1:13){
#   plot(som_model, type = "property", property=som_model$codes[[1]][,i], main=sprintf(
#     c('chlor_fall','chlor_spring','secchi_openwater','daphnia_biomass',
#       'total_zoop_biomass','straton', 'stratoff', 'energy', 'stability', 'anoxia_summer',
#       'iceon', 'iceoff', 'doc')[i]), palette.name=coolBlueHotRed)
# }
# 
# for(i in 1:12){
#   var <- i #define the variable to plot 
#   var_unscaled <- aggregate(as.numeric(df.matrix[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
#   plot(som_model, type = "property", property=var_unscaled, 
#        main=sprintf(
#          c('chlor_fall','chlor_spring','secchi_openwater','daphnia_biomass',
#            'total_zoop_biomass','straton', 'stratoff', 'energy', 'stability', 'anoxia_summer',
#            'iceon', 'iceoff', 'doc')[i]), 
#        palette.name=coolBlueHotRed)
# }

dev.off()

mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata[[1]],2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata[[1]], centers=i)$withinss)
}
plot(wss)

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(som_model$codes[[1]])),4)

plot(hclust(dist(som_model$codes[[1]])))

# plot these results:
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', "#7f7f7f", "#bcbd22", "#17becf","#00FF00", "#9F81F7"
                    ,"#FFFF00", "#F6CECE","#610B21")
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters",
       label = names,pchs = names, cex =0.7) 
add.cluster.boundaries(som_model, som_cluster)

pch.names = as.numeric(as.factor(names))

plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters",
     pchs = pch.names) 
add.cluster.boundaries(som_model, som_cluster)

typ.names = names
typ.names[typ.names%in%c("ME", "MO", "FI")] = 'S'
typ.names[typ.names %in% c('AL','TR')] ='Ndr'
typ.names[typ.names%in%c('BM','CR','SP')]='Ngw'
typ.names[typ.names%in%c('CB','TB')]='Nb'

plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters",
     pchs = typ.names, labels = typ.names) 
add.cluster.boundaries(som_model, som_cluster)

plot(som_model, type="codes", bgcol = pretty_palette[som_cluster])
add.cluster.boundaries(som_model, som_cluster)

