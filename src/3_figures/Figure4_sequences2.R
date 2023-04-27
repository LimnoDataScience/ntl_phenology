library(tidyverse)
library(MetBrewer)
library(TraMineR)
path_in = 'Data/final_metric_files/final_combined.csv'

lakes_order = c("BM", "TR", "CR", "SP", "CB", "TB", "ME", "MO", "AL", "WI")

dat <-  read_csv(path_in)

df <- dat %>%
  filter(lakeid != 'FI') |> 
  mutate(diffDays = abs(daynum - dayWeibull)) |> 
  filter(weibull.r2 > 0.7 | diffDays <= 30) |> 
  filter(metric %in% c('iceoff', 'iceon', 'straton', 'stratoff', 'schmidt', 'minimum_oxygen', 'secchi_max')) |> 
  mutate(metric = factor(metric, levels = c('iceoff', 'straton','secchi_max','schmidt', 'minimum_oxygen', 'stratoff','iceon'))) |> 
  select(lakeid, metric, year, dayWeibull) 

# Rank order of events
df.rank = df |> 
  group_by(lakeid, year) |> 
  arrange(dayWeibull, .by_group = TRUE) |> 
  mutate(rank = data.table::frank(dayWeibull, ties.method = 'average')) |> 
  filter(n() == 7)

# Calculate percent occurence
df.tot = df.rank |> group_by(lakeid) |> 
  arrange(rank, .by_group = TRUE) |> 
  group_by(lakeid, rank, metric) |> 
  summarise(n = n()) |> 
  group_by(lakeid, rank) |> 
  mutate(per = n/sum(n))

# Create a state sequence object
getDiss <- function(df) {
  df.seq = df |> 
  ungroup() |> 
  select(lakeid, year, metric, rank) |> 
  pivot_wider(names_from = rank, values_from = metric) |> 
  select(-lakeid, -year) |> 
  seqdef()
  
  # Generate substitution and indel costs. Constant value of 2. 
  scost <- seqsubm(df.seq, method="CONSTANT", cval=2)
  
  # Distances (dissimilarities) between sequences 
  df.dist = seqdist(df.seq, method = "OM", indel = 1, sm = scost, full.matrix = FALSE)
  df.diss = round(mean(df.dist),2)
  return(data.frame(diss = df.diss))
}

# Apply dissimilarity function across 
df.diss = df.rank |> 
  group_by(lakeid) |> 
  group_modify(~getDiss(.x), .keep = TRUE)
print(df.diss)

df.tot = df.tot |> left_join(df.diss) |> 
  mutate(numYears = sum(n)) |> 
  mutate(lakeid = factor(lakeid, levels = lakes_order)) |> 
  mutate(label = factor(paste0(lakeid,': ',round(mean(diss),2), ' (n = ', numYears,')')))

# Plot sequence figures
ggplot(data = df.tot,
       aes(x = rank, y = 100*per, fill = as.factor(metric))) +
  geom_bar(stat = 'identity') + 
  ylab('Occurrence (%)') +
  scale_fill_manual(values = rev(met.brewer("Redon", 7)), name = 'Metric', 
                    labels = c('Ice-off', 'Strat onset','Secchi max','Stability', 'Oxygen min', 'Strat offset','Ice-on')) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = 1:7, labels = paste0('event ',1:7)) +
  xlab('') +
  facet_wrap(~label, ncol = 2) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = 'bottom',
        legend.margin=margin(t = 0, unit='cm'),
        legend.key.size = unit(0.3,'cm'),
        panel.grid = element_blank(),
        strip.background = element_rect(fill="white", color = 'transparent'),
        strip.text = element_text(angle = 0, hjust = 0)
        ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(filename = 'Figures_manuscript/Figure4_sequences2.png', dpi = 500, width = 6, height = 5)
 


