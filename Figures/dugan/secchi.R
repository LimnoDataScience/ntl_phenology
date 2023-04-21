
library(NTLlakeloads)
library(tidyverse)

# data<-loadLTERsecchi()

inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/30/5a5a5606737d760b61c43bc59460ccc9"
infile1 <- tempfile()
download.file(inUrl1, infile1, method = "libcurl")
LTERsecchi <- read_csv(infile1, skip = 1, quote = "\"", guess_max = 1e+05, 
                       col_names = c("lakeid", "year4", "daynum", "sampledate", 
                                     "sta", "secview", "secnview", "timeon", "timeoff", 
                                     "airtemp", "windir", "windspd", "waveht", "cloud", 
                                     "ice"))

secchiow = read_csv("Data/final_metric_files/secchi.csv") |> 
  filter(metric == 'secchi_openwater')

##### Check for trends ######
ggplot(secchiow) +
  geom_point(aes(x = year, y = daynum)) +
  geom_smooth(aes(x = year, y = daynum)) +
  facet_wrap(~lakeid) +
  theme_bw(base_size = 9)


###################### 
# plot by DOY
ggplot(data = LTERsecchi |> filter(!is.na(secnview))) +
  geom_point(aes(x=daynum, y=secnview, col = as.factor(year4)), size = 1) +
  geom_line(aes(x=daynum, y=secnview, col = as.factor(year4))) +
  geom_boxplot(data = secchiow, aes(x = daynum, y = -1), size = 0.2, fill = 'grey70', outlier.size = 0.6) +
  # geom_jitter(data = out_secchiPeaks_ow, aes(x = daynum, y = -1, col = as.factor(year4)), width = 0, height = 0.) +
  scale_color_manual(values = met.brewer('Homer1', n = 40), name = 'Year') +
  facet_wrap(~lakeid, scales="free") +
  theme_bw(base_size = 9) +
  theme(legend.position = 'bottom')

ggsave('Figures/dugan/secchi.png', width = 6.5, height = 8, dpi = 500)
