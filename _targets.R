# Written by use_targets().
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# main target script for calling all subsequent targets
library(targets)

options(tidyverse.quiet = TRUE)
# options(clustermq.scheduler = "multicore")
options(clustermq.scheduler = "multiprocess")

packages <- c('tidyverse', 
              'lubridate', 
              "rLakeAnalyzer",
              "zoo",
              "pracma")

source("src/1_raw_data_process.R")
source("src/2_final_data_prep.R")
source("src/3_figures.R")
# complete list of targets
c(p1_targets_list, p2_targets_list, p3_targets_list)