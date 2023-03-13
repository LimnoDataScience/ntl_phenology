# packages needed for these targets
tar_option_set(packages = c("lubridate",
                            "tidyverse",
                            "rLakeAnalyzer",
                            "zoo",
                            "pracma"))

source("src/1_raw_data_process/00_icedates.R")
source("src/1_raw_data_process/01_physics.R")
source("src/1_raw_data_process/02_nutrients.R")
source("src/1_raw_data_process/02_secchi.R")
source("src/1_raw_data_process/02_zoopDensity.R")
# source functions
source("src/Functions/weibullYear.R")


p1_targets_list <- list(
  tar_target(
    name = ice_csv,
    icedates(path_out = "Data/final_metric_files/ice.csv"),
    format = "file"
  ),
  tar_target(
    name = physics_csv,
    physics(path_in = "Data/derived/NTLhypsometry.csv",
            path_out = "Data/final_metric_files/physics.csv",
            path_out_derived = "Data/derived/thermocline.csv"),
    format = "file",
  ),
  tar_target(
    name = nutrients_csv,
    nutrients(ice_file = ice_csv,
            path_out = "Data/final_metric_files/nutrients.csv"),
    format = "file",
  ),
  tar_target(
    name = secchi_csv,
    secchi(ice_file = ice_csv,
           strat_file = physics_csv,
          path_out = "Data/final_metric_files/secchi.csv"),
    format = "file",
  ),
  tar_target(
    name = zoopDensity_csv,
    zoopDensity(path_out = "Data/final_metric_files/zoop_density.csv"),
    format = "file",
  )
)