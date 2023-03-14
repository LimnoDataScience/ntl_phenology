# packages needed for these targets
tar_option_set(packages = c("lubridate",
                            "tidyverse",
                            "scales",
                            "MetBrewer",
                            "ggridges",
                            "patchwork",
                            "ggcorrplot",
                            "corrr",
                            "rstatix"))

# source("src/3_figures/Figure1_ggridges.R")
source("src/3_figures/Figure1_ggridges_v2.R")
source("src/3_figures/Figure2_PEGmodel.R")
source("src/3_figures/Figure3_betweenLake.R")
source("src/3_figures/FigureSI_WithinLake.R")
source("src/3_figures/FigureSI_MK.R")
source("src/3_figures/FigureSI_tile.R")

p3_targets_list <- list(
  # tar_target(
  #   name = figure1_png,
  #   figure1(path_in = 'Data/analysis_ready/final_combined_dates_filled_v2.csv',
  #     path_out = "Figures_manuscript/Figure1.png")
  # ),
  
  tar_target(name = vars_order2, c("iceoff", "straton", "stability", "energy","stratoff", "iceon",
                                   "drsif_surfMin", "nh4_surfMin", "no3no2_surfMin", 'totpuf_surfMin', 'doc_surfMax',
                                   "minimum_oxygen", "secchi_max", "secchi_springmax")),
  tar_target(name = vars_labels2, c("Ice off", "Strat onset", "Stability", "Energy", 'Strat offset','Ice on',
                                    'Si surf min', 'NH4 surf min', 'NO3 surf min', 'TP surf min', 'DOC surf max',
                                    'Oxygen min', 'Secchi max', 'Secchi spring max')),
  tar_target(
    name = figure1_v2_png,
    figure1_v2(path_in = combine_final_files_csv,
            path_out = "Figures_manuscript/Figure1_v2.pdf")
  ),
  # tar_target(
  #   name = figure2_png,
  #   figure2(path_in = combine_final_files_csv,
  #           path_out = "Figures_manuscript/Figure2.pdf",
  #           path_out2 = "Figures_manuscript/FigureSI_histograms.png")
  # ),
  tar_target(
    name = figure3_png,
    figure3(path_in = combine_final_files_csv,
            path_out = "Figures_manuscript/Figure3.png",
            path_out2 = 'Figures_manuscript/FigureSI_lakePairs.png')
  ),
  tar_target(
    name = figureSI_tile_png,
    figureSI_tile(path_in = combine_final_files_csv,
                path_out = "Figures_manuscript/FigureSI_tile.png")
  ),
  tar_target(
    name = figureSI_withinLake_png,
    figureSI_withinLake(path_in = combine_final_files_csv,
            path_out = "Figures_manuscript/FigureSI_withinLake.png")
  ),
  tar_target(
    name = figureSI_MK_png,
    figureSI_MK(path_in = combine_final_files_csv,
                path_out = "Figures_manuscript/FigureSI_MK.png",
                vars_order = vars_order2,
                vars_labels = vars_labels2)
  )

)