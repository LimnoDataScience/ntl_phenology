# packages needed for these targets
tar_option_set(packages = c("lubridate",
                            "tidyverse",
                            "cardidates",
                            "scales",
                            "MetBrewer",
                            "ggridges",
                            "patchwork",
                            "ggcorrplot",
                            "corrr",
                            "rstatix",
                            "ggtext",
                            "TraMineR"))

source("src/3_figures/Figure1_weibullFigs.R")
source("src/3_figures/Figure2_tile.R")
source("src/3_figures/Figure2_ggridges.R")
source("src/3_figures/Figure3_betweenLake.R")
source("src/3_figures/Figure4_sequences2.R")
source("src/3_figures/FigureSI_WithinLake.R")
source("src/3_figures/FigureSI_MK.R")
source("src/3_figures/FigureSI_PEGyears.R")
source("src/3_figures/FigureSI_histFit.R")


p3_targets_list <- list(
  tar_target(
    name = figure1_png,
    figure1(path_out = "Figures_manuscript/Figure1_weibull.png")
  ),
  tar_target(
    name = figure2_tile_png,
    figure2_tile(path_in = combine_final_files_csv,
                 path_out = "Figures_manuscript/FigureSI_tileall.png",
                 path_out2 = "Figures_manuscript/Figure2_tile.png")
  ),
  tar_target(
    name = figure2_pdf,
    figure2(path_in = combine_final_files_csv,
            path_out = "Figures_manuscript/Figure2_ridges.pdf")
  ),
  tar_target(
    name = figure3_png,
    figure3(path_in = combine_final_files_csv,
            path_out = "Figures_manuscript/Figure3_betweenLakes.png",
            path_out2 = 'Figures_manuscript/FigureSI_lakePairs.png')
  ),
  tar_target(
    name = figure4_png,
    figure4(path_in = combine_final_files_csv,
            path_out = "Figures_manuscript/Figure4_sequences.png")
  ),

  tar_target(
    name = figureSI_withinLake_png,
    figureSI_withinLake(path_in = combine_final_files_csv,
            path_out = "Figures_manuscript/FigureSI_withinLake.png")
  ),
  tar_target(
    name = figureSI_MK_png,
    figureSI_MK(path_in = combine_final_files_csv,
                path_out = "Figures_manuscript/FigureSI_MK.png")
  ),
  tar_target(
    name = figureSI_PEGyears_png,
    figureSI_PEG(path_in = combine_final_files_csv,
                path_out = 'Figures_manuscript/FigureSI_PEGyears.png')
  ),
  tar_target(
    name = figureSI_histFit_png,
    figureSI_histFit(path_in = combine_final_files_csv,
                 path_out = 'Figures_manuscript/FigureSI_histFit.png')
  )

)