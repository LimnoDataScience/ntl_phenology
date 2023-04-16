######################################################################
############## Read in and combine final phenology dates ##############
######################################################################

combine_final_files <- function(ice_file, physics_file, nutrients_file, secchi_file, zoopDensity_file, 
                                vars_order, path_out) {

  dfs = lapply(c(ice_file, physics_file, nutrients_file, secchi_file, zoopDensity_file), read_csv)
  
  comb_data = bind_rows(dfs)
  
  comb_data_out = comb_data %>%
     filter((lakeid %in% c("AL", "BM", "MB", "CB", "CR", "SP", "TB", "TR") & year >= 1982) |
              (lakeid %in% c("ME", "MO", "WI", "FI") & year >= 1996)) |> 
     filter(metric %in% vars_order)
  
  comb_data_out = comb_data_out |> 
    mutate(dayWeibull = if_else(metric %in% c('iceoff','iceon'), daynum, dayWeibull)) |> 
    mutate(weibull.max = if_else(metric %in% c('iceoff','iceon'), TRUE, weibull.max)) |> 
    mutate(weibull.r2 = if_else(metric %in% c('iceoff','iceon'), 1, weibull.r2))
  
  write_csv(comb_data_out, file = path_out)
  return(path_out)
  
}
