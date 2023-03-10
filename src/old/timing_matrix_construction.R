library(tidyverse)

# timings = read_csv("../Data/all_event_timing_differences.csv")

matrix_single_year <- function(timings.df = timings, single_year=2015){
  
  nLakes = length(unique(timings.df$id))
  nVars = length(unique(timings.df$Var1))
  
  cur_year = timings.df %>% 
    filter(year == single_year) %>% 
    select(id, Var1, Var2, doy_diff) %>% 
    arrange(id, Var1, Var2)
  
  
  
  hold = array(unlist(cur_year$doy_diff), dim = c(nVars,nVars,nLakes))
  dimnames(hold) = list(sort(unique(cur_year$Var1)), sort(unique(cur_year$Var2)), sort(unique(cur_year$id)))
  
  return(hold)
}

# test = matrix_single_year()
# test[,,"BM"]


matrix_single_lake <- function(timings.df = timings, single_lake="ME"){
  
  timings.df = timings.df %>% filter(id == single_lake)
  
  nYears = length(unique(timings.df$year))
  nVars = length(unique(timings.df$Var1))
  
  cur_lake = timings.df %>% 
    filter(id == single_lake) %>% 
    select(year, Var1, Var2, doy_diff) %>% 
    arrange(year, Var1, Var2)
  
  hold = array(unlist(cur_lake$doy_diff), dim = c(nVars,nVars,nYears))
  dimnames(hold) = list(sort(unique(cur_lake$Var1)), sort(unique(cur_lake$Var2)), sort(unique(cur_lake$year)))
  
  return(hold)
}

# test2 = matrix_single_lake(timings.df = timings, single_lake = "ME")
# test2[,,"2015"]

