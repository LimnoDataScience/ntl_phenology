
##### Paper stats #######
path_in = "Data/final_metric_files/final_combined.csv"
dat = read_csv(path_in) |>  
  mutate(diffDays = abs(daynum - dayWeibull))

# How may fits were adjusted
dat |> 
  mutate(total = n()) |> 
  group_by(weibull.adjust) |> 
  summarise(n = n(), total = first(total), per = n/total)

# How may fits were adjusted in final data
dat |> 
  filter(metric %in% vars_order) |> 
  mutate(total = n()) |> 
  group_by(weibull.adjust) |> 
  summarise(n = n(), total = first(total), per = n/total)

# How many lakes remained in final data
dat |> 
  filter(metric %in% vars_order) |> 
  group_by(lakeid) |>
  mutate(total = n()) |> 
  filter(weibull.r2 > 0.7 | diffDays <= 30) |> 
  summarise(n = n(), total = first(total), kept = n/total, removed = 1 - kept)  


vars_order = c("iceoff", "straton", "energy", "schmidt", "stratoff", "iceon",
               "drsif_springSurfMin", 
               "totnuf_surfMin",
               "totpuf_surfMin", 
               "ph_surfMax",
               "minimum_oxygen", "secchi_max", "zoop_max")

dat |> 
  filter(metric %in% vars_order) |> 
  mutate(total = n()) |> 
  filter(weibull.r2 > 0.7 | diffDays <= 30) |> 
  group_by() |>
  summarise(n = n(), total = first(total), per = n/total)  

