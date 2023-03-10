
icedates <- function(path_out) {
  # Package ID: knb-lter-ntl.33.36 Cataloging System:https://pasta.edirepository.org.
  # Data set title: North Temperate Lakes LTER: Ice Duration - Madison Lakes Area 1853 - current.
  
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/33/36/f5bc02452cafcd461c49bd7429d8b40c" 
  infile1 <- tempfile()
  download.file(inUrl1,infile1,method="auto")
  
  iceS <- read_csv(infile1) |> filter(year4 > 1990) |> 
    select(lakeid, ice_on, ice_off) 
  
  # Make fish ice data the same as Monona
  fish = iceS |> filter(lakeid == 'MO') |> mutate(lakeid = 'FI')
  
  # Package ID: knb-lter-ntl.32.29 Cataloging System:https://pasta.edirepository.org.
  # Data set title: North Temperate Lakes LTER: Ice Duration - Trout Lake Area 1981 - current.
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/32/29/e57a6b46a237355214844e2c76fa8aa5" 
  infile1 <- tempfile()
  download.file(inUrl1,infile1,method="auto")
  
  iceN <- read_csv(infile1) |> 
    filter(lakeid != 'LR') |> 
    select(lakeid, ice_off = datefirstopen, ice_on = datefirstice)
  
  ### IceOFF ####
  iceOFF = iceN |> bind_rows(iceS) |> bind_rows(fish) |>
    mutate(metric = 'iceoff') |> 
    select(lakeid, metric, sampledate = ice_off) |> 
    filter(!is.na(sampledate)) |> 
    mutate(year = year(sampledate), daynum = yday(sampledate)) 
  
  iceON = iceN |> bind_rows(iceS) |> bind_rows(fish) |>
    mutate(metric = 'iceon') |> 
    select(lakeid, metric, sampledate = ice_on) |> 
    filter(!is.na(sampledate)) |> 
    mutate(year = year(sampledate), daynum = yday(sampledate)) |> 
    mutate(daynum = if_else(month(sampledate) <= 4, daynum + 365, daynum)) |> 
    mutate(year = if_else(month(sampledate) <= 4, year - 1, year))
    
  ice = iceOFF |> bind_rows(iceON) |> 
    mutate(dayWeibull = NA, weibull.r2 = NA)
  
  ### write derived data
  # 'Data/final_metric_files/ice.csv'
  write_csv(ice, file = path_out)
  
  return(path_out)

}