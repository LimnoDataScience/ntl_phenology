
zoopDensity <- function(path_out) {

  #### Download southern lake zooplankton data from EDI ####
  inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/90/33/5880c7ba184589e239aec9c55f9d313b"
  infile1 <- tempfile()
  download.file(inUrl1, infile1, method = "curl")
  dt1 <- read_csv(infile1)
  
  #### Download northern lake zooplankton data from EDI ####
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/37/36/c4b652eea76cd431ac5fd3562b1837ee" 
  infile1 <- tempfile()
  download.file(inUrl1,infile1,method="curl")
  dt2 <-read_csv(infile1)
  
  # get ice on/off dates
  ice0 = read_csv("Data/final_metric_files/ice.csv") |> 
    filter(metric == 'iceoff') |> 
    dplyr::select(lakeid, year4 = year, lastice = sampledate)
  
  #Combine files
  zoops = dt1 |> dplyr::select(-towdepth) |> 
    bind_rows(dt2) |> 
    left_join(ice0) |> 
    filter(sample_date > lastice) |> 
    mutate(code = floor(species_code/10000), daynum = yday(sample_date)) |>
    mutate(zoopGroup = case_when(code == 1 ~ 'copepod nauplii',
                                 code == 2 ~ 'copepod',
                                 code == 3 ~ 'calanoid',
                                 code == 4 ~ 'harpacticoid',
                                 code == 5 ~ 'cladocera',
                                 code == 6 ~ 'rotifer',
                                 code == 7 ~ 'unknown',
                                 code == 8 ~ 'unknown',
                                 code == 9 ~ 'unknown')) |> 
    filter(code %in% c(2,3,4,5)) |>  # cladocera and copepods
    rename(year = year4, sampledate = sample_date)
    
    
  ####### Zoop function to output day of year and weibull #################
  makeZoop <- function(df, usemetric, max = TRUE, spring = FALSE, usecutoff = 8) {
    
    if(spring == TRUE) {
      df = df |> filter(daynum < 182)
    }
    
    df = df |> group_by(lakeid, year, sampledate, daynum) |> 
      summarize(density = sum(density, na.rm = T)) |> 
      group_by(lakeid, year)
    
    if (max == TRUE) {
      dayMax = df |>  slice_max(density, with_ties = FALSE, n = 1) # if ties, select the first 
      weibullMax = df |> 
        group_modify(~weibull.year(.x, 'density', find = 'max', datacutoff = usecutoff), .keep = TRUE)
    } else {
      dayMax = df |>  slice_min(density, with_ties = FALSE, n = 1) # if ties, select the first 
      weibullMax = df |> 
        group_modify(~weibull.year(.x, 'density', find = 'min', datacutoff = usecutoff), .keep = TRUE)
    }
    
    output = dayMax |> left_join(weibullMax) |> 
      mutate(metric = usemetric) |> 
      select(lakeid, metric, sampledate, year, daynum, dayWeibull, weibull.r2)
    return(output)
  }
  
  # Get doys
  o1 = makeZoop(zoops, 'zoop_max', max = TRUE, spring = FALSE) # zoop max
  o2 = makeZoop(zoops, 'zoop_springmax', max = TRUE, spring = TRUE, usecutoff = 5) # zoop max spring
  
  # Combine datasets 
  zoop.out =  bind_rows(o1, o2) |> 
    select(lakeid, metric, sampledate, year, daynum, dayWeibull, weibull.r2)
  
  # Check for duplicates
  zoop.out |> group_by(lakeid, metric, year) |> filter(n() > 1)
  
  # # Plot check
  # ggplot(zoop.out) + 
  #   geom_density(aes(x = daynum, color = metric)) +
  #   facet_wrap(~lakeid, scales = 'free_y')
  
  write_csv(zoop.out, path_out)
  
 return(path_out) 
}
