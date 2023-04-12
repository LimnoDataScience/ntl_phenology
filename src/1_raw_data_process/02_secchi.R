
secchi <- function(ice_file, strat_file, path_out) {
  # Updated 2023-03-10 - Hilary Dugan
  
  #################### LOAD SECCHI DATA ####################
  inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/29/5a5a5606737d760b61c43bc59460ccc9"
  infile1 <- tempfile()
  download.file(inUrl1, infile1, method = "libcurl")
  LTERsecchi <- read_csv(infile1)
  
  ### Get TSI ###
  LTERsecchi |> filter(month(sampledate) %in% c(7,8)) |> 
    group_by(lakeid) |> 
    summarise(mean.Secchi = mean(secnview, na.rm = T)) |> 
    mutate(TSI.SD = 60-14.41*log(mean.Secchi))
  
  # TSI < 40 Oligotrophic
  # TSI 40-50 Mesotrophic
  # TSI >50 Eutrophic
  
  # get ice on/off dates
  iceOff = read_csv(ice_file) |> 
    filter(metric == 'iceoff') |> 
    select(lakeid, year4 = year, lastice = sampledate)
  
  iceOn = read_csv(ice_file) |> 
    filter(metric == 'iceon') |> 
    select(lakeid, year4 = year, firstice = daynum)
  
  stratOff = read_csv(strat_file) |> 
    filter(metric == 'stratoff') |> 
    select(lakeid, year4 = year, stratoff = daynum)
  
  secchi = LTERsecchi |> select(lakeid:sampledate, secnview, ice) |> 
    filter(!is.na(secnview)) |> 
    left_join(iceOff) |> 
    filter(sampledate > lastice) |> # filter dates after ice off
    left_join(stratOff) |> 
    filter(daynum < stratoff) |>  # filter dates before fall mixing
    left_join(iceOn) |> 
    filter(daynum < firstice) |>  # using daynum because ice-on can switch years
    group_by(lakeid, year4) |> 
    mutate(n = n()) |> 
    filter(!n < 8) |> # filter out low year
    ungroup() |> select(-n) |> 
    rename(year = year4)
  
  ####### Secchi function to output day of year and weibull #################
  makeSecchi <- function(df, usemetric, max = TRUE, spring = FALSE, usecutoff = 8) {
    if(spring == TRUE) {
      df = df |> filter(yday(sampledate) <= 196)
    }
    
    df = df |> group_by(lakeid, year)
    
    if (max == TRUE) {
      dayMax = df |>  slice_max(secnview, with_ties = FALSE, n = 1) # if ties, select the first 
      weibullMax = df |> 
        group_modify(~weibull.year(.x, 'secnview', find = 'max', datacutoff = usecutoff), .keep = TRUE)
    } else {
      dayMax = df |>  slice_min(secnview, with_ties = FALSE, n = 1) # if ties, select the first 
      weibullMax = df |> 
        group_modify(~weibull.year(.x, 'secnview', find = 'min', datacutoff = usecutoff), .keep = TRUE)
    }
    
    output = dayMax |> left_join(weibullMax) |> 
      mutate(metric = usemetric) |> 
      select(lakeid, metric, sampledate, year, daynum, secnview, dayWeibull, weibull.r2, weibull.max, weibull.adjust)
    return(output)
  }
  
  o1 = makeSecchi(df = secchi, usemetric = 'secchi_max', max = TRUE, spring = FALSE) # secchi max
  o2 = makeSecchi(secchi, 'secchi_min', max = FALSE, spring = FALSE) # secchi min
  o3 = makeSecchi(secchi, 'secchi_springmax', max = TRUE, spring = TRUE, usecutoff = 5) # spring secchi min
  
  ####### Join dataset ###### ###### ###### ###### ###### ######
  secchi.out = bind_rows(o1, o2, o3) |> 
    select(-secnview) 
  
  write_csv(secchi.out, path_out)

  return(path_out) 
}
  