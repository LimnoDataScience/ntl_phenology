
secchi <- function(ice_file, path_out) {
  # Updated 2023-02-02 - HD
  
  #################### LOAD DATA ####################
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
  
  secchi = LTERsecchi |> select(lakeid:sampledate, secnview, ice) |> 
    filter(!is.na(secnview)) |> 
    left_join(iceOff) |> 
    filter(sampledate > lastice) |> 
    left_join(iceOn) |> 
    filter(daynum < firstice) |>  # using daynum because ice-on can switch years
    group_by(lakeid, year4) |> 
    mutate(n = n()) |> 
    filter(!n < 10) |> # filter out low year
    ungroup() |> select(-n) |> 
    rename(year = year4)
  
  
  yearCombos <- crossing(lakeid = unique(secchi$lakeid), year = unique(secchi$year))
  
  # Openwater Secchi Maximum 
  s.openwater.max = secchi |> 
    group_by(lakeid, year) %>% 
    slice_max(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(metric = "secchi_max") %>% 
    select(lakeid, metric, sampledate, year, daynum, secnview)
  
  weibull.list = purrr::map2(yearCombos$lakeid, yearCombos$year, weibull, df = secchi, var = 'secnview', find = 'max')
  weibull.df = do.call(rbind.data.frame, weibull.list)
  
  s.openwater.max = s.openwater.max |> left_join(weibull.df)
  
  # Openwater Secchi Minimum 
  s.openwater.min = secchi |> 
    group_by(lakeid, year) %>% 
    slice_min(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(metric = "secchi_min") %>% 
    select(lakeid, metric, sampledate, year, daynum, secnview)
  
  weibull.list = purrr::map2(yearCombos$lakeid, yearCombos$year, weibull, df = secchi, var = 'secnview', find = 'min')
  weibull.df = do.call(rbind.data.frame, weibull.list)
  
  s.openwater.min = s.openwater.min |> left_join(weibull.df)
  
  # Spring Secchi Maximum 
  s.spring.max = secchi |> 
    filter(yday(sampledate) < 200) |>
    group_by(lakeid, year) %>% 
    slice_max(secnview, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(metric = "secchi_springmax") %>% 
    select(lakeid, metric, sampledate, year, daynum, secnview)
  
  weibull.list = purrr::map2(yearCombos$lakeid, yearCombos$year, weibull, df = secchi |> filter(yday(sampledate) < 200), 
                             var = 'secnview', find = 'max', datacutoff = 6)
  weibull.df = do.call(rbind.data.frame, weibull.list)
  
  s.spring.max = s.spring.max |> left_join(weibull.df)
  
  # Join datasets
  secchi.out = s.openwater.max |> bind_rows(s.openwater.min) |> 
    bind_rows(s.spring.max) |> 
    select(-secnview) 
  
  write_csv(secchi.out, path_out)

  return(path_out) 
}
  