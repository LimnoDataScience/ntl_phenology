
nutrients <- function(ice_file, path_out) {
  # Updated 2023-01-11
  varsWant = c("doc_epiMax", "totpuf_epiMin", #"totpuf_epiMax", 
               "totpuf_hypoMax", #"totpuf_hypoMin", 
               "drsif_epiMin", "drsif_epiSpringMin",
               "totnuf_epiMin", "totnuf_hypoMax")
  
  #################### FUNCTIONS ####################
  # filtering function - turns outliers into NAs to be removed
  filter_lims <- function(x){
    # l = quantile(x, probs = 0.001)
    # u = quantile(x, probs = 0.999)
    stdx = sd(x)
    
    u = mean(x, na.rm = T) + 3*stdx
    l = mean(x, na.rm = T) - 3*stdx
    for (i in 1:length(x)){
      x[i] <- ifelse(x[i]>l & x[i]<u, x[i], NA)
    }
    return(x)
  }
  
  #################### LOAD DATA ####################
  inUrl1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/57/802d63a4c35050b09ef6d1e7da3efd3f"
  infile1 <- tempfile()
  download.file(inUrl1, infile1, method = "curl")
  
  # There is an offset between state lab of hygeiene (SLOH) drsif and NLT drsif. 
  # A linear model of sloh ~ ntl has a slope of 2.28 with r2 of 0.94. 
  LTERnutrients <- read_csv(infile1) |> 
    mutate(drsif = 2.28 * drsif)
  
  # removed flagged data
  lternuts.flagged = LTERnutrients %>%
    mutate(across(everything(), ~replace(., .<0 , NA))) %>%
    rename_all( ~ str_replace(., "_sloh", '.sloh')) %>%
    rename_all( ~ str_replace(., "_n", '.n')) %>%
    rename_at(vars(ph:drsif.sloh), ~ str_c("value_",.)) %>%
    rename_at(vars(flagdepth:flagdrsif.sloh), ~ str_c("error_",.)) %>%
    rename_all(~str_replace_all(.,"flag","")) %>%
    pivot_longer(-(lakeid:event), names_to = c('.value','item'), names_sep = '_') %>%
    filter(!is.na(value) & value>= 0) %>%
    # filter(!str_detect(error,'A|K|L|H|Q') | is.na(error)) %>%
    filter(!str_detect(error,'A|K|H') | is.na(error)) %>% #Removed L and Q
    dplyr::select(-error) %>% 
    mutate(value = case_when(str_detect(item, ".sloh") ~ value*1000, #change sloh from mg to µg
                             TRUE ~ value)) %>% 
    mutate(item = case_when(str_detect(item, ".sloh") ~  str_remove(item, ".sloh"),
                            TRUE ~ item))
  
  # Exclude outliers based on statistics. Remove < 5th and > 95th percentile
  lternuts.flagged = lternuts.flagged |> 
    group_by(lakeid, item) |> 
    mutate(value = filter_lims(value)) |> 
    ungroup() |> 
    filter(!is.na(value))

  # get ice on/off dates
  ice0 = read_csv(ice_file) |> 
    filter(metric == 'iceoff') |> 
    dplyr::select(lakeid, year4 = year, lastice = sampledate)
  
  # Which depths to use? 
  maxDepths = lternuts.flagged |> 
    group_by(lakeid, depth) %>% tally() %>% 
    filter(if_else(lakeid %in% c('ME',"MO","WI","FI"), n >= 2500, n>= 4000)) %>% 
    group_by(lakeid) |> 
    filter(depth == max(depth)) |> 
    rename(maxDepth = depth)
  
  # Limit to surface or bottom and exclude years with < 8 measurements in that year
  surfNuts = lternuts.flagged |> filter(depth <= 1) |> 
    group_by(lakeid, year4, daynum, sampledate, item) |> 
    summarise(value = mean(value, na.rm = T)) |> 
    mutate(layer = 'surf') |> 
    group_by(lakeid, year4, item) |> 
    filter(n() >= 8)
    
  botNuts = lternuts.flagged |> left_join(maxDepths) |> 
    dplyr::filter(depth == maxDepth) |> 
    group_by(lakeid, year4, daynum, sampledate, item) |> 
    summarise(value = mean(value, na.rm = T)) |> 
    mutate(layer = 'bot') |> 
    group_by(lakeid, year4, item) |> 
    filter(n() >= 8)
  
  #################### MANIPULATE DATA ####################
  
  # restrict to epi/hypo and stratification period
  nuts = surfNuts %>% bind_rows(botNuts) |> 
    left_join(ice0) |> 
    filter(sampledate > lastice)
  
  # find day min or max values
  epi_min = nuts %>% 
    filter(layer == 'surf') |> 
    group_by(lakeid, year4, item) %>% 
    slice_min(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMin")) %>% 
    ungroup() %>% 
    dplyr::select(lakeid, metric, sampledate, year4, daynum)
  
  epi_max = nuts %>% 
    filter(layer == 'surf') |> 
    group_by(lakeid, year4, item) %>% 
    slice_max(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_epiMax")) %>% 
    ungroup() %>% 
    dplyr::select(lakeid, metric, sampledate, year4, daynum)
  
  hypo_min = nuts %>% 
    filter(layer == 'bot') |> 
    group_by(lakeid, year4, item) %>% 
    slice_min(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMin")) %>% 
    ungroup() %>% 
    dplyr::select(lakeid, metric, sampledate, year4, daynum)
  
  hypo_max = nuts %>% 
    filter(layer == 'bot') |> 
    group_by(lakeid, year4, item) %>% 
    slice_max(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_hypoMax")) %>% 
    ungroup() %>% 
    dplyr::select(lakeid, metric, sampledate, year4, daynum)
  
  epi_springmin = nuts %>% 
    filter(layer == 'surf') |> 
    filter(yday(sampledate) < 200) |> 
    group_by(lakeid, year4, item) %>% 
    slice_min(value, with_ties = FALSE, n = 1) %>% # if ties, select the first 
    mutate(daynum = yday(sampledate), metric = paste0(item, "_epiSpringMin")) %>% 
    ungroup() %>% 
    dplyr::select(lakeid, metric, sampledate, year4, daynum)
  
  
  comb = bind_rows(epi_min, epi_max, hypo_min, hypo_max, epi_springmin) |> 
    rename(year = year4)
  
  # Plot density distributions
  # ggplot(comb) +
  #   geom_density(aes(x = daynum)) +
  #   facet_grid(rows=vars(lakeid), cols=vars(metric))
  
  write_csv(comb %>% filter(metric %in% varsWant), file = path_out)
  
  return(path_out) 
}
