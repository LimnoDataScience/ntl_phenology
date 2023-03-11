######################################################################
############## Fill Missing Phenology Dates ##############
######################################################################

# vars_in is the vector of variables of interest

missing_dates_fill <- function(path_in, path_out, vars_order) {
    
  # Read in combined files
  dat = read_csv(path_in) |> 
    mutate(lakeid = factor(lakeid, 
            levels = rev(c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "FI", "ME", "MO", "WI"))))

  
  dat$metric = factor(dat$metric,
                      levels = rev(vars_order),
                      ordered=T)
  
  #' # Data
  #' ## Which years are missing a metric
  dat %>% 
    ggplot(aes(year, lakeid, fill=is.na(daynum))) +
    geom_tile(color="grey") +
    theme_bw() +
    facet_wrap(~metric, nrow=6) 
  
  #' 
  #' **Remaining data issues:**
  #' 
  #' * Northern lakes don't have DOC until until 1986; missing first 4 years when all other variables are available. Potential fixes:
  #'   - Exclude DOC; start analyses in 1986; fill DOC with e.g., median value, or **model of other variables**?
  #' * Southern lakes missing chlorophyll data: 1996 - 1999 and 2002 - 2004; 2002-04 is bad/uncalibrated values, used DOY but not magnitude; earlier years fill with median
  #' * Zoop data issues:
  #'   - CB missing daphnia peak 1995(?): **fill with median**
  #'   - FI missing several years from 1996 - 2006; **fill with median**
  #'   
  #' **Predict DOC daynum from other variable daynum's in each northern lake**
  ## ------------------------------------------------------------------------------------------------------------------------
  no_dups = dat %>% group_by(lakeid, year, metric) %>% summarise(N = n()) %>% filter(N == 1)
  
  n_lakes_wide = left_join(no_dups, dat) %>% 
    select(-sampledate) %>% 
    pivot_wider(names_from = metric, values_from = daynum) %>% 
    filter(lakeid %in% c("AL", "BM", "CB", "CR", "SP", "TB", "TR"))
  
  hold_data = na.omit(data.frame(n_lakes_wide %>%  ungroup() %>% select(-year, -N))) # , -doc_predict
  all = lm(doc_epiMax~(iceon+straton+stratoff+energy+
                   stability+minimum_oxygen+
                   secchi_max)*lakeid, data=hold_data)
  i_o = lm(doc_epiMax~1, data=hold_data)
  hold_step = step(i_o, scope=formula(all))
  
  doc_model = lm(doc_epiMax~(iceon+straton+stratoff+energy+
                   stability+minimum_oxygen+
                   secchi_max)*lakeid, 
                 data=n_lakes_wide, 
                 na.action = na.exclude)
  summary(doc_model)
  
  n_lakes_wide$doc_predict = predict(doc_model, n_lakes_wide)
  
  cor = n_lakes_wide %>% 
    group_by(lakeid) %>% 
    summarise(r = paste("r =", round(cor(doc_epiMax, doc_predict, use="complete.obs"), 3)))
  
  n_lakes_wide %>% 
    ggplot(aes(x=doc_epiMax, y=doc_predict, color=as.character(lakeid))) +
    geom_point()+
    theme_bw() +
    labs(color="lakeid")+
    geom_abline(slope=1, intercept = 0) + 
    facet_wrap(~lakeid) + 
    geom_text(aes(label=r), data=cor, x=300, y=0) +
    labs(x="obs peak DOC (daynum)",  y="modeled peak DOC (daynum)")
  
  # not good but vaguely positive relationship; use this instead of median?
  missing_doc = n_lakes_wide %>% filter(is.na(doc_epiMax))
  
  missing_doc$doc_fill = round(predict(doc_model, missing_doc))
  
  doc_fill = missing_doc %>% 
    select(lakeid, year, doc_fill) %>% 
    rename(daynum_fill = doc_fill) %>% 
    mutate(metric = "doc_epiMax") %>% 
    filter(year < 2000)
  
  
  #' ## Create filled metric column and filled
  all_fill = bind_rows(doc_fill)
  dat_filled = full_join(dat, all_fill) %>% 
    mutate(filled_flag = ifelse(is.na(daynum) & !is.na(daynum_fill), TRUE, FALSE)) %>% 
    mutate(daynum_fill = ifelse(is.na(daynum) & !is.na(daynum_fill), daynum_fill, daynum))
  
  dat_filled$lakeid = factor(dat_filled$lakeid, 
                      levels = c("AL", "BM", "CB", "CR", "SP", "TB", "TR", "FI", "ME", "MO", "WI"), 
                      ordered = T)
 
  dat_filled$metric = factor(dat_filled$metric,
                      levels = rev(vars_order),
                      ordered=T)
  
  dat_filled %>% 
    ggplot(aes(year, metric, fill=filled_flag)) +
    geom_tile(color="grey") +
    theme_bw() +
    facet_wrap(~lakeid, nrow=6)
    
  
  #' ## Additional trimming/filling
  #' Trim to "good" years, fill FI ice data with Monona, then fill additional missing 
  #' with median values for each lake/metric
  df_yearsWant = dat_filled %>% 
    filter((lakeid %in% c("FI", "ME", "MO", "WI") & year %in% 1996:2018) |
             (!(lakeid %in% c("FI", "ME", "MO", "WI")) & year %in% 1982:2018))
  
  fi_icefill = df_yearsWant %>% 
    filter(lakeid == "MO" & metric %in% c("iceoff", "iceon")) %>% 
    mutate(lakeid = "FI") %>% 
    select(-daynum, -filled_flag, -sampledate) %>% 
    rename(icefill = daynum_fill)
  
  df_yearsWant = df_yearsWant %>% 
    full_join(fi_icefill) %>% 
    mutate(daynum_fill = ifelse(is.na(daynum_fill) & !is.na(icefill), 
                                icefill, daynum),
           filled_flag = ifelse(is.na(daynum) & !is.na(daynum_fill) & !is.na(icefill), 
                                TRUE, filled_flag))
  
  df_yearsWant %>% 
    ggplot(aes(year, metric, fill=filled_flag)) +
    geom_tile(color="grey") +
    theme_bw() +
    facet_wrap(~lakeid, nrow=6)
  
  #' 
  #' ### Final fill w/ medians
  ## ------------------------------------------------------------------------------------------------------------------------
  all_lys = df_yearsWant %>% 
    select(lakeid, year) %>% 
    distinct()
  
  metric = unique(df_yearsWant$metric)
  
  all_lys_want = expand_grid(all_lys, metric)
  
  dat_final = left_join(all_lys_want, df_yearsWant)
  
  medians = dat_final %>% 
    group_by(lakeid, metric) %>% 
    summarise(median_daynum = median(daynum_fill, na.rm=T))
  
  dat_final = dat_final %>% 
    left_join(medians) %>% 
    mutate(daynum_fill = ifelse(is.na(daynum_fill), median_daynum, daynum_fill)) %>% 
    mutate(filled_flag = ifelse(is.na(daynum) & !is.na(daynum_fill), TRUE, filled_flag)) %>% 
    select(-icefill, -median_daynum)
  
  #' 
  dat_final %>%  
    ggplot(aes(year, metric, fill=filled_flag)) +
    geom_tile(color="grey") +
    theme_bw() +
    facet_wrap(~lakeid, nrow=6)
  
  # Write final output file with filled dates
  write_csv(dat_final, file = path_out)
  
  return(path_out)
}
