
plotWeibull <- function(df) {
  # fit weibull, estimate new fit for plotting
  res <- fitweibull6(df$daynum, df$secnview)
  fit <- res$fit |> 
    mutate(newy = fweibull6(x, res$p) * res$ymax)
  # r2
  res$r2
  
  ## identify cardinal dates from fitted curves
  smd <- weibullCDW(res, quantile=0.05)
  smd.out = data.frame(x = smd[[1]], y = smd[[2]]* res$ymax)
  
  # Plotting
  p1 = ggplot(df) +
    geom_point(aes(x = daynum, y = secnview)) +
    geom_path(data = fit, aes(x = x, y = newy), color = 'lightblue3') +
    geom_point(data = smd.out, aes(x = x, y = y), fill = 'lightblue3', shape = 21, stroke = 0.2, size = 3) +
    annotate(geom = 'text', x = 5, y = Inf, 
             label = paste0('r^2 == ', round(res$r2, 2)), hjust = 0, vjust = 2, parse = TRUE, color = 'lightblue4', size = 3) +
    ylab('Secchi (m)') + 
    theme_bw(base_size = 9)
  
  return(p1)

}

dat |> filter(metric == 'secchi_max') |> arrange(weibull.r2) |> filter(weibull.r2 > 0.5)

p1 = plotWeibull(secchi |> filter(lakeid == 'ME', year == 2012)) + labs(title = 'Lake Mendota, 2012')
p2 = plotWeibull(secchi |> filter(lakeid == 'TB', year == 2019)) + labs(title = 'Trout Bog, 2019')
p3 = plotWeibull(secchi |> filter(lakeid == 'CR', year == 1984)) + labs(title = 'Crystal Lake, 1984')

plotWeibull(nuts |> filter(layer == 'surf', item == 'drsif') |> 
              filter(lakeid == 'TR', year == 1985)) + labs(title = 'Crystal Lake, 1984')

plotWeibull(secchi |> filter(lakeid == 'CB', year == 1998)) + labs(title = 'Crystal Lake, 1984')

# patchwork
p1/p3/p2

