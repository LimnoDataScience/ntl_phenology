df = read_csv('~/Downloads/ntl129_1_v10.csv')
head(df)

df |> filter(year4 == 2020) |> 
  ggplot() + 
  # geom_line(aes(x = sampledate, y = avg_par)) +
  geom_line(aes(x = sampledate, y = avg_par_below/avg_par)) +
  ylim(0,1)

useYear = 2020
a = LTERsecchi |> filter(lakeid == 'ME', year4 == useYear) |> 
  filter(!is.na(secnview))

minTurb = df |> filter(year4 == useYear) |> 
  filter(avg_turbidity == min(avg_turbidity, na.rm = T))

df |> filter(year4 == useYear) |> 
  ggplot() + 
  geom_vline(data = minTurb, aes(xintercept = sampledate), linetype = 3) +
  geom_line(aes(x = sampledate, y = avg_turbidity)) +
  geom_point(data = a, aes(x = sampledate, y = secnview)) +
  geom_line(data = a, aes(x = sampledate, y = secnview), linetype = 2)


