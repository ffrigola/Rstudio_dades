taula2 = tab2 %>% 
  mutate(
    power = current*380*sqrt(3)/1000,
    datetime = as_datetime(timestamp/1000),
    datetime = round_date(datetime, 'minute'),
  ) %>% 
  select(id, datetime, power)

ggplot(taula2)

taula2 %>% 
  ggplot(aes(x = datetime, y = power, color = id)) +
  geom_line()

ggplotly(taula2)
