#Gràfics congelador prova 6ºc Histeresis
taulacongelador <- taula2 %>% 
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/05/19')%>% 
  filter(datetime < '2022/05/21')%>% 
  mutate(datetime=with_tz(datetime,tzone="Europe/Madrid"))

p <- ggplot(taulacongelador,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)


taulacongelador %>% 
  select(datetime, power) %>% 
  dyplot(xlab="Hora",ylab = "Energia (kWh)", fillGraph = T, strokeWidth = 2, stepPlot = F)

taulacongelador %>% 
  select(datetime, power) %>% 
  dyplot(xlab="Dia i mes",ylab = "Energia (kWh)", fillGraph = T, strokeWidth = 2, stepPlot = F) %>% 
  dyLegend(show = "always", width = 150)

taulacongeladorExperiment2 <- taula2 %>% 
  filter(id == 'ffffb848' ) %>% 
  filter(datetime == '2022/05/30')%>% 
  mutate(datetime=with_tz(datetime,tzone="Europe/Madrid"))

p <- ggplot(taulacongeladorExperiment2,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)
