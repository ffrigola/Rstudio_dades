library(dutils)
library(lubridate)
library(dplyr)
library(dygraphs)
simulation_load_ara <- simulate_step_load(time_start=today(), time_end = today()+days(1), mins_on = 6, mins_off = 10, power = 3, resolution = 1)

simulation_load_opt <- simulate_step_load(time_start=today(), time_end = today()+days(1), mins_on = 7, mins_off = 17, power = 3, resolution = 1)

dttm_seq <- simulation_load_ara$datetime

left_join(
  simulation_load_ara,
  simulation_load_opt,
  by = 'datetime',
) %>% 
  dyplot(stepPlot = T, fillGraph = T, strokeWidth = 2, xlab='Hora',ylab = "Potència (kW)") %>% 
  dyShading(from = dttm_seq[1], to = dttm_seq[8*60], color = "#e6ffe6") %>%
  dyShading(from = dttm_seq[8*60], to = dttm_seq[9*60], color = "#ffd9b3") %>%
  dyShading(from = dttm_seq[9*60], to = dttm_seq[14*60], color = "#ffe6f2") %>%
  dyShading(from = dttm_seq[14*60], to = dttm_seq[18*60], color = "#ffd9b3") %>%
  dyShading(from = dttm_seq[18*60], to = dttm_seq[22*60], color = "#ffe6f2") %>%
  dyShading(from = dttm_seq[22*60], to = dttm_seq[24*60-1], color = "#ffd9b3") %>% 
  dySeries('power.x', label = "Perfil actual", color = "#5c8a8a") %>% 
  dySeries("power.y", label = "Perfil òptim", color = "black") %>% 
  dyLegend(show = 'always', width = 100)


#Tornem a començar
left_join(
  simulation_load_ara,
  by = 'datetime')
 
 
simulation_load_opt<-simulation_load_opt %>%
  mutate(datetime = round_date(datetime,unit = "hour")) %>%
  group_by(datetime)%>%
  summarise(consum_dia_opt=sum(power/60))


simulation_load_ara<-simulation_load_ara %>%
  mutate(datetime = round_date(datetime,unit = "hour")) %>%
  group_by(datetime)%>%
  summarise(consum_dia_actual=sum(power/60))

simulation<-  left_join(
  simulation_load_ara,
  simulation_load_opt,
  by = 'datetime'
)%>%
  mutate(hour=hour(datetime))

simulation<-  left_join(
    simulation_load_ara %>%
    mutate(datetime = round_date(datetime,unit = "hour")) %>%
    group_by(datetime)%>%
    summarise(consum_dia_actual=sum(power/60))%>%
    mutate(hour=hour(datetime))%>%
    mutate(month=month(datetime)),
    simulation_load_opt %>%
    mutate(datetime = round_date(datetime,unit = "hour")) %>%
    group_by(datetime)%>%
    summarise(consum_dia_opt=sum(power/60))%>%
    mutate(hour=hour(datetime))%>%
    mutate(month=month(datetime)))%>%
    select(-c(hour.x,hour.y,month.x,month.y))%>%
    mutate(hour=hour(datetime))


 taula_preus_31 <- taula_preus%>%
  filter(month==5)%>%
  filter(wday==2)%>%
 select(-c(preu_excedent,month,wday))

 taula_simulation <-left_join(
   simulation,
   taula_preus_31,
   by='hour')%>%
   mutate(preu_hora_actual=consum_dia_actual*preu_energia)%>%
   mutate(preu_hora_opt=consum_dia_opt*preu_energia)
 
colSums(taula_simulation [, c (6,7)], na.rm = TRUE )



#Dibuix2

  simulation_load_opt %>% 
  dyplot(stepPlot = T, fillGraph = T, strokeWidth = 2, xlab = 'Hores', ylab = "Potència (kW)") %>% 
  dyShading(from = dttm_seq[1], to = dttm_seq[8*60], color = "#e6ffe6") %>%
  dyShading(from = dttm_seq[8*60], to = dttm_seq[9*60], color = "#ffd9b3") %>%
  dyShading(from = dttm_seq[9*60], to = dttm_seq[14*60], color = "#ffe6f2") %>%
  dyShading(from = dttm_seq[14*60], to = dttm_seq[18*60], color = "#ffd9b3") %>%
  dyShading(from = dttm_seq[18*60], to = dttm_seq[22*60], color = "#ffe6f2") %>%
  dyShading(from = dttm_seq[22*60], to = dttm_seq[24*60-1], color = "#ffd9b3") %>% 
  dySeries('power', label = "Perfil òptim de consum", color = "black") %>% 
  dyLegend(show = 'always', width = 100)

 