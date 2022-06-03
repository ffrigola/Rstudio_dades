#Depuradora
tauladepuradora<-taula2%>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/04/08')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(datetime = round_date(datetime,unit = "hour"))%>% 
  group_by(datetime)


Tperfil<-taula2 %>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/31')%>%
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
  mutate(hora = hour(datetime))%>% 
  group_by(hora)%>% 
  summarise(consum_mig_hora=sum(power/60)/23)%>% 
  ggplot(aes(x=hora, y=consum_mig_hora)) +
  geom_col()

ggplotly(Tperfil)

#Cuina
taula_cuina<-taula2%>%
  filter(id == 'ffffe833' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
  mutate(hora = hour(datetime))%>% 
  mutate(wday=wday(datetime))%>%
  filter(wday == 1 | wday>3)%>%
  group_by(hora)%>%
  summarise(consum_mig_hora=sum(power/60)/12)%>% 
  ggplot(aes(x=hora, y=consum_mig_hora)) +
  geom_col()

ggplotly(taula_cuina)

#Congelador
t_congelador<-taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/04/08')%>%
  filter(datetime < '2022/05/31')%>%
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
  mutate(hora = hour(datetime))%>% 
  group_by(hora)%>%
  summarise(consum_mig_hora=sum(power/60)/53)%>% 
  ggplot(aes(x=hora, y=consum_mig_hora)) +
  geom_col()

ggplotly(t_congelador)

#Inversor
t_inv<-taula2%>%
  filter(id == 'ffffb450' ) %>% 
  filter(datetime > '2022/04/08')%>%
  filter(datetime < '2022/05/03')%>%
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
  mutate(hora = hour(datetime))%>% 
  group_by(hora)%>%
  summarise(consum_mig_hora=sum(power/60)/24)%>% 
  ggplot(aes(x=hora, y=consum_mig_hora)) +
  geom_col()

ggplotly(t_inv)


#Ajuntar perfils diaris
Tperfil<-taula2 %>%
 filter(id %in% c('ffffe833', '3cd6'))%>%
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
 mutate(hora = hour(datetime))%>% 
   group_by(id, hora)%>%
  summarise(consum_mig_hora = mean(power))%>%
  ggplot(aes(hora, consum_mig_hora, fill=id))+
   geom_col()

 ggplotly(Tperfil)

