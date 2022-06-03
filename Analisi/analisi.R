library(lubridate)
library(dplyr)
library(plotly)


taula2 = tab2 %>% 
  mutate(
    power = current*380*sqrt(3)/1000,
    datetime = as_datetime(timestamp/1000),
    datetime = round_date(datetime, 'minute')
  ) %>% 
  select(id, datetime, power)


taulacuina <- taula2 %>% 
  filter(id == '8f5' ) %>% 
  mutate(dia = date(datetime)) %>% 
  mutate(hora = hour(datetime)) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/09 ')%>%
  group_by(dia,hora)

ggplot(taulacuina)

taulacuina<-taula2 %>% 
  filter(id == '8f5' ) %>% 
  filter(datetime > '2022/05/01')%>%
  filter(datetime < '2022/05/07')%>%
  mutate(dia = date(datetime)) %>% 
  mutate(datetime = round_date(datetime, 'hour')) %>% 
  group_by(dia) %>% 
  summarise(consum_dia = sum(power/60))%>% 
  ggplot(aes(dia, consum_dia)) +
  geom_col()
  
ggplotly(taulacuina)

taula2 %>% 
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/17')%>%
  mutate(datetime = round_date(datetime, 'hour')) %>% 
  ggplot(aes(datetime,power)) +
  geom_col()


  
  
tauladepuradora %>% 
  ggplot(aes(x = datetime, y = power, color=id)) +
  geom_line()

ggplotly(tauladepuradora)

plot_ly(data=tauladepuradora,x=datetime,y=power)

tauladepuradora<-taula2 %>% 
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/01')%>%
  filter(datetime < '2022/05/07')%>%
  mutate(dia = date(datetime)) %>% 
  mutate(hora = hour(datetime)) %>% 
  group_by(dia,hora) %>% 
  summarise(consum_dia = sum(power/60))%>% 
  ggplot(aes(dia,hora)) +
  geom_col()

ggplotly(tauladepuradora)

#Gràfics depuradora potència
tauladepuradora <- taula2 %>% 
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/31')  %>%
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))

p <- ggplot(tauladepuradora,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)

#Gràfic depuradora energia
tp2<-taula2%>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/14')%>%
  filter(datetime < '2022/05/16')%>%
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_energia=sum(power/60))%>%
  ggplot(aes(x=dia_hora, y=consum_energia)) +
  geom_col()

ggplotly(tp2)


tp2<-taula2%>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/15')%>%
  filter(datetime < '2022/05/17')%>%
  group_by(datetime)%>% 
  summarise(consum_dia=sum(power/60))%>%
  dutils::dyplot(xlab = datetime, ylab=consum_dia) %>%
  dygraphs::dyBarChart()

#Càlculs energètics depuradora
#Dia
taula2 %>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/06')%>%
  filter(datetime < '2022/05/17')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/10)

#Mes
tp2<-taula2%>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/06')%>%
  filter(datetime < '2022/05/17')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mes=sum(consum_dia)*10)
#Hora
tp2<-taula2%>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/05/06')%>%
  filter(datetime < '2022/05/17')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_hora=sum(consum_dia)/(10*24))


#Gràfics grup d'aigua potència
taula_grupaigua <- taula2 %>% 
  filter(id == '182e' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/24')

p <- ggplot(taula_grupaigua,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)

#Gràfic grup daigua energia
t_ga<-taula2%>%
  filter(id == '182e' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/09')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum=sum(power/60))%>%
  ggplot(aes(x=dia_hora, y=consum)) +
  geom_col()

ggplotly(t_ga)



#Càlculs energètics  grup d'aigua
#Diari
tp2<-taula2%>%
  filter(id == '182e' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/09')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/30)

#Mes
tp2<-taula2%>%
  filter(id == '182e' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/09')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mes=sum(consum_dia))
#Hora
tp2<-taula2%>%
  filter(id == '182e' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/09')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_hora=sum(consum_dia)/(30*24))

#Gràfics refredadora
#Energia
t_ref<-taula2%>%
  filter(id == 'ffffe4f0' ) %>% 
  filter(datetime > '2022/04/17')%>%
  filter(datetime < '2022/04/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_energia=sum(power/60))%>%
  ggplot(aes(x=dia_hora, y=consum_energia)) +
  geom_col()

ggplotly(t_ref)

#Càlculs energètics  refredadora
#Diari
t_ref<-taula2%>%
  filter(id == 'ffffe4f0' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/09')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/30)

#MEs
t_ref<-taula2%>%
  filter(id == 'ffffe4f0' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/17')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mes=sum(consum_dia))

#Hora
t_ref<-taula2%>%
  filter(id == 'ffffe4f0' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/09')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_hora=sum(consum_dia)/(30*24))

#Cuina
#Energia
t_cuina<-taula2%>%
  filter(id == 'ffffe833' ) %>% 
  filter(datetime > '2022/05/09')%>%
  filter(datetime < '2022/05/23')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum=sum(power/60))%>%
  ggplot(aes(x=dia_hora, y=consum)) +
  geom_col()

ggplotly(t_cuina)

#Potència
taula_cuina <- taula2 %>% 
  filter(id == 'ffffe833' ) %>% 
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
  filter(datetime > '2022/05/11')%>%
  filter(datetime < '2022/05/16')

p <- ggplot(taula_cuina,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)

#Calculs energetics
#Diari
t_cuina<-taula2%>%
  filter(id == 'ffffe833' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/18)


#Mes
t_cuina<-taula2%>%
  filter(id == 'ffffe833' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/18*30)

#Hora
#Mes
t_cuina<-taula2%>%
  filter(id == 'ffffe833' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/(18*24))

#INVERSOR
#Energia
t_inv<-taula2%>%
  filter(id == 'ffffb450' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/31')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>%
  ggplot(aes(x=dia_hora, y=consum_dia)) +
  geom_col()

ggplotly(t_inv)

#Potència
taula_inversor <- taula2 %>% 
  filter(id == 'ffffb450' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/26')

p <- ggplot(taula_inversor,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)

#Calculs energetics
#Diari
t_inversor<-taula2%>%
  filter(id == 'ffffe833' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/18*30)

#Congelador
#Energia
t_congelador<-taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/04/07')%>%
  filter(datetime < '2022/05/31')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>%
  ggplot(aes(x=dia_hora, y=consum_dia)) +
  geom_col()

ggplotly(t_congelador)

#Potència
t_congelador <- taula2 %>% 
  filter(id == 'ffffb848' ) %>% 
  mutate(datetime = with_tz(datetime,tzone="Europe/Madrid"))%>%
  filter(datetime > '2022/05/11')%>%
  filter(datetime < '2022/05/16')

p <- ggplot(t_congelador,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)

#Calculs energetics
#Diari
t_congelador<-taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/18)


#Mes
t_congelador<-taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/18*30)

#Hora
#Mes
t_congelador<-taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/05/07')%>%
  filter(datetime < '2022/05/26')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/(18*24))