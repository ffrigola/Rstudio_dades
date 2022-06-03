#Càlculs energètics congelador
#Dia
taula2 %>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/10')%>%
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mig=sum(consum_dia)/31)
#Mes
taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/10')%>% 
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_mes=sum(consum_dia))

#Hora
taula2%>%
  filter(id == '3cd6' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/10')%>% 
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_dia=sum(power/60))%>% 
  summarise(consum_hora=sum(consum_dia)/(31*24))


#Gràfic congelador energia diari
tp2<-taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/05/10')%>% 
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>%
  group_by(dia_hora)%>% 
  summarise(consum_hora=sum(power/60)) %>%
  mutate(dia = date(dia_hora)) %>% 
  group_by(dia) %>% 
  summarise(consum_dia = sum(consum_hora)) %>% 
  ggplot(aes(x=dia, y=consum_dia)) +
  geom_col()

ggplotly(tp2)

#Gràfic congelador energia hora
tp2<-taula2%>%
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/04/17')%>% 
  mutate(dia_hora = round_date(datetime,unit = "hour"))%>% 
  group_by(dia_hora)%>% 
  summarise(consum_hora=sum(power/60))%>%
  ggplot(aes(x=dia_hora, y=consum_hora)) +
  geom_col()

ggplotly(tp2)

#Gràfics congelador potència
taulacong <- taula2 %>% 
  filter(id == 'ffffb848' ) %>% 
  filter(datetime > '2022/04/09')%>%
  filter(datetime < '2022/04/17') 

p <- ggplot(taulacong,mapping = aes(x=datetime,y=power,color=id))+geom_line()

ggplotly(p)

