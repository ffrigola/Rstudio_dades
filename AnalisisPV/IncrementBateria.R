#Partim duna bateria de 10kW

#Cas 75KWp

taula_bateries3 <- taula_bateries2 %>% 
  mutate(estalvi_bateria = ifelse(lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)-bateria>0,(lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)-bateria)*preu_energia,0)
  )

add_battery_simple(B=10, L=3, Bcap=10, Bc=10, Bd=10, SOCini = 0)
