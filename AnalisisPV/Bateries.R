library(data.table)
library("writexl")
library(flextools)
write_xlsx(taula_bateries,"taulabateries.xlsx")

taula_bateries$powerPV <- as.numeric(as.character(taula_bateries$powerPV))
print(taula_bateries)
sapply(taula_bateries, class)

vec1 <-taula_bateries$powerPV
vec2 <-taula_bateries$consum

taula_bateries <- taula_global3 %>% 
  mutate( 
    add_battery_simple(B=taula_bateries$powerPV, L=taula_bateries$consum, Bcap=10, Bc=10, Bd=10, SOCini = 0)
  )
   
vec1 <-as.numeric(vec1)



add_battery_simple(vec1, vec2,10,5,5,SOCini = 0)



taula_bateries2 <- taula_global3 %>% 
  mutate( 
    bateria = ifelse(consum - powerPV > 0, 
      ifelse(consum - powerPV -  lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)>0,0, lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)+powerPV-consum),
      ifelse(powerPV-consum>10,10,ifelse( lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)+powerPV-consum>10,10,lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)+powerPV-consum))
      
   )
  )
taula_bateries3 <- taula_bateries2 %>% 
  mutate(estalvi_bateria = ifelse(lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)-bateria>0,(lag(zoo::rollsumr(bateria, 1, fill = 0), default = 0)-bateria)*preu_energia,0)
  )



taula_bateries3<-taula_bateries3[-c(5809),]
estalviBat56_10=colSums(taula_bateries3[,11])








taula_bateries

taula_bateries <- taula_global3 %>% 
  mutate( 
    bateria = powerPV + consum_xarxa - consum,
    acumulat_bateria = ifelse(powerPV-consum>0,ifelse(powerPV-consum +c(NA,head(bateria,-1))>10,10,powerPV-consum+c(NA,head(bateria,-1))),0)
    consum_xarxa = ifelse(consum_xarxa>powerPV-acumulat_bateria,consum_xarxa-powerPV-acumulat_bateria,0)
    acumulat_bateria =ifelse(consum_xarxa>powerPV-acumulat_bateria, powerPV)
  )


