library(dygraphs)

#Cas actual
taula57<-taula_global3[-c(5809),]
consum_global57 = colSums(taula57[ , 8:9])
c57=colSums(taula57[,2])
cc57=colSums(taula57[,5:6])

#NO FUNCIONA GRÀFIC CONSUM LLARG DEL DATETIME
taula_plot = taula57 %>% 
  select(datetime, consum, powerPV, consum_xarxa, injeccio_xarxa) %>% 
  tidyr::pivot_longer(!datetime, names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = datetime, y = value, color = variable)) +
  geom_line() + 
  labs(x = "Dia i hora", y = "Energia (kWh)", color = "")

ggplotly(taula_plot)


taula57 %>% 
  select(datetime, consum, powerPV, consum_xarxa, injeccio_xarxa) %>% 
  dyplot(xlab="Dia i mes",ylab = "Energia (kWh)", fillGraph = T, strokeWidth = 2, stepPlot = F) %>% 
  dyLegend(show = "always", width = 150)

# ?dygraphs::dyOptions


#Cas 75KWp

factor = 70/56.84

taula_global75 <-taula_global3 %>%
mutate(
  powerPV=powerPV*factor,
  consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
  injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
  cost = consum_xarxa*preu_energia,
  EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
)


taula75 <-taula_global75[-c(5809),]


consum_global75 = colSums(taula75[ , 8:9])
c75=colSums(taula75[,2])
cc75=colSums(taula75[,5:6])
  
#Cas 85KWp
factor1 = 85/56.84


taula_global85 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor1,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula85 <-taula_global85[-c(5809),]

consum_global85 = colSums(taula85[ , 8:9])
c85=colSums(taula85[,2])
cc85=colSums(taula85[,5:6])

#Cas 100KWp
factor2=100/56.84

taula_global100 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor2,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula100 <-taula_global100[-c(5809),]

consum_global100 = colSums(taula100[ , 8:9])
c100=colSums(taula100[,2])
cc100=colSums(taula100[,5:6])

#Cas 115KWp
factor3=115/56.84

taula_global115 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor3,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula115 <-taula_global115[-c(5809),]

consum_global115 = colSums(taula115[ , 8:9])
c115=colSums(taula115[,2])
cc115=colSums(taula115[,5:6])

#Cas 130KWp
factor4=130/56.84
taula_global130 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor4,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula130 <-taula_global130[-c(5809),]

consum_global130 = colSums(taula130[ , 8:9])
c130=colSums(taula130[,2])
cc130=colSums(taula130[,5:6])

#Cas 142KWp
factor5=142/56.84
taula_global142 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor5,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula142 <-taula_global142[-c(5809),]

consum_global142 = colSums(taula142[ , 8:9])
c142=colSums(taula142[,2])
cc142=colSums(taula142[,5:6])

#Cas 155KWp
factor6=155/56.84
taula_global155 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor6,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula155 <-taula_global155[-c(5809),]

consum_global155 = colSums(taula155[ , 8:9])
c155=colSums(taula155[,2])
cc155=colSums(taula155[,5:6])


#Cas 170KWp
factor7=170/56.84
taula_global170 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor7,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula170 <-taula_global170[-c(5809),]

consum_global170 = colSums(taula170[ , 8:9])
c170=colSums(taula170[,2])
cc170=colSums(taula170[,5:6])

#Cas 185KWp
factor8=185/56.84
taula_global185 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor8,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula185 <-taula_global185[-c(5809),]

consum_global185 = colSums(taula185[ , 8:9])
c185=colSums(taula185[,2])
cc185=colSums(taula185[,5:6])

#Cas 200KWp
factor9=200/56.84
taula_global200 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor9,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula200 <-taula_global200[-c(5809),]

consum_global200 = colSums(taula200[ , 8:9])
c200=colSums(taula200[,2])
cc200=colSums(taula200[,5:6])

#Cas 250KWp
factor10=250/56.84
taula_global250 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor10,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula250 <-taula_global250[-c(5809),]

consum_global250 = colSums(taula250[ , 8:9])
c250=colSums(taula250[,2])
cc250=colSums(taula250[,5:6])

#Cas 300KWp
factor11=300/56.84
taula_global300 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor11,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula300 <-taula_global300[-c(5809),]

consum_global300 = colSums(taula300[ , 8:9])
c300=colSums(taula300[,2])
cc300=colSums(taula300[,5:6])

#Cas 350KWp
factor12=350/56.84
taula_global350 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor12,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula350 <-taula_global350[-c(5809),]

consum_global350 = colSums(taula350[ , 8:9])
c350=colSums(taula350[,2])
cc350=colSums(taula350[,5:6])


#Cas 400KWp
factor13=400/56.84
taula_global400 <-taula_global3 %>%
  mutate(
    powerPV=powerPV*factor13,
    consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
    injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
    cost = consum_xarxa*preu_energia,
    EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
  )

taula400 <-taula_global400[-c(5809),]

consum_global400 = colSums(taula400[ , 8:9])
c400=colSums(taula400[,2])
cc400=colSums(taula400[,5:6])


