library(lubridate)
library(dplyr)


Set_Inv1 <- read_excel("~/Desktop/DadesInversors/Set-Inv1.xlsx")
Set_Inv2 <- read_excel("~/Desktop/DadesInversors/Set-Inv2.xlsx")
Oct_Inv1 <- read_excel("~/Desktop/DadesInversors/Oct-Inv1.xlsx")
Oct_Inv2 <- read_excel("~/Desktop/DadesInversors/Oct-Inv2.xlsx")
Nov_Inv1 <- read_excel("~/Desktop/DadesInversors/Nov-Inv1.xlsx")
Nov_Inv2 <- read_excel("~/Desktop/DadesInversors/Nov-Inv2.xlsx")
Des_Inv1 <- read_excel("~/Desktop/DadesInversors/Des-Inv1.xlsx")
Des_Inv2 <- read_excel("~/Desktop/DadesInversors/Des-Inv2.xlsx")
Gen_Inv1 <- read_excel("~/Desktop/DadesInversors/Gen-Inv1.xlsx")
Gen_Inv2 <- read_excel("~/Desktop/DadesInversors/Gen-Inv2.xlsx")
Feb_Inv1 <- read_excel("~/Desktop/DadesInversors/Feb-Inv1.xlsx")
Feb_Inv2 <- read_excel("~/Desktop/DadesInversors/Feb-Inv2.xlsx")
Mar_Inv1 <- read_excel("~/Desktop/DadesInversors/Mar-Inv1.xlsx")
Mar_Inv2 <- read_excel("~/Desktop/DadesInversors/Mar-Inv2.xlsx")
Abr_Inv1 <- read_excel("~/Desktop/DadesInversors/Abr-Inv1.xlsx")
Abr_Inv2 <- read_excel("~/Desktop/DadesInversors/Abr-Inv2.xlsx")

colnames (Set_Inv1) <- c ('datetime', 'consum')
colnames (Set_Inv2) <- c ('datetime', 'consum')
colnames (Oct_Inv1) <- c ('datetime', 'consum')
colnames (Oct_Inv2) <- c ('datetime', 'consum')
colnames (Nov_Inv1) <- c ('datetime', 'consum')
colnames (Nov_Inv2) <- c ('datetime', 'consum')
colnames (Des_Inv1) <- c ('datetime', 'consum')
colnames (Des_Inv2) <- c ('datetime', 'consum')
colnames (Gen_Inv1) <- c ('datetime', 'consum')
colnames (Gen_Inv2) <- c ('datetime', 'consum')
colnames (Feb_Inv1) <- c ('datetime', 'consum')
colnames (Feb_Inv2) <- c ('datetime', 'consum')
colnames (Feb_Inv1) <- c ('datetime', 'consum')
colnames (Feb_Inv2) <- c ('datetime', 'consum')
colnames (Mar_Inv1) <- c ('datetime', 'consum')
colnames (Mar_Inv2) <- c ('datetime', 'consum')
colnames (Abr_Inv1) <- c ('datetime', 'consum')
colnames (Abr_Inv2) <- c ('datetime', 'consum')

Set<- merge(Set_Inv1, Set_Inv2, by="datetime", all=TRUE)
Oct <- merge(Oct_Inv1, Oct_Inv2, by="datetime", all=TRUE) 
Nov <- merge(Nov_Inv1, Nov_Inv2, by="datetime", all=TRUE) 
Des <- merge(Des_Inv1, Des_Inv2, by="datetime", all=TRUE) 
Gen <- merge(Gen_Inv1, Gen_Inv2, by="datetime", all=TRUE) 
Feb <- merge(Feb_Inv1, Feb_Inv2, by="datetime", all=TRUE) 
Mar <- merge(Mar_Inv1, Mar_Inv2, by="datetime", all=TRUE) 
Abr <- merge(Abr_Inv1, Abr_Inv2, by="datetime", all=TRUE) 

SetOct <- merge(Set,Oct, all=TRUE)
NovDes <- merge(Nov,Des,all=TRUE)
GenFeb <- merge(Gen,Feb,all=TRUE)
MarAbr <- merge(Mar,Abr,all=TRUE)

SetOctNovDes <- merge(SetOct,NovDes,all=TRUE)
GenFebMarAbr <- merge(GenFeb, MarAbr, all = TRUE)

Inversors <- merge(SetOctNovDes,GenFebMarAbr, all=TRUE)

colnames (Inversors) <- c ('datetime', 'consum1','consum2')

Inversors1 <- mutate_all(Inversors, ~replace(., is.na(.), 0))

taula_generacio <- Inversors1  %>%
  mutate(
    power = consum1 + consum2,
    datetime = as_datetime(datetime),
    datetime = round_date(datetime, 'hour'),
  ) %>%
  group_by(datetime)%>%
  summarise(powerPV = mean(power))


