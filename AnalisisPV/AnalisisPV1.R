library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
Consum_Setembre21_Abril22 <- read_excel("~/Consum/Consum-Setembre21-Abril22.xlsx")

taula_energia <- Consum_Setembre21_Abril22
colnames (taula_energia) <- c ('datetime', 'consum')

taula_energia2 <- taula_energia %>% 
  mutate(
    month = month(datetime),
    wday = wday(datetime, week_start = 1),
    hour = hour(datetime)
  )

# Taula preus energia --------------------------------------------------------------------

taula_preus <- tibble(
  month = rep(1:12, each = 7*24),
  wday = rep(rep(1:7, each = 24), times = 12),
  hour = rep(0:23, times = 12*7)
)  %>%
  mutate(
    preu_energia = ifelse(
      wday>5,
      0.2501,
      ifelse(
        hour<8,
        0.2501,
        ifelse(
          month %in% c(1, 2, 7, 12),
          ifelse(
            hour %in% c(8, 14:17, 22, 23),
            0.4292,
            0.3368
          ),
          ifelse(
            month %in% c(3, 11),
            ifelse(
              hour %in% c(8, 14:17, 22, 23),
              0.3824,
              0.4292
            ),
            ifelse(
              month %in% c(4, 5, 10),
              ifelse(
                hour %in% c(8, 14:17, 22, 23),
                0.2459,
                0.2632
              ),
              ifelse(
                month %in% c(6, 8, 9),
                ifelse(
                  hour %in% c(8, 14:17, 22, 23),
                  0.2632,
                  0.3824
                ),
               0
              )
            )
          )
        )
        )
      ),
    preu_excedent=0
  )

# Taula energia i preus --------------------------------------------------------
taula_total <- left_join(
  taula_energia2,
  taula_preus,
  by = c('month', 'wday', 'hour')
) %>% 
  select(-c(month, wday, hour))



 taula_total2 <- taula_total[order(taula_total$datetime), ]

 taula_global <-left_join(
   taula_total2,
   taula_generacio,
   by = c('datetime')
   )
 
taula_global2 <- mutate_at(taula_global, c("powerPV"), ~replace(., is.na(.), 0))
 
taula_global3 <- taula_global2 %>% 
mutate( 
  consum_xarxa = ifelse(consum - powerPV>0,consum - powerPV,0),
  injeccio_xarxa = ifelse(consum-powerPV<0,powerPV - consum,0),
  cost = consum_xarxa*preu_energia,
  EstalviPW = ifelse(consum - powerPV >0, powerPV*preu_energia, consum*preu_energia)
)



   
 
 