library(readr)
library(klaR)
library(dplyr)
library(repurrrsive)
library(purrr)
library(data.table)

buendia_marquez_h <- read_csv("anova/buendia_marquez_h.csv")
buendia_marquez_m <- read_csv("anova/buendia_marquez_m.csv")
covarrubias_h <- read_csv("anova/covarrubias_h.csv")
covarrubias_m <- read_csv("anova/covarrubias_m.csv")
enkoll_h <- read_csv("anova/enkoll_h.csv")
enkoll_m <- read_csv("anova/enkoll_m.csv")
mitofsky_h <- read_csv("anova/mitofsky_h.csv")
mitofsky_m <- read_csv("anova/mitofsky_m.csv")
padron_h <- read_csv("anova/padron_h.csv")
padron_m <- read_csv("anova/padron_m.csv")
financiero_h <- read_csv("anova/financiero_h.csv")
financiero_m <- read_csv("anova/financiero_m.csv")
meba_h <- read_csv("anova/meba_h.csv")
meba_m <- read_csv("anova/meba_m.csv")
cipreso_h <- read_csv("anova/cipreso_h.csv")
cipreso_m <- read_csv("anova/cipreso_m.csv")


buendia_marquez_h <- left_join(buendia_marquez_h,padron_h, by="rango_edad")
covarrubias_h <- left_join(covarrubias_h,padron_h, by="rango_edad")
enkoll_h <- left_join(enkoll_h,padron_h, by="rango_edad")
mitofsky_h <- left_join(mitofsky_h,padron_h, by="rango_edad")
financiero_h <- left_join(financiero_h,padron_h, by="rango_edad")
meba_h <- left_join(meba_h,padron_h, by="rango_edad")
cipreso_h <- left_join(cipreso_h,padron_h, by="rango_edad")

buendia_marquez_m <- left_join(buendia_marquez_m, padron_m, by="rango_edad")
covarrubias_m <- left_join(covarrubias_m, padron_m, by="rango_edad")
enkoll_m <- left_join(enkoll_m, padron_m, by="rango_edad")
mitofsky_m <- left_join(mitofsky_m, padron_m, by="rango_edad")
financiero_m <- left_join(financiero_m,padron_h, by="rango_edad")
meba_m <- left_join(meba_m,padron_m, by="rango_edad")
cipreso_m <- left_join(cipreso_m,padron_m, by="rango_edad")

buendia_marquez_t <- left_join(buendia_marquez_m, buendia_marquez_h, by="rango_edad")
covarrubias_t <- left_join(covarrubias_m, covarrubias_h, by="rango_edad")
enkoll_t <- left_join(enkoll_m, enkoll_h, by="rango_edad")
mitofsky_t <- left_join(mitofsky_m, mitofsky_h, by="rango_edad")
financiero_t <- left_join(financiero_m, financiero_h, by="rango_edad")
meba_t <- left_join(meba_m, meba_h, by="rango_edad")
cipreso_t <- left_join(cipreso_m,cipreso_h, by="rango_edad")

buendia_marquez_t$p_mujeres <- ((buendia_marquez_t[,2] - buendia_marquez_t[,3])^2)/buendia_marquez_t[,3]
buendia_marquez_t$p_hombres <- ((buendia_marquez_t[,4] - buendia_marquez_t[,5])^2)/buendia_marquez_t[,5]
buendia_marquez_t$p_total <- (((buendia_marquez_t[,2]+buendia_marquez_t[,4]) - 
                                (buendia_marquez_t[,3]+buendia_marquez_t[,5]))^2)/(buendia_marquez_t[,3]+buendia_marquez_t[,5])

covarrubias_t$p_mujeres <- ((covarrubias_t[,2] - covarrubias_t[,3])^2)/covarrubias_t[,3]
covarrubias_t$p_hombres <- ((covarrubias_t[,4] - covarrubias_t[,5])^2)/covarrubias_t[,5]
covarrubias_t$p_total <- (((covarrubias_t[,2]+covarrubias_t[,4]) - 
                                (covarrubias_t[,3]+covarrubias_t[,5]))^2)/(covarrubias_t[,3]+covarrubias_t[,5])


enkoll_t$p_mujeres <- ((enkoll_t[,2] - enkoll_t[,3])^2)/enkoll_t[,3]
enkoll_t$p_hombres <- ((enkoll_t[,4] - enkoll_t[,5])^2)/enkoll_t[,5]
enkoll_t$p_total <- (((enkoll_t[,2]+enkoll_t[,4]) - 
                                (enkoll_t[,3]+enkoll_t[,5]))^2)/(enkoll_t[,3]+enkoll_t[,5])

mitofsky_t$p_mujeres <- ((mitofsky_t[,2] - mitofsky_t[,3])^2)/mitofsky_t[,3]
mitofsky_t$p_hombres <- ((mitofsky_t[,4] - mitofsky_t[,5])^2)/mitofsky_t[,5]
mitofsky_t$p_total <- (((mitofsky_t[,2]+mitofsky_t[,4]) - 
                                (mitofsky_t[,3]+mitofsky_t[,5]))^2)/(mitofsky_t[,3]+mitofsky_t[,5])

financiero_t$p_mujeres <- ((financiero_t[,2] - financiero_t[,3])^2)/financiero_t[,3]
financiero_t$p_hombres <- ((financiero_t[,4] - financiero_t[,5])^2)/financiero_t[,5]
financiero_t$p_total <- (((financiero_t[,2]+financiero_t[,4]) - 
                                (financiero_t[,3]+financiero_t[,5]))^2)/(financiero_t[,3]+financiero_t[,5])

financiero_t$p_mujeres <- ((financiero_t[,2] - financiero_t[,3])^2)/financiero_t[,3]
financiero_t$p_hombres <- ((financiero_t[,4] - financiero_t[,5])^2)/financiero_t[,5]
financiero_t$p_total <- (((financiero_t[,2]+financiero_t[,4]) - 
                                (financiero_t[,3]+financiero_t[,5]))^2)/(financiero_t[,3]+financiero_t[,5])

meba_t$p_mujeres <- ((meba_t[,2] - meba_t[,3])^2)/meba_t[,3]
meba_t$p_hombres <- ((meba_t[,4] - meba_t[,5])^2)/meba_t[,5]
meba_t$p_total <- (((meba_t[,2]+meba_t[,4]) - 
                                (meba_t[,3]+meba_t[,5]))^2)/(meba_t[,3]+meba_t[,5])

cipreso_t$p_mujeres <- ((cipreso_t[,2] - cipreso_t[,3])^2)/cipreso_t[,3]
cipreso_t$p_hombres <- ((cipreso_t[,4] - cipreso_t[,5])^2)/cipreso_t[,5]
cipreso_t$p_total <- (((cipreso_t[,2]+cipreso_t[,4]) - 
                                (cipreso_t[,3]+cipreso_t[,5]))^2)/(cipreso_t[,3]+cipreso_t[,5])



1 - pchisq(sum(buendia_marquez_t[,6]), df = 11)
1 - pchisq(sum(buendia_marquez_t[,7]), df = 11)
1 - pchisq(sum(buendia_marquez_t[,8]), df = 11)

1 - pchisq(sum(covarrubias_t[,6]), df = 11)
1 - pchisq(sum(covarrubias_t[,7]), df = 11)
1 - pchisq(sum(covarrubias_t[,8]), df = 11)

1 - pchisq(sum(enkoll_t[,6]), df = 11)
1 - pchisq(sum(enkoll_t[,7]), df = 11)
1 - pchisq(sum(enkoll_t[,8]), df = 11)

1 - pchisq(sum(mitofsky_t[,6]), df = 11)
1 - pchisq(sum(mitofsky_t[,7]), df = 11)
1 - pchisq(sum(mitofsky_t[,8]), df = 11)

1 - pchisq(sum(financiero_t[,6]), df = 11)
1 - pchisq(sum(financiero_t[,7]), df = 11)
1 - pchisq(sum(financiero_t[,8]), df = 11)

1 - pchisq(sum(meba_t[,6]), df = 11)
1 - pchisq(sum(meba_t[,7]), df = 11)
1 - pchisq(sum(meba_t[,8]), df = 11)

1 - pchisq(sum(cipreso_t[,6]), df = 11)
1 - pchisq(sum(cipreso_t[,7]), df = 11)
1 - pchisq(sum(cipreso_t[,8]), df = 11)

chi_cuadrada_critica <- qchisq(0.95, df = 11)
print(chi_cuadrada_critica)


