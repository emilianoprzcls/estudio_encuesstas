library(tidyverse)
library(ggplot2)
library(treemapify)
library(MoMAColors)

padron <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/padron_estado_gener2.csv")
estados <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/covarrubias/covarrubias.csv")[,c(1)]
padron <- padron %>%
  mutate(rango_edad = paste(min, max, sep = "-"),  # Crear un rango de edad como una sola cadena
         genero = case_when(
           genero == 1 ~ "Hombres",
           genero == 2 ~ "Mujeres",
           genero == 9 ~ "No binario"
         ),
         total = ifelse(genero == "Hombres", -total, total))

estados$estado <- as.numeric(as.factor(estados$N_estado))
padron <- left_join(padron, estados, by='estado')%>%
  unique()

estados_unique <- estados %>%
  group_by(estado) %>%
  slice(1) %>%
  ungroup()

buendia_marquez <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/buendia_marquez/buendia_marquez.csv")
covarrubias <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/covarrubias/covarrubias.csv")
enkoll <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/enkoll/enkoll.csv")
mitofsky <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/mitofsky/mitofsky.csv")
te <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/te/te.csv")
financiero <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/elfinanciero/elfinanciero.csv")
meba <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/meba/meba.csv")
cipreso <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/cipreso/cipreso.csv")
geaisa <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/bd/geaisa/geaisa.csv")



estados_unique <- mutate(estados_unique, estado = as.numeric(estado))
buendia_marquez <- mutate(buendia_marquez, estado = as.numeric(estado))
enkoll <- mutate(enkoll, estado = as.numeric(estado))
mitofsky <- mutate(mitofsky, estado = as.numeric(estado))
te <- mutate(te, estado = as.numeric(estado))
financiero <- mutate(financiero, estado = as.numeric(estado))
meba <- mutate(meba, estado = as.numeric(estado))
cipreso <- mutate(cipreso, estado = as.numeric(estado))
geaisa <- mutate(geaisa, estado = as.numeric(estado))

buendia_marquez <- left_join(buendia_marquez, estados_unique, by = "estado")
covarrubias <- left_join(covarrubias, estados_unique, by = "N_estado")
enkoll <- left_join(enkoll, estados_unique, by = "estado")
mitofsky <- left_join(mitofsky, estados_unique, by = "estado")
te <- left_join(te, estados_unique, by = "estado")


geaisa <- geaisa %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
genero = case_when(
  genero == 1 ~ "Hombres",
  genero == 2 ~ "Mujeres",
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon))
geaisa$estado <- NA
geaisa$voto <- NA

cipreso <- cipreso %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
genero = case_when(
  genero == 'Hombre' ~ "Hombres",
  genero == 'Mujer' ~ "Mujeres",
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon))
cipreso$estado <- NA
cipreso$voto <- NA
cipreso <- cipreso %>% 
  filter(!is.na(total))%>% 
  filter(!is.na(partido))


financiero <- financiero %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
genero = case_when(
  genero == "Hombre" ~ "Hombres",
  genero == "Mujer" ~ "Mujeres"
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon))
financiero$voto <- NA

meba <- meba %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
genero = case_when(
  genero == 1 ~ "Hombres",
  genero == 2 ~ "Mujeres",
  genero == 9 ~ "No binario"
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon))
meba$estado <- NA
meba$voto <- NA
meba <- meba %>%
  mutate(partido = ifelse(is.na(partido), "NR", partido))

buendia_marquez <- buendia_marquez %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
  genero = case_when(
    genero == 1 ~ "Hombres",
    genero == 2 ~ "Mujeres",
    genero == 9 ~ "No binario"
  ),
  total = ifelse(genero == "Hombres", -homo_pon, homo_pon))

covarrubias <- covarrubias %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
genero = case_when(
  genero == 1 ~ "Hombres",
  genero == 2 ~ "Mujeres",
  genero == 9 ~ "No binario"
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon))

enkoll <- enkoll %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
genero = case_when(
  genero == 1 ~ "Hombres",
  genero == 2 ~ "Mujeres",
  genero == 9 ~ "No binario"
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon))

mitofsky <- mitofsky %>% mutate(rango_edad = case_when(
  edad == 18 ~ "18-18",
  edad == 19 ~ "19-19",
  edad > 19 & edad <= 24 ~ "20-24",
  edad > 24 & edad <= 29 ~ "25-29",
  edad > 29 & edad <= 34 ~ "30-34",
  edad > 34 & edad <= 39 ~ "35-39",
  edad > 39 & edad <= 44 ~ "40-44",
  edad > 44 & edad <= 49 ~ "45-49",
  edad > 49 & edad <= 54 ~ "50-54",
  edad > 54 & edad <= 59 ~ "55-59",
  edad > 59 & edad <= 64 ~ "60-64",
  edad > 64 ~ "65-100",
  TRUE ~ "Desconocido"
),
genero = case_when(
  genero == 1 ~ "Hombres",
  genero == 2 ~ "Mujeres",
  genero == 9 ~ "No binario"
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon))

te <- te %>% mutate(rango_edad = case_when(
  edad == 1 ~ "18-25",
  edad == 2 ~ "26-38",
  edad == 3 ~ "39-50",
  edad == 4 ~ "51-67",
  edad == 5 ~ "68-100",
),
genero = case_when(
  genero == 1 ~ "Hombres",
  genero == 2 ~ "Mujeres",
  genero == 9 ~ "No binario"
),
total = ifelse(genero == "Hombres", -homo_pon, homo_pon),
)


write_csv(buendia_marquez, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/buendia_marquez.csv")
write_csv(covarrubias, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/covarrubias.csv")
write_csv(enkoll, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/enkoll.csv")
write_csv(mitofsky, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/mitofsky.csv")
write_csv(te, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/te.csv")
write_csv(padron, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/padron.csv")
write_csv(financiero, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/financiero.csv")
write_csv(meba, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/meba.csv")
write_csv(cipreso, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/cipreso.csv")
write_csv(geaisa, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/geaisa.csv")

anova_geaisa_m <- geaisa %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_geaisa_h <- geaisa %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_cipreso_m <- cipreso %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_cipreso_h <- cipreso %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_buendia_marquez_m <- buendia_marquez %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_buendia_marquez_h <- buendia_marquez %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_covarrubias_m <- covarrubias %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_covarrubias_h <- covarrubias %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_enkoll_m <- enkoll %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_enkoll_h <- enkoll %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_mitofsky_m <- mitofsky %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_mitofsky_h <- mitofsky %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_te_m <- te %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_te_h <- te %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_financiero_m <- financiero %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_financiero_h <- financiero %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_meba_m <- meba %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_meba_h <- meba %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total = sum(abs(total)))

anova_padron_m <- padron %>%
  filter(genero == "Mujeres") %>%
  group_by(rango_edad) %>%
  summarize(total_p = sum(abs(total)))

anova_padron_h <- padron %>%
  filter(genero == "Hombres") %>%
  group_by(rango_edad) %>%
  summarize(total_p = sum(abs(total)))


write_csv(anova_buendia_marquez_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/buendia_marquez_h.csv")
write_csv(anova_covarrubias_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/covarrubias_h.csv")
write_csv(anova_enkoll_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/enkoll_h.csv")
write_csv(anova_mitofsky_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/mitofsky_h.csv")
write_csv(anova_te_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/te_h.csv")
write_csv(anova_padron_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/padron_h.csv")
write_csv(anova_buendia_marquez_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/buendia_marquez_m.csv")
write_csv(anova_covarrubias_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/covarrubias_m.csv")
write_csv(anova_enkoll_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/enkoll_m.csv")
write_csv(anova_mitofsky_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/mitofsky_m.csv")
write_csv(anova_te_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/te_m.csv")
write_csv(anova_padron_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/padron_m.csv")
write_csv(anova_financiero_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/financiero_h.csv")
write_csv(anova_financiero_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/financiero_m.csv")
write_csv(anova_meba_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/meba_h.csv")
write_csv(anova_meba_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/meba_m.csv")
write_csv(anova_cipreso_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/cipreso_h.csv")
write_csv(anova_cipreso_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/cipreso_m.csv")
write_csv(anova_geaisa_h, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/geaisa_h.csv")
write_csv(anova_geaisa_m, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/anova/geaisa_m.csv")





















#padron$ponreal <- abs(padron$total)
#ponreal <- padron %>% select(genero, estado, ponreal, rango_edad)
#ponreal$estado <- as.character(ponreal$estado)
#ponreal_aggregated <- ponreal %>%
#  group_by(genero, estado, rango_edad) %>%
#  summarise(ponreal = mean(ponreal, na.rm = TRUE), .groups = 'drop')
#
#
#buendia_marquez_vot <- left_join(buendia_marquez, ponreal_aggregated, by = c("genero", "estado", "rango_edad"))
#covarrubias_vot <- left_join(covarrubias, ponreal_aggregated, by = c("genero", "estado", "rango_edad"))
#enkoll_vot <- left_join(enkoll, ponreal_aggregated, by = c("genero", "estado", "rango_edad"))
#mitofsky_vot <- left_join(mitofsky, ponreal_aggregated, by = c("genero", "estado", "rango_edad"))
#
#
#write_csv(buendia_marquez_vot, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/voto/buendia_marquez_vot.csv")
#write_csv(covarrubias_vot, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/voto/covarrubias_vot.csv")
#write_csv(enkoll_vot, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/voto/enkoll_vot.csv")
#write_csv(mitofsky_vot, "~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/voto/mitofsky_vot.csv")


