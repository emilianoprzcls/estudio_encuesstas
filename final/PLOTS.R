library(readr)
library(tidyverse)
library(ggplot2)
library(treemapify)
library(MoMAColors)
library(scales)
library(MoMAColors)
library(MetBrewer)

col <- c("#af4f2f", "#d48f90","#df8d71", "#1e5a46", "#75884b", "#1e395f","#5b859e", "#5b859e")

buendia_marquez <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/buendia_marquez.csv")
covarrubias <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/covarrubias.csv")
enkoll <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/enkoll.csv")
mitofsky <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/mitofsky.csv")
te <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/te.csv")
financiero <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/financiero.csv")
padron <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/padron.csv")
meba <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/meba.csv")
cipreso <- read_csv("~/OneDrive/DOCUMENTOS/CIDE/8-ECO/SS/encuestas/final/cipreso.csv")

padron$partido <- NA
positive_format <- function(x) {
  sprintf("%-20s", format(abs(x), big.mark = ",", scientific = FALSE))
}

all_data <- bind_rows(
  buendia_marquez %>% mutate(Fuente = "Buendía y Marquez"),
  covarrubias %>% mutate(Fuente = "Covarrubias y Asociados"),
  enkoll %>% mutate(Fuente = "Enkoll"),
  mitofsky %>% mutate(Fuente = "Consulta Mitofsky"),
  financiero %>% mutate(Fuente = "El Financiero"),
  meba %>% mutate(Fuente = "MEBA"),
  cipreso %>% mutate(Fuente = "CIPRESO")
)
all_data$partido <- ifelse(is.na(all_data$partido), "NR", all_data$partido)


all_data <- all_data %>%
  mutate(Fuente = factor(Fuente, levels = c("Buendía y Marquez", "Covarrubias y Asociados", "Enkoll", "Consulta Mitofsky", "El Financiero", "MEBA", "CIPRESO","GEA-ISA")))

suma_padron <- padron %>% 
  group_by(rango_edad, genero) %>% 
  summarise(total_sum = sum(total))

suma_padron <- padron %>% 
  filter(!grepl("No binario", genero)) %>%
  group_by(rango_edad, genero) %>% 
  summarise(total_sum = sum(total))

suma_padron$partido <- "Padrón"


# Crear el gráfico
ggplot(all_data, aes(x = rango_edad, y = total, fill = partido)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Fuente, scales = "free_y", ncol = 1) +  
  coord_flip()  +
  labs(title = " ",
       x = "Rango de Edad",
       y = "Población") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(family = "Times New Roman", size = 12), # Adjust the size here
        axis.title.x = element_text(margin = margin(t = 15, b = 1), size = 14), # Adjust the size here
        axis.title.y = element_text(margin = margin(r = 15, l = 1), size = 14), # Adjust the size here
        axis.text.x = element_text(margin = margin(t = 5), size = 12), # Adjust the size here
        axis.text.y = element_text(margin = margin(r = 5), size = 12), # Adjust the size here
        plot.title = element_text(margin = margin(b = 10), size = 20),
        strip.text = element_text(size = 20)) + # Adjust the size here
  scale_fill_manual(values = col, guide = guide_legend(title = " ")) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  annotate("text", x = 0, y = 4, label = "Mujeres", vjust = -0.5, hjust = -4, size = 4, family = "Times New Roman") + 
  annotate("text", x = 0, y = 0, label = "Hombres", vjust = -0.5, hjust = 4, size = 4, family = "Times New Roman") +
  ylim(-8000000, 8000000)+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -8000000, ymax = 0, alpha = 0.3, fill = "gray")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 8000000, alpha = 0.3, fill = "#62393D")+
  geom_point(data=suma_padron, aes(x = rango_edad, y = total_sum), shape=17, size=2.5)

suma_padron_total <- padron %>% 
  group_by(rango_edad) %>% 
  summarise(total_sum = sum(abs(total)))
suma_padron_total$partido <- "Padrón"

ggplot(all_data, aes(x = rango_edad, y = homo_pon, fill = partido)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Fuente, scales = "free_y", ncol = 1) +  
  coord_flip()  +
  labs(title = " ",
       x = "Rango de Edad",
       y = "Población") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(family = "Times New Roman", size = 12), # Adjust the size here
        axis.title.x = element_text(margin = margin(t = 15, b = 1), size = 14), # Adjust the size here
        axis.title.y = element_text(margin = margin(r = 15, l = 1), size = 14), # Adjust the size here
        axis.text.x = element_text(margin = margin(t = 5), size = 12), # Adjust the size here
        axis.text.y = element_text(margin = margin(r = 5), size = 12), # Adjust the size here
        plot.title = element_text(margin = margin(b = 10), size = 20),
        strip.text = element_text(size = 20)) + # Adjust the size here
  scale_fill_manual(values = col, guide = guide_legend(title = " ")) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  ylim(0, 16000000)+
  geom_point(data=suma_padron_total, aes(x = rango_edad, y = total_sum), shape=17, size=2.5)


ggplot(padron, aes(x = rango_edad, y = total, fill = N_estado)) +
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip()  +
  labs(title = " ",
       x = "Rango de Edad",
       y = "Población") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(family = "Times New Roman", size = 12), # Adjust the size here
        axis.title.x = element_text(margin = margin(t = 15, b = 1), size = 14), # Adjust the size here
        axis.title.y = element_text(margin = margin(r = 15, l = 1), size = 14), # Adjust the size here
        axis.text.x = element_text(margin = margin(t = 5), size = 12), # Adjust the size here
        axis.text.y = element_text(margin = margin(r = 5), size = 12), # Adjust the size here
        plot.title = element_text(margin = margin(b = 10), size = 20),
        strip.text = element_text(size = 20)) + # Adjust the size here
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = moma.colors("Warhol", 32), guide = guide_legend(title = " ")) +
  annotate("text", x = 0, y = 4, label = "Mujeres", vjust = -0.5, hjust = -2, size = 5, family = "Times New Roman") + 
  annotate("text", x = 0, y = 0, label = "Hombres", vjust = -0.5, hjust = 2, size = 5, family = "Times New Roman")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -8000000, ymax = 0, alpha = 0.3, fill = "gray")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 8000000, alpha = 0.3, fill = "#62393D")

ggplot(padron, aes(x = rango_edad, y = abs(total), fill = N_estado)) +
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip()  +
  labs(title = " ",
       x = "Rango de Edad",
       y = "Población") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(family = "Times New Roman", size = 12), # Adjust the size here
        axis.title.x = element_text(margin = margin(t = 15, b = 1), size = 14), # Adjust the size here
        axis.title.y = element_text(margin = margin(r = 15, l = 1), size = 14), # Adjust the size here
        axis.text.x = element_text(margin = margin(t = 5), size = 12), # Adjust the size here
        axis.text.y = element_text(margin = margin(r = 5), size = 12), # Adjust the size here
        plot.title = element_text(margin = margin(b = 10), size = 20),
        strip.text = element_text(size = 20)) + # Adjust the size here
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_fill_manual(values = moma.colors("Warhol", 32), guide = guide_legend(title = " "))

