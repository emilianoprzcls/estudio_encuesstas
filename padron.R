library(readr)
padron <- read_csv("bd/padron.csv")
padron <- padron[-1, ]
padron <- padron[1:(nrow(padron)-1), ]
padron <- padron %>%
  select("CLAVE\nENTIDAD", matches("LISTA")) %>%
  rename("clave" = "CLAVE\nENTIDAD") %>%
  select(-2:-5) %>%
  group_by(clave) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) 
padron$clave <- as.numeric(padron$clave)
library(tidyr)

padron_long <- padron %>%
  pivot_longer(cols = -clave, names_to = "columnas", values_to = "total") %>%
  mutate(digitos = gsub("\\D", "", columnas)) %>%
  mutate(min = substr(digitos, 1, 2),  # Obtener los primeros dos dígitos
         max = ifelse(nchar(digitos) == 2, min, substr(digitos, 3, 4))) %>%
  mutate(genero = case_when(
    grepl("HOMBRES", columnas) ~ 1,     # Si la columna contiene "HOMBRES", asignar 1
    grepl("MUJERES", columnas) ~ 2,     # Si la columna contiene "MUJERES", asignar 2
    grepl("NOBINARIO", columnas) ~ 9,   # Si la columna contiene "NOBINARIO", asignar 9
    TRUE ~ NA_integer_                  # Si no coincide con ninguno, asignar NA
  )) %>%
  rename("estado" = "clave")

padron_long$max[padron_long$max == 65] <- 100
write.csv(padron_long, "padron_estado_gener2.csv", row.names = FALSE)
