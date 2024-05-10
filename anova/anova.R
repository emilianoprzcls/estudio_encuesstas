ANOVA_bloque <- function(DATA, FAlpha){
  # Asegurar que la columna 'BLOQUE' sea eliminada correctamente
  DATATRAT <- DATA[, !colnames(DATA) %in% "rango_edad", drop = FALSE]
  
  nBLOQUE <- nrow(DATA)
  nTRAT <- ncol(DATATRAT)
  valtrat <- list()
  valbloc <- list()
  values <- list()
  
  # Crear una lista con las sumas de cada tratamiento
  for (k in 1:nTRAT){
    valtrat[[paste0("sumtrat_", k)]] <- sum(DATATRAT[, k], na.rm = TRUE)
  }
  T <- sum(unlist(valtrat, use.names = FALSE))
  TT <- (T^2) / (nBLOQUE * nTRAT)
  
  # Crear una lista con las sumas de cada bloque
  # Corrección para asegurar que estamos sumando correctamente los bloques
  for (k in 1:nBLOQUE){
    valbloc[[paste0("sumbloque_", k)]] <- sum(DATA[k, -which(names(DATA) == "rango_edad")], na.rm = TRUE)
  }
  
  # Crear una lista con todos los valores
  for (i in 1:nTRAT){
    values[[paste0("x_", i)]] <- DATATRAT[, i]
  }
  
  # SSTR
  sub_SSTR_B <- function(list){
    sum((list^2) / nBLOQUE, na.rm = TRUE)
  }
  SSTR_B <- sum(unlist(lapply(valtrat, sub_SSTR_B))) - TT
  
  # SSB
  sub_SSB <- function(list){
    sum((list^2) / nTRAT, na.rm = TRUE)
  }
  SSB <- sum(unlist(lapply(valbloc, sub_SSB))) - TT
  
  # SSTOT
  sub_SSTOT <- function(list){
    sum((list^2), na.rm = TRUE)
  }
  SSTOT <- sum(unlist(lapply(values, sub_SSTOT))) - TT
  
  # Creación de la tabla
  FUENTE <- c("TRAT.", "BLOQUES", "ERROR", "TOTAL")
  DF <- c((nTRAT - 1), (nBLOQUE - 1), ((nTRAT - 1) * (nBLOQUE - 1)), ((nBLOQUE * nTRAT) - 1))
  SS <- c(SSTR_B, SSB, (SSTOT - SSTR_B - SSB), SSTOT)
  MSS <- c((SSTR_B / (nTRAT - 1)), (SSB / (nBLOQUE - 1)), ((SSTOT - SSTR_B - SSB) / ((nTRAT - 1) * (nBLOQUE - 1))), NA)
  F <- c((MSS[1] / MSS[3]), (MSS[2] / MSS[3]), NA, NA)
  
  # Calcular el F crítico
  Fcritico_trat <- qf(1 - FAlpha, nTRAT - 1, (nTRAT - 1) * (nBLOQUE - 1))
  Fcritico_bloc <- qf(1 - FAlpha, nBLOQUE - 1, (nTRAT - 1) * (nBLOQUE - 1))
  Fcritico <- c(Fcritico_trat, Fcritico_bloc, NA, NA)
  
  # Comparar F obtenido con F crítico
  Comparacion <- c(F[1] > Fcritico_trat, F[2] > Fcritico_bloc, NA, NA)
  
  # Añadir al data frame
  TABLA <- cbind.data.frame(FUENTE, DF, SS, MSS, F, Fcritico, Comparacion)
  knitr::kable(TABLA, caption = "ANOVA DE BLOQUES")
}

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
te_h <- read_csv("anova/te_h.csv")
te_m <- read_csv("anova/te_m.csv")

buendia_marquez_h <- left_join(buendia_marquez_h,padron_h, by="rango_edad")
covarrubias_h <- left_join(covarrubias_h,padron_h, by="rango_edad")
enkoll_h <- left_join(covarrubias_h,padron_h, by="rango_edad")
buendia_marquez_h <- left_join(covarrubias_h,padron_h, by="rango_edad")
mitofsky_h <- left_join(covarrubias_h,padron_h, by="rango_edad")

