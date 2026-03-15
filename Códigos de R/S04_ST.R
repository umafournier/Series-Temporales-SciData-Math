#=====================================================#
# Diplomado: Series de tiempo con R y Python          #
# Modulo: Fundamentos y representacion del tiempo     #
# Tema: Operaciones con temporalidades                             #
# Docente: Alexis Adonai Morales Alberto              #
# Sesion: 02                                          #
# SciData                                             #
#=====================================================#

# Limpieza de memoria o consola -----

rm(list = ls())

# Verificador de pacman -----

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra instalado")
} else {
  install.packages("pacman", dependencies = T)
}

# Carga / instalacion de paquetes necesarios ----

pacman::p_load(
  "tidyverse",
  "tseries",
  "quantmod",
  "zoo",
  "lubridate",
  "xts",
  "tsbox"
)


# Cargar datos desde YF con quantmod ----

NFLX <- getSymbols(
  Symbols = "NFLX",
  env = NULL,
  src = "yahoo",
  from = "2015-03-05",
  to = "2026-03-05"
)

# Tranformar xts a data.frame ----

NFLX_df <- data.frame(
  "Fecha" = index(NFLX),
  coredata(NFLX)
)

# Comprobar formato de la columna fecha ----

class(NFLX_df$Fecha)

# Sustracciones de elementos o formatos de la fecha ----

# NOTA: PARA LOS SIGUIENTES PROCEDIMIENTOS, ES NECESARIO
# QUE EL FORMATO DE LA COLUMNA SEA "DATE", DE LO CONTRARIO
# LAS OPERACIONES QUE SE REALICEN A CONTINUACION
# NO SE PODRAN EFECTUAR

# Operador pipe: %>% ctrl+shift+m

NFLX_df <- NFLX_df %>%
  mutate(Año = format(Fecha, "%Y") ,
         Año2g = format(Fecha, "%y"),
         Mes_num = format(Fecha, "%m"),
         Mes_txt = format(Fecha, "%B"),
         Mes_txt2 = format(Fecha, "%b"),
         Dia_num = format(Fecha, "%d"),
         Dia_año = format(Fecha,"%j"),
         Dia_txt = format(Fecha, "%A"),
         Dia_txt2 = format(Fecha, "%a"),
         Semana_D = format(Fecha, "%U"),
         Semana_L = format(Fecha, "%W"),
         Trimestre = quarter(Fecha),
         TrimestreQ = as.yearqtr(Fecha))

## Exportar dataframe para posteriores usos ----

write.csv(x = NFLX_df,
          file = "Bases de datos/Datos_NFLX_temp.csv",
          fileEncoding = "UTF-8",
          row.names = F)

# Calculos usando las temporalidades ----

## Calculo de rendimientos o tasa de crecimiento ----

NFLX_df <- NFLX_df %>%
  mutate(TC_NFLX = c(NA, diff(log(NFLX.Adjusted))*100))

## Rendimientos promedios por mes ----

NFLX_df %>%
  group_by(Año, Mes_num) %>%
  summarise(TC_NFLX = mean(TC_NFLX, na.rm=T)) %>%
  view()
  
## Rendimientos analizados por cada año ----

NFLX_df %>%
  group_by(Año) %>%
  summarise(n())

NFLX_df %>%
  group_by(Año) %>%
  summarise(TC_NFLX = mean(TC_NFLX, na.rm=T)) %>%
  ungroup() %>%
  mutate(TC_NFLX = TC_NFLX*252) # Anualizado

NFLX_df %>%
  group_by(Año) %>%
  summarise(TC_NFLX = mean(TC_NFLX, na.rm=T),
            Dias_año = n()) %>%
  ungroup() %>%
  mutate(TC_NFLX = TC_NFLX*Dias_año)

# Rendimiento por dias de la semana ----

NFLX_df %>% 
  group_by(Año, Dia_txt) %>% 
  summarise(TC_NFLX = mean(TC_NFLX, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Dia_txt = factor(Dia_txt, 
                          levels = c("lunes",
                                     "martes",
                                     "miércoles",
                                     "jueves",
                                     "viernes"))) %>% 
  arrange(Año, Dia_txt) %>% 
  pivot_wider(id_cols = c("Año"),
              names_from = c("Dia_txt"),
              values_from = c("TC_NFLX"))

# Visualizacion

library(ggplot2)
library(tidyr)
library(dplyr)

tabla <- NFLX_df %>% 
  group_by(Año, Dia_txt) %>% 
  summarise(TC_NFLX = mean(TC_NFLX, na.rm = TRUE)) %>% 
  ungroup()

ggplot(tabla, aes(x = Dia_txt, y = TC_NFLX, fill = Dia_txt)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Año) +
  labs(title = "Rendimientos promedio por día de la semana",
       x = "Día",
       y = "Rendimiento promedio") +
  theme_minimal()