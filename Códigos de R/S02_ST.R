#=====================================================#
# Diplomado: Series de tiempo con R y Python          #
# Modulo: Fundamentos y representacion del tiempo     #
# Tema: Series de tiempo                              #
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


# Cargar datos desde FRED con quantmod ----

GDPR <- getSymbols(
  Symbols = "GDPC1",
  env = NULL,
  src = "FRED"
  
)

# Tranformar xts a data.frame ----

GDPR_df <- data.frame(
  "Fecha" = index(GDPR),
  coredata(GDPR)
)

# Transformar directamente a ts ----

GDPR_ts <- ts_ts(GDPR)

# Cargar desde csv -----

GDPR2 <- read.csv("Bases de datos/GDPC1.csv")

class(GDPR2$observation_date)
class(GDPR_df$Fecha)

# Transformacion de la fecha de caracter a date ----

GDPR2$observation_date <- as.Date(
  GDPR2$observation_date,
  format = "%Y-%m-%d"
)

# Transformacion a xts ----

GDPR_xts <- xts(x = GDPR2$GDPC1,
                order.by = GDPR2$observation_date)

#Transformacion a ts ----

GDPR_ts2 <- ts_ts(GDPR_xts)

#Manualmente con ts ----

GDPR_ts2 <- ts(data = GDPR2$GDPRC1,
               start = 1947,
               end = 2025,
               frequency = 4)

 
