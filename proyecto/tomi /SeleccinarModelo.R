##################################################
###                                            ###
###                  Proyecto                  ###
###                                            ###
##################################################

###############
#  Librerias  #
###############

library(tidyverse)
library(lubridate)
library(MASS)   

#####################
#  1. Carga datos   #
#####################

# EL procesamiento de datos esta en el jupiter: "limpiezaDatos"

datos_modelo <- read_csv("Datos/datos_combinados.csv", show_col_types = FALSE) %>%
  mutate(
    fecha_hora = as.POSIXct(fecha_hora),
    dia_semana = wday(fecha_hora, label = TRUE, week_start = 1),
    mes        = month(fecha_hora, label = TRUE)
  ) %>%
  arrange(fecha_hora)

head(datos_modelo)

#############################################
#  2. Division en train (80%) y test (20%)  #
#############################################

n       <- nrow(datos_modelo)
n_train <- floor(0.80 * n)

train <- datos_modelo %>% 
  slice(1:n_train)

test  <- datos_modelo %>% 
  slice((n_train + 1):n)

#################################
#  3. Ajuste del modelo básico  #
#################################

mod_basico <- lm( # co y so2 son lo mismo
  pm25 ~ pm10 + so2 + no2 + co + o3 + viento_ms + dia_semana + mes,
  data = train
)

summary(mod_basico)

##############################
#  4. Seleccion de modelo    #
##############################

mod_full <- mod_basico

# Backward
mod_back <- stepAIC(mod_full, direction = "backward", trace = FALSE)

# Forward
mod_null <- lm(pm25 ~ 1, data = train)
mod_fwd  <- stepAIC(mod_null,
                 scope = ~ pm10 + so2 + no2 + co + o3 + viento_ms + dia_semana + mes,
                 direction = "forward",
                 trace = FALSE)

# Comparar los 3 modelos
AIC(mod_basico, mod_back, mod_fwd)
# Los modelos forward y backwar son iguales
