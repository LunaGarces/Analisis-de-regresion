library(readr)
library(rio)
library(dplyr)
library(lubridate)
library(corrplot)
library(MASS)    
library(Metrics) 
library(car)

data_met <- read_csv("datos_completos_estacion_punteras.csv") 
names(data_met)
data_met <- data_met %>%
  dplyr::select(-c("N02", "MP10", "NO"))
names(data_met)
set.seed(123)


################################
## Separacion de datos 80, 20 ##
################################
train_indices <- sample(1:nrow(data_met), size = 0.8 * nrow(data_met))
train_data <- data_met[train_indices, ]
test_data <- data_met[-train_indices, ]


#####################################
## PCA para var con max, min, mean ##
#####################################

######################################
## PCA para variables de temperatura##
######################################
temps <- train_data %>% dplyr::select(temperatura_mean, temperatura_max, temperatura_min)
pca_temp <- prcomp(temps, scale. = TRUE)
train_data$temp_pca <- pca_temp$x[, 1]
train_data <- train_data %>% dplyr::select(-temperatura_mean, -temperatura_max, -temperatura_min)
pca_temp$rotation

######################################
## PCA para variables de vel viento ##
######################################
hume <- train_data %>% dplyr::select(humedad_mean, humedad_max, humedad_min)
pca_humi <- prcomp(hume, scale. = TRUE)
train_data$humidity_pca <- pca_humi$x[, 1]
train_data <- train_data %>% dplyr::select(-humedad_mean, -humedad_max, -humedad_min)
pca_humi$rotation

####################################
## PCA para variables de humidity ##
####################################
vel_viento <- train_data %>% dplyr::select(velocidad_viento_mean, velocidad_viento_max, velocidad_viento_min)
pca_vel_viento <- prcomp(vel_viento, scale. = TRUE)
train_data$vel_viento_pca <- pca_humi$x[, 1]
train_data <- train_data %>% dplyr::select(-velocidad_viento_mean, -velocidad_viento_max, -velocidad_viento_min)
pca_vel_viento$rotation

names(train_data)

#########################
## Modelo con Backward ##
#########################

modelo_inicial <- lm(MP2_5 ~ temp_pca + humidity_pca + vel_viento_pca + 
                       precipitacion_mean + precipitacion_max + 
                       direccion_viento_mean + direccion_viento_max + 
                       direccion_viento_min,
                     data = train_data)
modelo_backward <- step(modelo_inicial, direction = "backward")

summary(modelo_inicial)
summary(modelo_backward) # lm(formula = MP2_5 ~ temp_pca + direccion_viento_mean + direccion_viento_max + precipitacion_max, data = train_data)


##############################
### Transformación Box-Cox ### 
##############################

# trasforamcion a predictores 


# transformacion a MP2_5

##############################
### Transformación Box-Cox ### 
##############################

# 1. Verificar si necesitamos un offset (Box-Cox no acepta ceros ni negativos)
min_y <- min(train_data$MP2_5, na.rm = TRUE)
offset <- 0
if (min_y <= 0) {
  offset <- abs(min_y) + 0.1 # Sumamos una constante pequeña si es necesario
  cat("Se aplicó un offset de:", offset, "\n")
}

# 2. Calcular el Lambda óptimo usando el modelo seleccionado (modelo_backward)
# Se graficará la curva de verosimilitud para que veas el pico
bc <- boxcox(modelo_backward, plotit = TRUE, lambda = seq(-1, 1, by = 0.1))

# Extraer el lambda exacto donde se maximiza la curva
lambda_opt <- bc$x[which.max(bc$y)]
cat("El Lambda óptimo es:", round(lambda_opt, 3), "\n")

# 3. Aplicar la transformación a MP2_5 (Tanto en Train como en Test)
# Definimos una función para transformar
trans_boxcox <- function(y, lambda, offset = 0) {
  y_adj <- y + offset
  if (abs(lambda) < 0.1) { # Si lambda es casi 0, usamos Log
    return(log(y_adj))
  } else {                 # Si no, usamos la fórmula de potencia
    return((y_adj^lambda - 1) / lambda)
  }
}

# Creamos la nueva variable transformada en ambos sets
train_data$MP2_5_bc <- trans_boxcox(train_data$MP2_5, lambda_opt, offset)
test_data$MP2_5_bc  <- trans_boxcox(test_data$MP2_5, lambda_opt, offset)

# 4. Ajustar el nuevo modelo con la respuesta transformada
# Usamos las mismas predictoras que eligió el backward, pero cambiamos la Y
formula_final <- formula(modelo_backward)
# Reemplazamos la variable respuesta en la fórmula (truco de R)
formula_bc <- update(formula_final, MP2_5_bc ~ .)

modelo_final_bc <- lm(formula_bc, data = train_data)

# Ver resultados
summary(modelo_final_bc)

# 5. Diagnóstico rápido: ¿Mejoraron los residuos?
par(mfrow = c(1, 2))
qqnorm(residuals(modelo_backward), main = "QQ-Plot: Modelo Original", pch=20); qqline(residuals(modelo_backward), col="red")
qqnorm(residuals(modelo_final_bc), main = "QQ-Plot: Modelo Box-Cox", pch=20); qqline(residuals(modelo_final_bc), col="red")
par(mfrow = c(1, 1))











