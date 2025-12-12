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
## PCA para variables de vel viento ##
####################################
vel_viento <- train_data %>% dplyr::select(velocidad_viento_mean, velocidad_viento_max, velocidad_viento_min)
pca_vel_viento <- prcomp(vel_viento, scale. = TRUE)
train_data$vel_viento_pca <- pca_vel_viento$x[, 1]
train_data <- train_data %>% dplyr::select(-velocidad_viento_mean, -velocidad_viento_max, -velocidad_viento_min)
pca_vel_viento$rotation

names(train_data)

#########################
## Modelo con Backward ##
#########################
names(train_data)
modelo_inicial <- lm(MP2_5 ~ fecha + precipitacion_mean + precipitacion_max + direccion_viento_mean + direccion_viento_max + direccion_viento_min + humidity_pca + vel_viento_pca, 
                     data = train_data)
modelo_backward <- step(modelo_inicial, direction = "backward")

summary(modelo_inicial) 
summary(modelo_backward) # lm(formula = MP2_5 ~ temp_pca + direccion_viento_mean + direccion_viento_max + precipitacion_max, data = train_data)

## QQ plot para residuos del modelo_backward
plot(modelo_backward)


## modelo_backward tiene un muy buen R2 ajustado y todos los predictores son significativos, pero 
## los residuos no son normales (ver QQ-Plot más abajo). Por lo tanto, intentaremos una transformación Box-Cox.

##############################
### Transformación Box-Cox ### 
##############################

# trasforamcion a predictores 


# transformacion a MP2_5

#######################################################
##       MODELO 2: Transformación Box-Cox en Y       ##
#######################################################

# 1. Definir el offset para evitar ceros
offset_y <- 0.1  

# --- CORRECCIÓN: Ajustar modelo temporal para Box-Cox ---
# Creamos una variable temporal positiva en train_data para que boxcox() no falle
train_data$MP2_5_pos <- train_data$MP2_5 + offset_y

# Actualizamos el modelo backward para usar esta nueva variable positiva

modelo_temp_boxcox <- update(modelo_backward, MP2_5_pos ~ ., data = train_data)


bc <- boxcox(modelo_temp_boxcox, plotit = TRUE, lambda = seq(-0.5, 1, by = 0.1))
lambda_opt <- bc$x[which.max(bc$y)]
print(paste("Lambda óptimo encontrado:", round(lambda_opt, 3)))

train_data$MP2_5_bc <- forecast::BoxCox(train_data$MP2_5 + offset_y, lambda = lambda_opt)
modelo_boxcox_Y <- lm(MP2_5_bc ~ temp_pca + direccion_viento_mean + direccion_viento_max + precipitacion_max, data = train_data)


summary(modelo_boxcox_Y)
summary(modelo_backward)

# 1. Configurar cuadrícula 2x2 y espacio superior para el título (oma)
par(mfrow = c(2, 2), oma = c(0, 0, 6, 0))

plot(modelo_backward)
mtext("Diagnósticos: Modelo Backward", outer = TRUE, cex = 1.2, line = 2, font = 2)

plot(modelo_boxcox_Y)
mtext("Diagnósticos: Modelo Box-Cox", outer = TRUE, cex = 1.2, line = 2, font = 2)

par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# Verificación de normalidad
par(mfrow=c(1,2))
qqnorm(residuals(modelo_backward), main="Modelo 1 (Base)", pch=20); qqline(residuals(modelo_backward), col="red")
qqnorm(residuals(modelo_boxcox_Y), main="Modelo 2 (Box-Cox Y)", pch=20); qqline(residuals(modelo_boxcox_Y), col="red")
par(mfrow=c(1,1))
