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
