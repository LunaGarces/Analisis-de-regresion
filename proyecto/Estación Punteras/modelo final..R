# ==============================================================================
#  PROYECTO FINAL INTEGRADO: PREDICCIÓN MP2.5
#  (PCA + LAG + BOX-COX + COMPARACIÓN DE MODELOS)
# ==============================================================================

# 1. Librerías
library(readr)
library(rio)
library(dplyr)
library(lubridate)
library(corrplot)
library(MASS)    
library(Metrics) 
library(car)      # Fundamental para powerTransform y bcPower

# ------------------------------------------------------------------------------
# 2. CARGA Y PREPROCESAMIENTO
# ------------------------------------------------------------------------------
data_met <- read_csv("datos_completos_estacion_punteras.csv") 

# Eliminamos contaminantes que no son el objetivo
data_met <- data_met %>% dplyr::select(-c("N02", "MP10", "NO"))

# --- INGENIERÍA DE CARACTERÍSTICAS (LAG) ---
# IMPORTANTE: Ordenamos por fecha ANTES de crear el lag y antes del split
data_met$fecha <- as.Date(data_met$fecha) 
data_met <- data_met %>% arrange(fecha)

# Crear columna con el MP2.5 de ayer (Lag 1)
data_met$MP2_5_anterior <- dplyr::lag(data_met$MP2_5, n = 1)

# Eliminar filas con NA (la primera fila queda vacía por el lag)
data_met <- na.omit(data_met)

set.seed(123)

# Separación Train (80%) / Test (20%)
train_indices <- sample(1:nrow(data_met), size = 0.8 * nrow(data_met))
train_data <- data_met[train_indices, ]
test_data  <- data_met[-train_indices, ]


# ------------------------------------------------------------------------------
# 3. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# ------------------------------------------------------------------------------

# PCA Temperatura
temps <- train_data %>% dplyr::select(temperatura_mean, temperatura_max, temperatura_min)
pca_temp <- prcomp(temps, scale. = TRUE)
train_data$temp_pca <- pca_temp$x[, 1]

# PCA Humedad
hume <- train_data %>% dplyr::select(humedad_mean, humedad_max, humedad_min)
pca_humi <- prcomp(hume, scale. = TRUE)
train_data$humidity_pca <- pca_humi$x[, 1]

# PCA Velocidad Viento
vel_viento <- train_data %>% dplyr::select(velocidad_viento_mean, velocidad_viento_max, velocidad_viento_min)
pca_vel_viento <- prcomp(vel_viento, scale. = TRUE)
train_data$vel_viento_pca <- pca_vel_viento$x[, 1]

# Eliminamos las originales para evitar colinealidad en los modelos
train_data <- train_data %>% 
  dplyr::select(-temperatura_mean, -temperatura_max, -temperatura_min,
                -humedad_mean, -humedad_max, -humedad_min,
                -velocidad_viento_mean, -velocidad_viento_max, -velocidad_viento_min)


# ------------------------------------------------------------------------------
# 4. MODELO 1: BASE (Sin Transformación Box-Cox)
# ------------------------------------------------------------------------------
print(">>> GENERANDO MODELO 1 (BASE)...")

# Ajustamos modelo con variables originales (MP2_5 sin tocar)
modelo_base <- lm(MP2_5 ~ temp_pca + humidity_pca + vel_viento_pca + 
                    precipitacion_mean + precipitacion_max + 
                    direccion_viento_mean + direccion_viento_max + 
                    direccion_viento_min + 
                    MP2_5_anterior, 
                  data = train_data)

# Selección Backward
modelo_base_final <- step(modelo_base, direction = "backward", trace = 0)

print("--- Resumen Modelo 1 (Base) ---")
summary(modelo_base_final)

# --- Test de Ljung-Box (Independencia Temporal) ---
# H0: Ruido Blanco (Bueno) | H1: Autocorrelación (Malo)
lb_test <- Box.test(residuals(modelo_base_final), type = "Ljung-Box")
print("--- Test Ljung-Box (P-value > 0.05 indica independencia) ---")
print(lb_test)


# ------------------------------------------------------------------------------
# 5. TRANSFORMACIÓN BOX-COX
# ------------------------------------------------------------------------------
print(">>> APLICANDO TRANSFORMACIONES...")

# Función auxiliar para manejar ceros
calc_boxcox_var <- function(variable) {
  shift <- if(min(variable, na.rm = TRUE) <= 0) abs(min(variable, na.rm = TRUE)) + 0.001 else 0
  pt <- powerTransform(variable + shift)
  return(bcPower(variable + shift, pt$lambda))
}

# A) Transformar Predictores
train_data$precip_mean_bc <- calc_boxcox_var(train_data$precipitacion_mean)
train_data$precip_max_bc  <- calc_boxcox_var(train_data$precipitacion_max)
train_data$dir_mean_bc    <- calc_boxcox_var(train_data$direccion_viento_mean)
train_data$dir_max_bc     <- calc_boxcox_var(train_data$direccion_viento_max)
train_data$dir_min_bc     <- calc_boxcox_var(train_data$direccion_viento_min)
train_data$lag_bc         <- calc_boxcox_var(train_data$MP2_5_anterior)

# B) Transformar Variable Respuesta (Y)
# Calculamos lambda óptimo para MP2.5
offset_y <- if(min(train_data$MP2_5) <= 0) abs(min(train_data$MP2_5)) + 0.001 else 0
bc_obj <- powerTransform(train_data$MP2_5 + offset_y)
lambda_opt <- bc_obj$lambda

print(paste("Lambda óptimo para MP2.5:", round(lambda_opt, 3)))

# Crear variable transformada
train_data$MP2_5_bc <- bcPower(train_data$MP2_5 + offset_y, lambda_opt)


# ------------------------------------------------------------------------------
# 6. MODELO 2: OPTIMIZADO (Con Box-Cox)
# ------------------------------------------------------------------------------
print(">>> GENERANDO MODELO 2 (BOX-COX)...")

# Ajustamos modelo con variables TRANSFORMADAS (_bc)
modelo_bc <- lm(MP2_5_bc ~ temp_pca + humidity_pca + vel_viento_pca + 
                  precip_mean_bc + precip_max_bc + 
                  dir_mean_bc + dir_max_bc + dir_min_bc + 
                  lag_bc, 
                data = train_data)

# Selección Backward
modelo_bc_final <- step(modelo_bc, direction = "backward", trace = 0)

print("--- Resumen Modelo 2 (Box-Cox) ---")
summary(modelo_bc_final)


# ------------------------------------------------------------------------------
# 7. COMPARACIÓN FINAL (GRÁFICOS)
# ------------------------------------------------------------------------------

# A) Comparación de Residuos (QQ-Plots)
# Esto muestra qué tan bien se ajustan los errores a una distribución normal
par(mfrow = c(1, 2))
qqnorm(residuals(modelo_base_final), main="Modelo 1 (Base)\nSin Transformar", pch=20, col="gray")
qqline(residuals(modelo_base_final), col="red", lwd=2)

qqnorm(residuals(modelo_bc_final), main="Modelo 2 (Optimizado)\nCon Box-Cox", pch=20, col="blue")
qqline(residuals(modelo_bc_final), col="red", lwd=2)
par(mfrow = c(1, 1))

# B) Diagnósticos Completos
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo_bc_final)
mtext("Diagnóstico Completo: Modelo Final Box-Cox", outer = TRUE, cex = 1.2, font = 2)
par(mfrow = c(1, 1))
