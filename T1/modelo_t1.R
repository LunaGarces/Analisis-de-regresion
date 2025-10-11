library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)
library(corrplot)
library(lmtest)
library(car)
library(nortest)
library(lubridate)

# --- Importar datos ---
datos <- rio::import(file.choose())


## SEPARACIÓN DE DATOS Y TRABAJO DE DATOS 

# meses a nuemros
meses <- c("Jan", "Feb", "March", "April", "May", "June", "July",
           "Aug", "Sept", "Oct", "Nov", "Dec")
datos$mnth <- as.numeric(factor(datos$mnth, 
                                levels = meses)) 

estados <- c('clear', 'cloudy/misty', 'light rain/snow', 'heavy rain/snow')

datos$weathersit <- factor(
  datos$weathersit,
  levels = estados,
  ordered = TRUE
)

# --- Agrupar de datos por día ---

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# DESPUÉS (correcto):
datos <- datos %>%
  mutate(
    date = as.Date(day - 1, origin = "2011-01-01")
  )

# Agrupar por día 
datos_diarios <- datos %>%
  group_by(date) %>%
  summarise(
    # --- calendario ---
    mnth        = month(first(date)),                # conservar el mes
    day_of_year = yday(first(date)),                 
    weekday     = wday(first(date), week_start = 1), # [1,..,7]
    
    # --- bools ---
    holiday     = as.integer(any(holiday == 1, na.rm = TRUE)),
    workingday  = as.integer(any(workingday == 1, na.rm = TRUE)),
    
    # Humedad 
    hum_avg     = mean(hum, na.rm = TRUE),
    hum_min     = min(hum, na.rm = TRUE),
    hum_max     = max(hum, na.rm = TRUE),
    
    
    # --- promedios diario ---
    # Temperaturas
    
    temp_avg    = mean(temp, na.rm = TRUE),
    temp_max    = max(temp, na.rm = TRUE),
    temp_min    = min(temp, na.rm = TRUE),
    temp_range  = max(temp, na.rm = TRUE) - min(temp, na.rm = TRUE),
    
    # Sensación térmica
    atemp_avg   = mean(atemp, na.rm = TRUE),
    atemp_max   = max(atemp, na.rm = TRUE),
    
    windspeed   = mean(windspeed, na.rm = TRUE),
    
    # --- Condición climática (categoría más frecuente) ---
    weathersit  = get_mode(weathersit),
    
    # --- sumas diarias ---
    casual      = sum(casual, na.rm = TRUE),
    registered  = sum(registered, na.rm = TRUE),
    bikers      = sum(bikers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mnth = as.integer(mnth),
    day_of_year = as.integer(day_of_year),
    weekday = as.integer(weekday),
    weathersit = factor(
      weathersit,
      levels = estados,
      ordered = FALSE 
    )
  )

# Vista rápida del nuevo dataset
glimpse(datos_diarios)



sum(is.na(datos_diarios$date))
datos_diarios <- datos_diarios %>% filter(!is.na(date))

dim(datos_diarios)
dim(datos)


# Partición de entrenamiento y prueba:

datos_diarios$partition <- NA  # <- Creamos una nueva columna con NA's

# data de entrenamiento y de prueba:
n <- nrow(datos_diarios)
s <- sample(c(1:n),round(n*0.8,digits=0),replace=FALSE)
datos_diarios$partition[s] <- 'train'  # <- Rellenamos el 80% con entrenamiento

for(i in 1:n){
  if(is.na(datos_diarios$partition[i]) == TRUE){
    datos_diarios$partition[i] <- 'test'
  }
}  # <- Rellenamos el 20% restante con prueba

#guardamos
train <- subset(datos_diarios , subset = partition == 'train')
test <- subset(datos_diarios , subset = partition == 'test')


## MOODELOS 
# columnas candidatas: todas menos la respuesta y solo numéricas
cols <- setdiff(names(train), c("bikers", "casual", "registered"))
cols <- cols[sapply(train[cols], is.numeric)]

col_max <- NA_character_
r2_max  <- -Inf

for (nm in cols) {
  # filtra filas completas para esa X y la respuesta
  df_sub <- train[, c("bikers", nm)]
  df_sub <- stats::na.omit(df_sub)
  
  # salta si quedó sin datos suficientes
  if (nrow(df_sub) < 2) next
  
  # arma fórmula: bikers ~ <nm>
  f <- reformulate(nm, response = "bikers")
  
  mod <- lm(f, data = df_sub)
  r2  <- summary(mod)$r.squared
  
  if (is.finite(r2) && r2 > r2_max) {
    r2_max  <- r2
    col_max <- nm
  }
}

col_max
r2_max


# Empezar con la mejor variable individual
current_vars <- c(col_max)
best_adj_r2 <- summary(lm(reformulate(current_vars, "bikers"), data = train))$adj.r.squared
improved <- TRUE

cat("Paso 0 - Modelo inicial:\n")
cat("Variables:", paste(current_vars, collapse = ", "), "\n")
cat("R² ajustado:", round(best_adj_r2, 5), "\n\n")

# Variables candidatas 
candidate_vars <- setdiff(
  names(train), 
  c("bikers", "casual", "registered", "date", "partition", "day_of_year")
)

# Remover variables no numéricas o factores con muchos niveles
candidate_vars <- candidate_vars[sapply(train[candidate_vars], function(x) {
  is.numeric(x) || (is.factor(x) && nlevels(x) <= 12)
})]

step <- 1

while(improved && length(candidate_vars) > 0) {
  improved <- FALSE
  best_candidate <- NULL
  best_new_adj_r2 <- best_adj_r2
  
  for (var in candidate_vars) {
    # Probar agregar esta variable
    try_vars <- c(current_vars, var)
    formula <- reformulate(try_vars, "bikers")
    
    mod <- lm(formula, data = train)
    adj_r2 <- summary(mod)$adj.r.squared
    
    if (is.finite(adj_r2) && adj_r2 > best_new_adj_r2 + 1e-6) {
      best_new_adj_r2 <- adj_r2
      best_candidate <- var
    }
  }
  
  if (!is.null(best_candidate)) {
    current_vars <- c(current_vars, best_candidate)
    candidate_vars <- setdiff(candidate_vars, best_candidate)
    best_adj_r2 <- best_new_adj_r2
    improved <- TRUE
    
    cat("Paso", step, "- Mejor candidato:", best_candidate, "\n")
    cat("Variables:", paste(current_vars, collapse = ", "), "\n")
    cat("R² ajustado:", round(best_adj_r2, 5), "\n\n")
    step <- step + 1
  }
}

cat("=== MODELO FINAL ===\n")
cat("Variables seleccionadas:", paste(current_vars, collapse = ", "), "\n")
cat("R² ajustado final:", round(best_adj_r2, 5), "\n")

modelo_final <- lm(reformulate(current_vars, "bikers"), data = train)
vif(modelo_final)
summary(modelo_final)


#Modelo sin tanta colinealidad (MODIFICRA SI modelo_final CAMBIA)
modelo_simplificado <- lm(
  bikers ~ atemp_avg + hum_min + windspeed + weathersit + mnth + holiday,
  data = train
)

summary(modelo_simplificado)
vif(modelo_simplificado)

test$pred <- predict(modelo_simplificado, newdata = test)

R2_test <- 1 - sum((test$bikers - test$pred)^2) / sum((test$bikers - mean(test$bikers))^2)
cat("R² en test:", round(R2_test, 3), "\n")


ggplot(test, aes(x = bikers, y = pred)) +
  geom_point(color = "#0fc48b", alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Predicción vs Observación (Datos de Test)",
    subtitle = paste0("R² en test = ", round(R2_test, 3)),
    x = "Número de ciclistas observados",
    y = "Número de ciclistas predichos"
  ) +
  theme_minimal(base_size = 12)


# Falta agregar lo de MSE, MAE y MAPE





