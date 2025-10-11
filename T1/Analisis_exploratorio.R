library(readxl)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
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

# a) Análisis exploratorio

# Resúmenes básicos:

names(datos_diarios)   # <- Nombres de las variables
head(datos_diarios)    # <- Visualizar los primeros 6 datos
tail(datos_diarios)    # <- Visualizar los últimos 6 datos
dim(datos_diarios)     # <- Retorna número de observaciones y número de variables

# Summary - Info. básica sobre variables:
summary(datos_diarios$bikers)          # <- Variable a predecir
summary(datos_diarios$temp_range)      # <- Ejemplo de potencial regresor

# Descriptives - Retorna resumen para variables numéricas
descriptives <- datos_diarios %>% 
  select(-date) %>%
  psych::describe()
View(descriptives)

# Gráficos - Variable objetivo y regresores en el tiempo
p1 <- ggplot(datos_diarios, aes(x = date, y = bikers)) +
  geom_line(color = "#87CEFA") + 
  labs(
    title = "Número de ciclistas en el tiempo",
    x = "Fecha",
    y = "Número de ciclistas"
  ) +
  theme_minimal()
p1

ggsave("serie_temporal_bikers.png", p1, width = 10, height = 6, dpi = 300)

p2 <- ggplot(datos_diarios, aes(x = date, y = temp_max)) +
  geom_line(color = "#98FB98") +  
  labs(
    title = "Temperatura máxima diaria",
    x = "Fecha",
    y = "Temperatura (°C)"
  ) +
  theme_minimal()
p2

ggsave("serie_temporal_temp_max.png", p2, width = 10, height = 6, dpi = 300)

# Boxplot 
# por mes (outliers)
p3 <- ggplot(datos_diarios, aes(x = factor(mnth), y = bikers)) +
  geom_boxplot(fill = "#87CEFA", color = "#4682B4", outlier.color = "#FFB6C1", outlier.shape = 16) +  
  labs(
    title = "Distribución de arriendos diarios por mes",
    subtitle = "Número de ciclistas según el mes del año",
    x = "Mes",
    y = "Número de ciclistas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 0)
  )
p3

ggsave("boxplot_mes.png", p3, width = 10, height = 6, dpi = 300)

# por día de la semana y "temporada" no es muy explicativo 
png("tres_boxplots.png", width = 15, height = 6, units = "in", res = 300)
par(mfrow = c(1, 3))

boxplot(bikers ~ mnth, data = datos_diarios, 
        main = "Bikers por mes", xlab = "Mes", ylab = "Arriendos diarios",
        col = "#87CEFA", range = 1.5, 
        outcol = "#FFB6C1", pch = 16, outcex = 0.8)  

boxplot(bikers ~ weekday, data = datos_diarios, 
        main = "Bikers por día de la semana", xlab = "Día", ylab = "",
        col = "#98FB98", range = 1.5,  
        outcol = "#FFB6C1", pch = 16, outcex = 0.8)  

boxplot(bikers ~ weathersit, data = datos_diarios, 
        main = "Bikers según el clima", xlab = "Condición climática", ylab = "",
        col = "#FFB6C1", range = 1.5,  
        outcol = "#87CEFA", pch = 16, outcex = 0.8) 

par(mfrow = c(1, 1)) 
dev.off()


# Matriz de correlación 
num_df <- datos_diarios %>%
  select(-date) %>%             
  select(where(is.numeric))      
corr_mat <- cor(num_df, use = "pairwise.complete.obs")

p <- ggcorrplot(
  corr_mat,
  hc.order = TRUE,      
  #type = "lower",       
  lab = TRUE,          
  lab_size = 2.8,       
  digits = 2,           
  colors = c("#87CEFA", "#FFFFFF", "#FFB6C1"), 
  outline.color = "gray85",
  ggtheme = theme_minimal()
) +
  labs(title = "Mapa de correlaciones entre variables numéricas") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9)
  )

p

ggsave("mapa_correlaciones.png", p, width = 12, height = 10, dpi = 300)