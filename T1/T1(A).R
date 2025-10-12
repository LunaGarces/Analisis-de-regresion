library(dplyr)
library(lubridate)
library(readxl) 
library(rio)        # <- Útil para importar datos
library(car)        # <- Cálculo del VIF y multicolinealidad
library(lmtest)     # <- Necesaria para test de Breusch-Pagan
library(corrplot)   # <- Gráficos matrices de correlación
library(Metrics) 

# --- Importar datos ---
datos <- rio::import(file.choose())
head(datos)

# meses a nuemros
meses <- c("Jan", "Feb", "March", "April", "May", "June", "July",
           "Aug", "Sept", "Oct", "Nov", "Dec")
datos$mnth <- as.numeric(factor(datos$mnth, 
                                levels = meses)) 

Mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  names(sort(table(x), decreasing = TRUE))[1]
}

dias <- datos %>%
  group_by(day, mnth, season) %>%
  summarise(
    holiday = mean(holiday),
    weekday = mean(weekday),
    workingday = mean(workingday),
    weathersit_mode = Mode(weathersit),
    temp = mean(temp),
    atemp = mean(atemp),
    hum = mean(hum),
    windspeed = mean(windspeed),
    casual = sum(casual),
    registered = sum(registered),
    bikers = sum(bikers)
  )

head(dias)
df <- subset(dias, select = -c(casual, registered))


# Partición de entrenamiento y prueba:

df$partition <- NA  # <- Creamos una nueva columna con NA's

# Armamos una muestra aleatoria para asignar data de entrenamiento y de prueba:

n <- nrow(df)
s <- sample(c(1:n),round(n*0.8,digits=0),replace=FALSE)
df$partition[s] <- 'train'  # <- Rellenamos el 80% con entrenamiento

for(i in 1:n){
  if(is.na(df$partition[i]) == TRUE){
    df$partition[i] <- 'test'
  }
}  # <- Rellenamos el 20% restante con prueba

# Almacenamos la data correspondiente:    

data <- subset(df , subset = partition == 'train')
prueba <- subset(df , subset = partition == 'test')

sapply(data, function(x) length(unique(x)))

data <- subset(data, select = -partition)
prueba <- subset(prueba, select = -partition)


# a) Análisis exploratorio


# Resúmenes básicos:

names(data)   # <- Nombres de las variables
head(data)    # <- Visualizar los primeros 6 datos
tail(data)    # <- Visualizar los últimos 6 datos
dim(data)     # <- Retorna número de observaciones y número de variables

# Summary - Info. básica sobre variables:

summary(data$bikers)  # <- Variable a predecir

# Descriptives - Retorna resumen para variables numéricas

descriptives <- psych::describe(dplyr::select(data, where(is.numeric)))# <- Saco las variables no numéricas antes de usarlo
View(descriptives)

# Gráficos 

plot(ts(data$bikers))   # <- Vemos como cambia en el tiempo la variable objetivo

boxplot(data$bikers , horizontal = TRUE)   # <- ver el comportamiento de la respuesta y ver al ojo valores atípicos
stripchart(data$bikers, method = "jitter", pch = 19, add = TRUE, col = "blue")

# Visualmente,no tiene datos atípicos. Igual se puede ver el rango intercuartil.
# De todos modos, podemos hacer más preciso el análisis y ver por fechas más detalladas:

boxplot(bikers ~ season, data = data, range = 1.5, pch = 20)
boxplot(bikers ~ mnth, data = data, range = 1.5, pch = 20)
boxplot(bikers ~ weekday, data = data, range = 1.5, pch = 20)

# b) Selección de variables

# Buen paso para empezar -> Ver correlación entre respuesta y predictores candidatos:

data_numeric <- data[ , sapply(data , is.numeric)]  # <- Saco las variables numéricas
cor.base <- cor(data_numeric)                       # <- Cargamos la matriz de correlación      
corrplot::corrplot(cor.base,method = "number")      # <- Imprimimos la matriz con un plot

# Podemos probar un modelo lineal simple para ver cómo nos va:

mod2 <- lm(bikers ~ atemp, data)
summary(mod2) # <- Buen ajuste para empezar, notar que r^2 = 0.6101

mod3 <- lm(bikers ~ temp, data)
summary(mod3) # <- Buen ajuste para empezar, notar que r^2 = 0.5991


modelo_completo <- lm(bikers ~ ., data = data)

modelo_nulo <- lm(bikers ~ 1, data = data)

modelo_final_forward <- step(modelo_nulo, 
                             scope = list(lower = ~1, upper = formula(modelo_completo)), 
                             direction = "forward", trace = 0)

modelo_final_back <- step(modelo_completo, direction = "backward")

summary(modelo_final_forward) #r2 = 0.7816
summary(modelo_final_back) #r2 = 0.7827

vif(modelo_final_forward)
vif(modelo_final_back)

# c) Ajuste del modelo y supuestos regresión (se hará con el modelo obtenido por Backward - pero la aplicación es análoga)

# Ajuste:
summary(modelo_final_back)$adj.r.squared # 0.7781

## supuestos:

res <- residuals(modelo_final_back)

## Normalidad:
shapiro.test(res)

qqnorm(residuals(modelo_final_back))
qqline(residuals(modelo_final_back))

## Homocedasticidad
bptest(modelo_final_back)
plot(modelo_final_back)


## Independencia:
dwtest(modelo_final_back)

# No se cumple ningun supuesto de la regresión, esto es algo común en la práctica y existen métodos para mejorar el modelo y que se cumplan finalmente los supuestos pero son cosas que uds aun no han visto

# Igual no es necesario que se cumplan todos los supuestos de regresión solo que los resultados podrían ser no muy fiables en caso de que no se cumplan.
# d) Evaluación con data de entrenamiento:

R2 <- summary(modelo_final_back)$r.squared              # <- 0.91
r2 <- summary(modelo_final_back)$adj.r.squared          # <- 0.90
RMSE <- sqrt(mean(residuals(modelo_final_back)^2))      # <- 22
MAE <- mean(abs(residuals(modelo_final_back)))          # <- 15.81

# OJO! Si queremos usar el MAPE, debemos asegurarnos que la variable respuesta no tenga ceros

P <- predict(modelo_final_back , newdata = data , drop = F)
R <- data$bikers

P == 0  
R == 0   # <- Notemos que el dato 214 es 0

mape(R,P) # <- Mape 21%

# e) Evaluación con data de prueba - Calculamos MAPE

P <- predict(modelo_final_back , newdata = prueba , drop = F)
R <- prueba$bikers

mape(R,P) # <- Mape 18% - Importante analizar el resultado!! (es mejorable? ; captura bien la variabilidad? ; no está sobre-ajustado?)

# f) Volvemos a revisar summary, mape, supuestos (básicamente, concluímos)

coef_summary <- summary(modelo_final_back)$coefficients
conf_int <- confint(modelo_final_back, level = 0.95)

# Crear tabla de resultados
resultados <- data.frame(
  Variable = rownames(coef_summary),
  Coeficiente = coef_summary[, 1],
  SE = coef_summary[, 2],
  p_valor = coef_summary[, 4],
  LI_95 = conf_int[, 1],
  LS_95 = conf_int[, 2]
)
resultados
