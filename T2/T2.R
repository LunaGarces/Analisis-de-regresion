library(dplyr)
library(corrplot)
library(MASS) 
library(lmtest) 
library(forecast)
library(sandwich)
library(lubridate)
library(ggplot2)
library(readxl)
library(car)

data <- read_excel("Tarea_2_2025_02.xlsx")
data$date <- as.Date(data$date, format="%Y-%m-%d")
head(data)

data_2019 <- read_excel("Tarea_2_2025_02.xlsx", sheet = 2)
data_2019$date <- as.Date(data_2019$date, format="%Y-%m-%d")
head(data_2019)

# Variable a predecir sera "comercial"
set.seed(123)

#sacar granel e industrial 
df_final <- data %>%
  dplyr::select(-granel, -industrial)
colnames(df_final)[colnames(df_final) == "comercial"] <- "Y"


# Lista completa de feriados (Fijos y Móviles aproximados para Chile 2013-2019)
feriados_completa <- as.Date(c(
  "2013-01-01", "2013-03-29", "2013-05-01", "2013-05-21", "2013-06-29", "2013-07-16", "2013-08-15", "2013-09-18", "2013-09-19", "2013-09-20", "2013-10-12", "2013-10-31", "2013-11-01", "2013-12-08", "2013-12-25",
  "2014-01-01", "2014-04-18", "2014-05-01", "2014-05-21", "2014-06-29", "2014-07-16", "2014-08-15", "2014-09-18", "2014-09-19", "2014-10-31", "2014-11-01", "2014-12-08", "2014-12-25",
  "2015-01-01", "2015-04-03", "2015-05-01", "2015-05-21", "2015-06-29", "2015-07-16", "2015-08-15", "2015-09-18", "2015-09-19", "2015-10-12", "2015-10-31", "2015-11-01", "2015-12-08", "2015-12-25",
  "2016-01-01", "2016-03-25", "2016-05-01", "2016-05-21", "2016-06-27", "2016-07-16", "2016-08-15", "2016-09-19", "2016-10-10", "2016-10-31", "2016-11-01", "2016-12-08", "2016-12-25",
  "2017-01-01", "2017-04-14", "2017-04-19", "2017-05-01", "2017-05-21", "2017-06-26", "2017-07-16", "2017-08-15", "2017-09-18", "2017-09-19", "2017-10-09", "2017-10-27", "2017-11-01", "2017-12-08", "2017-12-25",
  "2018-01-01", "2018-03-30", "2018-05-01", "2018-05-21", "2018-07-02", "2018-07-16", "2018-08-15", "2018-09-17", "2018-09-18", "2018-09-19", "2018-10-15", "2018-11-01", "2018-11-02", "2018-12-08", "2018-12-25",
  "2019-01-01", "2019-04-19", "2019-05-01", "2019-05-21", "2019-06-29", "2019-07-16", "2019-08-15", "2019-09-18", "2019-09-19", "2019-09-20", "2019-10-12", "2019-10-31", "2019-11-01", "2019-12-08", "2019-12-25"
))

df_final <- df_final %>%
  mutate(es_feriado = ifelse(date %in% feriados_completa, 1, 0))
table(df_final$es_feriado) 
df_final$dia_semana <- wday(df_final$date, label = TRUE, abbr = FALSE)

data_2019 <- data_2019 %>%
  dplyr::select(-granel, -industrial)
colnames(data_2019)[colnames(data_2019) == "comercial"] <- "Y"
data_2019 <- data_2019 %>%
  mutate(es_feriado = ifelse(date %in% feriados_completa, 1, 0))
data_2019$dia_semana <- wday(data_2019$date, label = TRUE, abbr = FALSE)

# div train test 80% y 20%
train_indices <- sample(1:nrow(df_final), size = 0.8 * nrow(df_final))
train_data <- df_final[train_indices, ]
test_data <- df_final[-train_indices, ]


#############################
### Analisis exploratorio ###
#############################

sd_Y <- sd(train_data$Y, na.rm = TRUE)
sd_Y

# Dsitribución de la variable Y
g2 <- ggplot(train_data, aes(x = Y)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "white") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribución de la Demanda Comercial",
       subtitle = "Verificar asimetría para posible transformación Box-Cox",
       x = "Demanda comercial", y = "Densidad") +
  theme_minimal()
print(g2)

# Demanda por mes (se pueden ver muchos outliers, probables feriados)
g_boxplot <- ggplot(train_data, aes(x = factor(month), y = Y)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribución de la Demanda Comercial por Mes",
       subtitle = "Observe cómo aumenta la demanda y la variabilidad en meses de invierno",
       x = "Mes",
       y = "Demanda Comercial (kg)") +
  theme_minimal()
print(g_boxplot)  

# Relación Y vs temp_avg
ggplot(train_data, aes(x = temp_avg, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Relación entre Temperatura (AVG) y Demanda") +
  theme_minimal()

#correlación entre variables numéricas
vars_correlacion <- df_final %>%
  dplyr::select(Y, 
                temp_avg, temp_min, temp_max, 
                lluvia, 
                humidity_avg, humidity_max, humidity_min, 
                cloudcov_avg, cloudcov_max, cloudcov_min, 
                visibility_avg, visibility_max, visibility_min)
matriz_cor <- cor(vars_correlacion, use = "complete.obs")
cor_ordenada <- sort(matriz_cor["Y", ], decreasing = FALSE) 
corrplot::corrplot(matriz_cor, type = "full", method = "color",
                   tl.col = "black",
                   title = "Matriz de Correlación", mar = c(0,0,2,0),
                   addCoef.col = "black",  
                   number.cex = 0.8)
# las var de temp, humi, cloud y vis tienen correalción muy alta entre ellas 
#(avg, max, min) por ende aplicare PCA para cada una

######################################
## PCA para variables de temperatura##
######################################
temps <- train_data %>% dplyr::select(temp_avg, temp_max, temp_min)
pca_temp <- prcomp(temps, scale. = TRUE)
train_data$temp_pca <- pca_temp$x[, 1] #Valores ALTOS de temp_pca significan FRÍO
train_data <- train_data %>% dplyr::select(-temp_avg, -temp_max, -temp_min)
pca_temp$rotation

####################################
## PCA para variables de humidity ##
####################################
humi <- train_data %>% dplyr::select(humidity_avg, humidity_max, humidity_min)
pca_humi <- prcomp(humi, scale. = TRUE)
train_data$humidity_pca <- pca_humi$x[, 1]
train_data <- train_data %>% dplyr::select(-humidity_avg, -humidity_max, -humidity_min)

####################################
## PCA para variables de cloudcov ##
####################################
cloud <- train_data %>% dplyr::select(cloudcov_avg, cloudcov_max, cloudcov_min)
pca_cloud <- prcomp(cloud, scale. = TRUE)
train_data$cloudcov_pca <- pca_cloud$x[, 1]
train_data <- train_data %>% dplyr::select(-cloudcov_avg, -cloudcov_max, -cloudcov_min)

######################################
## PCA para variables de visibility ##
######################################
visib <- train_data %>% dplyr::select(visibility_avg, visibility_max, visibility_min)
# Hay 6 nan, los cambio por el promedio de visibilidad
visib <- visib %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
pca_visib <- prcomp(visib, scale. = TRUE)
train_data$visibility_pca <- pca_visib$x[, 1]
train_data <- train_data %>% dplyr::select(-visibility_avg, -visibility_max, -visibility_min)



# Verificamos la nueva matriz de correlaciones
vars_correlacion <- train_data %>%
  dplyr::select(Y, 
                temp_pca, 
                lluvia,
                humidity_pca,
                cloudcov_pca,
                visibility_pca)
matriz_cor <- cor(vars_correlacion, use = "complete.obs")
cor_ordenada <- sort(matriz_cor["Y", ], decreasing = FALSE) 
corrplot::corrplot(matriz_cor, type = "full", method = "color",
                   tl.col = "black",
                   title = "Matriz de Correlación", mar = c(0,0,2,0),
                   addCoef.col = "black",  
                   number.cex = 0.8) 
names(train_data) # no elimine las columnas originales de temp, humi, cloud y visib por que dsp hay que uyilizar back o fordward

# Relación Y vs temp_pca
ggplot(train_data, aes(x = temp_pca, y = Y)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Relación entre Temperatura (PCA) y Demanda") +
  theme_minimal()


##############################
### Transformación Box-Cox ### 
##############################

#Tomare los regresores a los que le les aplico pca, ya que no afecta en significancia el valor de lambda

# Box-Cox solo admite valores positivos, por lo que sumamos 1 a Y
train_data$Y_adj <- train_data$Y + 1
mod_base <- lm(Y_adj ~ temp_pca + lluvia + humidity_pca  + visibility_pca + cloudcov_pca + es_feriado+factor(month) + factor(dia_semana), 
               data = train_data)

# Encontrar lambda óptimo
lambdas <- MASS::boxcox(mod_base, lambda = seq(0.3, 0.5, 0.001), plotit = TRUE)
lambda_optimo <- lambdas$x[lambdas$y == max(lambdas$y)]
lambda_optimo
lambda_elegido <- lambda_optimo

# Transformamos Y_adj 
train_data$Y_trans <- forecast::BoxCox(train_data$Y_adj, lambda = lambda_elegido)

# Ajustamos el modelo final con la Y transformada
modelo_final <- lm(Y_trans ~ temp_pca + lluvia + humidity_pca + visibility_pca+ cloudcov_pca + es_feriado + factor(month)+ factor(dia_semana), 
                   data = train_data)

#Visualizar la mejora 
par(mfrow=c(1,2))
hist(train_data$Y, main="Original (Asimétrica)", col="gray", border="white", xlab="Demanda")
hist(train_data$Y_trans, main="Transformada Box-Cox (Más Normal)", col="green", border="white", xlab="Demanda Transformada")

qqPlot(mod_base$residuals, 
       main="QQ-Plot de los residuos (Datos Originales)")
qqPlot(modelo_final$residuals, 
       main="QQ-Plot de los residuos (Datos Transformados)") 
par(mfrow=c(1,1))


# Verificar residuos

par(mfrow = c(2, 2))
plot(mod_base, main = "Diagnósticos del Modelo Base")
plot(modelo_final, main = "Diagnósticos del Modelo Final")
par(mfrow = c(1, 1))

# Homocedasticidad (Breusch-Pagan)
# Hipótesis Nula (H0): Varianza constante (Homocedasticidad)
# Si p-value < 0.05, RECHAZAMOS H0 -> Hay Heterocedasticidad

bp_test <- bptest(modelo_final)
print(bp_test) # es MUY pequeño -> hay heterocedasticidad
bp_test$p.value


###########################################
## Construcción y comparación de modelos ##
###########################################

# CONSTRUCCIÓN DE MODELOS: MÉTODO FORWARD (con backward da lo mismo)

modelo_nulo <- lm(Y_trans ~ 1, data = train_data)
modelo_completo <- lm(Y_trans ~ temp_pca + humidity_pca + visibility_pca + 
                        cloudcov_pca + lluvia + es_feriado +
                        factor(month) + factor(dia_semana), 
                      data = train_data)

#AIC forward -> Y_trans ~ factor(dia_semana) + factor(month) + temp_pca + cloudcov_pca
candidato_aic_f <- step(modelo_nulo, 
                      scope = list(lower = modelo_nulo, upper = modelo_completo),
                      direction = "forward",
                      trace = 0) # Pon trace=1 si quieres ver el "bucle" paso a paso

#BIC forward -> Y_trans ~ factor(dia_semana) + factor(month)
n <- nrow(train_data)
candidato_bic_f <- step(modelo_nulo, 
                      scope = list(lower = modelo_nulo, upper = modelo_completo),
                      direction = "forward",
                      k = log(n), # Esto activa la fórmula del BIC
                      trace = 0)

summary(candidato_aic_f) # Y_trans ~ factor(dia_semana) + factor(month) + es_feriado + temp_pca
summary(candidato_bic_f) # Y_trans ~ factor(dia_semana) + es_feriado + factor(month) + temp_pca

tabla_comparativa <- data.frame(
  Modelo = c("Forward AIC", "Forward BIC"),
  Variables_Num = c(length(coef(candidato_aic_f))-1, length(coef(candidato_bic_f))-1),
  R2_Ajustado = c(summary(candidato_aic_f)$adj.r.squared, summary(candidato_bic_f)$adj.r.squared),
  AIC = c(AIC(candidato_aic_f), AIC(candidato_bic_f)),
  BIC = c(BIC(candidato_aic_f), BIC(candidato_bic_f))
)
print(tabla_comparativa)

# Comparación test F 
# Comparación estadística
anova(candidato_aic_f, candidato_bic_f) # nos quedamos con el modelo AIC, el aporte es significativo con Pr(>F) = 0.02389 < 0.05

modelo_elegido <- candidato_bic_f

par(mfrow = c(2, 2))
plot(modelo_elegido, main = "Diagnósticos Modelo elegido")
par(mfrow = c(1, 1))

bp_test <- bptest(modelo_elegido)
print(bp_test)
bp_test$p.value 

cat("\n--- COEFICIENTES ROBUSTOS (HC3) ---\n")
# Esto ajusta los "Std. Error" y los "p-values"
modelo_robusto <- coeftest(modelo_elegido, vcov = vcovHC(modelo_elegido, type = "HC3"))
print(modelo_robusto)



##########
## TEST ##
##########

# Temperatura
temps_test <- test_data %>% dplyr::select(temp_avg, temp_max, temp_min)
test_data$temp_pca <- predict(pca_temp, newdata = temps_test)[, 1]

# Humedad
humi_test <- test_data %>% dplyr::select(humidity_avg, humidity_max, humidity_min)
test_data$humidity_pca <- predict(pca_humi, newdata = humi_test)[, 1]

# Nubosidad
cloud_test <- test_data %>% dplyr::select(cloudcov_avg, cloudcov_max, cloudcov_min)
test_data$cloudcov_pca <- predict(pca_cloud, newdata = cloud_test)[, 1]

# Visibilidad (usar MISMA estrategia de imputación)
visib_test <- test_data %>% dplyr::select(visibility_avg, visibility_max, visibility_min)
media_train <- mean(unlist(visib), na.rm = TRUE)
visib_test <- visib_test %>%
  mutate(across(everything(), ~ ifelse(is.na(.), media_train, .)))
test_data$visibility_pca <- predict(pca_visib, newdata = visib_test)[, 1]

test_data$dia_semana <- lubridate::wday(test_data$date, label = TRUE, abbr = FALSE)



pred_trans <- predict(modelo_elegido, newdata = test_data)
pred_final <- InvBoxCox(pred_trans, lambda = lambda_elegido) - 1
pred_final[pred_final < 0] <- 0

actual <- test_data$Y
pred_final <- InvBoxCox(pred_trans, lambda = lambda_elegido) - 1
pred_final[pred_final < 0] <- 0

indices_validos <- which(actual > 0) 
actual_limpio <- actual[indices_validos]
pred_clean <- pred_final[indices_validos]

mape <- mean(abs((actual_limpio - pred_clean) / actual_limpio)) * 100
rmse <- sqrt(mean((actual - pred_final)^2))
 
cat("\n--- Desempeño en Test ---\n")
cat("MAPE (Error Porcentual):", round(mape, 2), "%\n")
cat("RMSE (Error Cuadrático):", round(rmse, 2), "kg\n")
df_plot <- data.frame(
  Fecha = test_data$date,
  Real = actual,
  Predicho = pred_final
)

ggplot(df_plot, aes(x = Fecha)) +
  geom_line(aes(y = Real, color = "Real"), size = 0.8, alpha = 0.7) +
  geom_line(aes(y = Predicho, color = "Predicho"), size = 0.8, alpha = 0.7) +
  labs(title = "Predicción Demanda Comercial datos test",
       subtitle = paste("MAPE:", round(mape, 2), "% - RMSE:", round(rmse, 0)),
       y = "Demanda (kg)", x = "Fecha") +
  scale_color_manual(values = c("Real" = "black", "Predicho" = "red")) +
  theme_minimal() + 
  theme(
    legend.position = "bottom",      # Leyenda abajo para no tapar el gráfico
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40")
  )



