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

modelo_inicial <- lm(Y ~ temp_avg + lluvia + humidity_avg + cloudcov_avg + visibility_avg + es_feriado + factor(month) + factor(dia_semana), 
                     data = train_data)
############################################
### Transformación Box–Cox de regresores ###
############################################

### Temperaturas ###
# Calculamos el lambda óptimo
bc_temp_avg <- BoxCox.lambda(train_data$temp_avg)
bc_temp_min <- BoxCox.lambda(train_data$temp_min)
bc_temp_max <- BoxCox.lambda(train_data$temp_max)

# Aplicamos la transformación usando ese lambda
train_data$temp_avg_bc <- BoxCox(train_data$temp_avg, bc_temp_avg)
train_data$temp_min_bc <- BoxCox(train_data$temp_min, bc_temp_min)
train_data$temp_max_bc <- BoxCox(train_data$temp_max, bc_temp_max)

### Humedad ###
bc_humi_avg <- BoxCox.lambda(train_data$humidity_avg)
bc_humi_min <- BoxCox.lambda(train_data$humidity_min)
bc_humi_max <- BoxCox.lambda(train_data$humidity_max)

train_data$humidity_avg_bc <- BoxCox(train_data$humidity_avg, bc_humi_avg)
train_data$humidity_min_bc <- BoxCox(train_data$humidity_min, bc_humi_min)
train_data$humidity_max_bc <- BoxCox(train_data$humidity_max, bc_humi_max)

### Nubosidad ###
bc_cloud_avg <- BoxCox.lambda(train_data$cloudcov_avg + 0.01)
bc_cloud_min <- BoxCox.lambda(train_data$cloudcov_min + 0.01)
bc_cloud_max <- BoxCox.lambda(train_data$cloudcov_max + 0.01)

train_data$cloudcov_avg_bc <- BoxCox(train_data$cloudcov_avg + 0.01, bc_cloud_avg)
train_data$cloudcov_min_bc <- BoxCox(train_data$cloudcov_min + 0.01, bc_cloud_min)
train_data$cloudcov_max_bc <- BoxCox(train_data$cloudcov_max + 0.01, bc_cloud_max)

### Visibilidad ###
visib_fix <- train_data %>% 
  dplyr::select(visibility_avg, visibility_min, visibility_max) %>%
  dplyr::mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Usamos visib_fix porque train_data aún tiene NAs y BoxCox fallaría
bc_visib_avg <- BoxCox.lambda(visib_fix$visibility_avg)
bc_visib_min <- BoxCox.lambda(visib_fix$visibility_min)
bc_visib_max <- BoxCox.lambda(visib_fix$visibility_max)

train_data$visibility_avg_bc <- BoxCox(visib_fix$visibility_avg, bc_visib_avg)
train_data$visibility_min_bc <- BoxCox(visib_fix$visibility_min, bc_visib_min)
train_data$visibility_max_bc <- BoxCox(visib_fix$visibility_max, bc_visib_max)

### Lluvia ###
bc_lluvia <- BoxCox.lambda(train_data$lluvia + 1)
train_data$lluvia_bc <- BoxCox(train_data$lluvia + 1, bc_lluvia)

###########
### PCA ###
###########

### PCA temperatura
temps_bc <- train_data %>% 
  dplyr::select(temp_avg_bc, temp_min_bc, temp_max_bc)
pca_temp <- prcomp(temps_bc, scale. = TRUE)
train_data$temp_pca <- as.numeric(pca_temp$x[, 1])

### PCA humedad
humi_bc <- train_data %>% 
  dplyr::select(humidity_avg_bc, humidity_min_bc, humidity_max_bc)
pca_humi <- prcomp(humi_bc, scale. = TRUE)
train_data$humidity_pca <- as.numeric(pca_humi$x[, 1])

### PCA nubosidad
cloud_bc <- train_data %>% 
  dplyr::select(cloudcov_avg_bc, cloudcov_min_bc, cloudcov_max_bc)
pca_cloud <- prcomp(cloud_bc, scale. = TRUE)
train_data$cloudcov_pca <- as.numeric(pca_cloud$x[, 1])

### PCA visibilidad
visib_bc <- train_data %>%
  dplyr::select(visibility_avg_bc, visibility_min_bc, visibility_max_bc)
pca_visib <- prcomp(visib_bc, scale. = TRUE)
train_data$visibility_pca <- as.numeric(pca_visib$x[, 1])

### Eliminamos únicamente las variables originales redundantes ###
train_data <- train_data %>% 
  dplyr::select(-temp_avg, -temp_min, -temp_max,
         -humidity_avg, -humidity_min, -humidity_max,
         -cloudcov_avg, -cloudcov_min, -cloudcov_max,
         -visibility_avg, -visibility_min, -visibility_max,
         -temp_avg_bc, -temp_min_bc, -temp_max_bc,
         -humidity_avg_bc, -humidity_min_bc, -humidity_max_bc,
         -cloudcov_avg_bc, -cloudcov_min_bc, -cloudcov_max_bc,
         -visibility_avg_bc, -visibility_min_bc, -visibility_max_bc)

###################################
### Transformación Box–Cox de Y ###
###################################
names(train_data)

train_data$Y_adj <- train_data$Y + 1
mod_base <- lm(Y_adj ~ temp_pca + lluvia_bc + humidity_pca  + visibility_pca + cloudcov_pca + es_feriado +factor(month) + factor(dia_semana), 
               data = train_data)

# Encontrar lambda óptimo
lambdas <- MASS::boxcox(mod_base, lambda = seq(0.3, 0.5, 0.001), plotit = TRUE)
lambda_optimo <- lambdas$x[lambdas$y == max(lambdas$y)]
lambda_elegido <- lambda_optimo
lambda_optimo
train_data$Y_trans <- forecast::BoxCox(train_data$Y_adj, lambda = lambda_elegido)

# Ajustamos el modelo final con la Y transformada
modelo_Ytrans <- lm(Y_trans ~ temp_pca + lluvia_bc + humidity_pca + visibility_pca+ cloudcov_pca + es_feriado + factor(month)+ factor(dia_semana), 
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

bp_test_base <- bptest(mod_base)
print(bp_test_base) 
bp_test_base$p.value

bp_test_Ytrans <- bptest(modelo_Ytrans)
print(bp_test_Ytrans) 
bp_test_Ytrans$p.value

############################
### comparacion de modelos #
############################
par(mfrow = c(2, 2))
summary(modelo_inicial)
plot(modelo_inicial, main = "Diagnósticos Modelo Inicial")
summary(mod_base)
plot(mod_base, main = "Diagnósticos Modelo Base")
summary(modelo_Ytrans)
plot(modelo_Ytrans, main = "Diagnósticos Modelo Y Transformada")
par(mfrow = c(1, 1))


fitted_transformados <- modelo_Ytrans$fitted.values
train_data$fitted_modelo_Ytrans <- forecast::InvBoxCox(fitted_transformados, lambda = lambda_elegido)
RSS_Ytrans <- sum((train_data$Y - train_data$fitted_modelo_Ytrans)^2)

RSS_mod_inicial <- summary(modelo_inicial)$sigma^2 * modelo_inicial$df.residual
RSS_mod_base <- summary(mod_base)$sigma^2 * mod_base$df.residual

# Comparación numérica
print(paste("RSS Inicial", round(RSS_mod_inicial, 2)))
print(paste("RSS Base:", round(RSS_mod_base, 2)))
print(paste("RSS Ytrans:", round(RSS_Ytrans, 2)))

#shapiro 
  #Hipótesis nula (H₀): Los residuos siguen una distribución normal
  #Hipótesis alternativa (H₁): Los residuos NO siguen una distribución normal
  #Criterio de decisión: Si p-value < 0.05, se rechaza H₀ (no hay normalidad)

shapiro_resid_inicial <- shapiro.test(modelo_inicial$residuals)
shapiro_resid_inicial
shapiro_resid_base <- shapiro.test(mod_base$residuals)
shapiro_resid_base
shapiro_resid_Ytrans <- shapiro.test(modelo_Ytrans$residuals)
shapiro_resid_Ytrans
############################
### Selección de modelos ###
############################

# Modelo completo con todas las variables relevantes
modelo_completo <- modelo_Ytrans #elijo el mejor de los anteriores
modelo_nulo <- lm(Y_trans ~ 1, data = train_data)


#AIC forward -> Y_trans ~ factor(dia_semana) + factor(month) + temp_pca + cloudcov_pca
candidato_aic_f <- step(modelo_nulo, 
                        scope = list(lower = modelo_nulo, upper = modelo_completo),
                        direction = "forward",
                        trace = 1) # Pon trace=1 si quieres ver el "bucle" paso a paso

#BIC forward -> Y_trans ~ factor(dia_semana) + factor(month)
n <- nrow(train_data)
candidato_bic_f <- step(modelo_nulo, 
                        scope = list(lower = modelo_nulo, upper = modelo_completo),
                        direction = "forward",
                        k = log(n), # Esto activa la fórmula del BIC
                        trace = 0)

summary(candidato_aic_f) # Y_trans ~ factor(dia_semana) + factor(month) + es_feriado + temp_pca
summary(candidato_bic_f) #  Y_trans ~ factor(dia_semana) + es_feriado + factor(month) + temp_pca

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

# HC3 errores robustos

modelo_robusto <- coeftest(modelo_elegido, vcov = vcovHC(modelo_elegido, type = "HC3"))
print(modelo_robusto)
library(ggplot2)

coef_data <- data.frame(
  variable = rownames(modelo_robusto),
  estimate = modelo_robusto[,1],
  std_error = modelo_robusto[,2],
  p_value = modelo_robusto[,4]
)
# IC 
coef_data$conf_low <- coef_data$estimate - 1.96 * coef_data$std_error
coef_data$conf_high <- coef_data$estimate + 1.96 * coef_data$std_error
coef_plot <- coef_data[coef_data$variable != "(Intercept)", ]

ggplot(coef_plot, aes(x = estimate, y = reorder(variable, estimate))) +
  geom_point(size = 2, color = "blue") +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), 
                 height = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Coeficientes robustos (HC3) con intervalos de confianza 95%",
       subtitle = "Variables ordenadas por magnitud del efecto",
       x = "Estimación del coeficiente", 
       y = "Variables",
       caption = "Línea roja: efecto nulo. Intervalos que no cruzan cero son significativos") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggplot(coef_plot, aes(x = estimate, y = -log10(p_value), 
                      color = p_value < 0.05, label = variable)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(check_overlap = TRUE, nudge_y = 0.1, size = 3) +
  scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "blue"),
                     name = "Significativo (p < 0.05)") +
  labs(title = "Volcán de significancia - Coeficientes robustos",
       x = "Magnitud del efecto", 
       y = "Significancia (-log10 p-value)") +
  theme_minimal()


meses_data <- coef_plot[grep("month", coef_plot$variable), ]
  
ggplot(meses_data, aes(x = reorder(variable, estimate), y = estimate)) +
  geom_col(aes(fill = p_value < 0.05), alpha = 0.8) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray"),
                      name = "Significativo") +
  labs(title = "Efecto de meses vs mes de referencia",
        x = "Mes", y = "Diferencia vs referencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################
### TEST DATA ##
################

### Aplicar las mismas transformaciones Box-Cox a test_data ###

### Temperaturas
test_data$temp_avg_bc <- BoxCox(test_data$temp_avg, bc_temp_avg)
test_data$temp_min_bc <- BoxCox(test_data$temp_min, bc_temp_min)
test_data$temp_max_bc <- BoxCox(test_data$temp_max, bc_temp_max)

### Humedad 
test_data$humidity_avg_bc <- BoxCox(test_data$humidity_avg, bc_humi_avg)
test_data$humidity_min_bc <- BoxCox(test_data$humidity_min, bc_humi_min)
test_data$humidity_max_bc <- BoxCox(test_data$humidity_max, bc_humi_max)

### Nubosidad 
test_data$cloudcov_avg_bc <- BoxCox(test_data$cloudcov_avg + 0.01, bc_cloud_avg)
test_data$cloudcov_min_bc <- BoxCox(test_data$cloudcov_min + 0.01, bc_cloud_min)
test_data$cloudcov_max_bc <- BoxCox(test_data$cloudcov_max + 0.01, bc_cloud_max)

### Visibilidad 
visib_fix_test <- test_data %>% 
  dplyr::select(visibility_avg, visibility_min, visibility_max) %>%
  dplyr::mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

test_data$visibility_avg_bc <- BoxCox(visib_fix_test$visibility_avg, bc_visib_avg)
test_data$visibility_min_bc <- BoxCox(visib_fix_test$visibility_min, bc_visib_min)
test_data$visibility_max_bc <- BoxCox(visib_fix_test$visibility_max, bc_visib_max)

### Lluvia 
test_data$lluvia_bc <- BoxCox(test_data$lluvia + 1, bc_lluvia)

# PCA temperatura
temps_bc_test <- test_data %>% 
  dplyr::select(temp_avg_bc, temp_min_bc, temp_max_bc)
test_data$temp_pca <- predict(pca_temp, newdata = temps_bc_test)[, 1]

# PCA humedad
humi_bc_test <- test_data %>% 
  dplyr::select(humidity_avg_bc, humidity_min_bc, humidity_max_bc)
test_data$humidity_pca <- predict(pca_humi, newdata = humi_bc_test)[, 1]

# PCA nubosidad
cloud_bc_test <- test_data %>% 
  dplyr::select(cloudcov_avg_bc, cloudcov_min_bc, cloudcov_max_bc)
test_data$cloudcov_pca <- predict(pca_cloud, newdata = cloud_bc_test)[, 1]

# PCA visibilidad
visib_bc_test <- test_data %>%
  dplyr::select(visibility_avg_bc, visibility_min_bc, visibility_max_bc)
test_data$visibility_pca <- predict(pca_visib, newdata = visib_bc_test)[, 1]

### Eliminar variables redundantes (igual que en train) ###
test_data <- test_data %>% 
  dplyr::select(-temp_avg, -temp_min, -temp_max,
                -humidity_avg, -humidity_min, -humidity_max,
                -cloudcov_avg, -cloudcov_min, -cloudcov_max,
                -visibility_avg, -visibility_min, -visibility_max,
                -temp_avg_bc, -temp_min_bc, -temp_max_bc,
                -humidity_avg_bc, -humidity_min_bc, -humidity_max_bc,
                -cloudcov_avg_bc, -cloudcov_min_bc, -cloudcov_max_bc,
                -visibility_avg_bc, -visibility_min_bc, -visibility_max_bc)

### Aplicar transformación Box-Cox a Y en test_data ###
test_data$Y_adj <- test_data$Y + 1
test_data$Y_trans <- forecast::BoxCox(test_data$Y_adj, lambda = lambda_elegido)

test_data$dia_semana <- factor(test_data$dia_semana, levels = levels(train_data$dia_semana))
test_data$month <- factor(test_data$month, levels = levels(factor(train_data$month)))

### Predicciones y evaluación del modelo en test_data ###
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
    legend.position = "bottom",     
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40")
  )




