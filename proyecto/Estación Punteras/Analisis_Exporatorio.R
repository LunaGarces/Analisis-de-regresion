# Analisis Exporatorio
library(readr)
library(rio)
library(dplyr)
library(lubridate)
library(corrplot)
data_met <- read_csv("datos_completos_estacion_punteras.csv")  # Más rápida que read.csv
names(data_met)

############################################################
## Análisis exploratorio, para eleccion de var a predecir ##
############################################################
data_met$fecha <- as.Date(data_met$fecha)

M <- cor(data_met[, -1], use = "complete.obs")
contaminantes <- c("N02", "MP10", "MP2_5", "NO")
predictoras <- setdiff(colnames(M), contaminantes)
cor_subset <- M[predictoras, contaminantes]

# 4. Análisis de Correlación
# Meteorología: columnas 2 a 16
# Contaminantes: columnas 17 a 20 (NO2, MP10, MP2_5, NO)

# Matriz de correlación general
cor_matrix <- cor(data_met[, -1], use = "complete.obs")
corrplot(cor_matrix, 
         method = "ellipse", 
         tl.col = "black", 
         tl.cex = 0.6,    
         addCoef.col = "black",   ## CAMBIAR POR NULL PARA EL PROYECTO
         number.cex = 0.4,        ## BORRA 
         diag = FALSE)

# 5. Correlación Específica (Zoom a los contaminantes)
contaminantes <- c("N02", "MP10", "MP2_5", "NO")
cor_target <- cor_matrix[, contaminantes]


# 5. Graficar solo ese cruce
corrplot(cor_subset,
         method = "ellipse",       
         tl.col = "black",         
         tl.srt = 45,              
         tl.cex = 0.8,             
         addCoef.col = "black",   
         number.cex = 0.7,         
         cl.pos = "r",            
         main = "Correlación: Clima vs Contaminantes",
         mar = c(0,0,2,0))        

# 6. Histogramas de los contaminantes (Para ver normalidad)

par(mfrow = c(2, 2)) # Grilla de 2x2

hist(data_met$N02, main = "Histograma NO2", col = "lightblue", border = "white", xlab = "Concentración")
hist(data_met$MP10, main = "Histograma MP10", col = "lightgreen", border = "white", xlab = "Concentración")
hist(data_met$MP2_5, main = "Histograma MP2_5", col = "lightcoral", border = "white", xlab = "Concentración")
hist(data_met$NO, main = "Histograma NO", col = "orange", border = "white", xlab = "Concentración")

par(mfrow = c(1, 1)) # Volver a 1 gráfico por ventana

# 7. Series de tiempo (Para ver estacionalidad o cortes raros)

par(mfrow = c(4, 1), mar = c(2, 4, 2, 1))

plot(data_met$fecha, data_met$N02, type = "l", main = "Serie de Tiempo NO2", col = "blue")
plot(data_met$fecha, data_met$MP10, type = "l", main = "Serie de Tiempo MP10", col = "darkgreen")
plot(data_met$fecha, data_met$MP2_5, type = "l", main = "Serie de Tiempo MP2_5", col = "red")
plot(data_met$fecha, data_met$NO, type = "l", main = "Serie de Tiempo NO", col = "orange")

par(mfrow = c(1, 1))



## En base al analisis exporatorio se eligio MP2_5 como variable a predecir ##
## No elegi MP10 pq puede tener mas "ruido" al ser zona costera puede haber particulas de 
## sal, arena, polvo grueso, etc.
# en el paper/presentación explicar por que!!



