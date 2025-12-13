# Analisis Exporatorio
library(readr)
library(rio)
library(dplyr)
library(lubridate)
library(corrplot)
data_met <- read_csv("datos_completos_estacion_punteras.csv")  
names(data_met)
colores_azul_formal <- colorRampPalette(c("#FF0000", "white", "#0000FF"))(200)

############################################################
## Análisis exploratorio, para eleccion de var a predecir ##
############################################################
data_met$fecha <- as.Date(data_met$fecha)

M <- cor(data_met[, -1], use = "complete.obs")
contaminantes <- c("N02", "MP10", "MP2_5", "NO")
predictoras <- setdiff(colnames(M), contaminantes)
cor_subset <- M[predictoras, contaminantes]

# Análisis de Correlación
corrplot(cor_matrix, 
         method = "ellipse", 
         col = colores_azul_formal, 
         tl.col = "black",          
         tl.cex = 0.8,             
         tl.srt = 45,               
         addCoef.col = NULL,    
         number.cex = 0.6,          
         diag = FALSE,              
         mar = c(0, 0, 1, 0))       


#Correlación Específica (Zoom a los contaminantes)
contaminantes <- c("N02", "MP10", "MP2_5", "NO")
cor_target <- cor_matrix[, contaminantes]
corrplot(cor_subset,
         method = "ellipse",
         col = colores_azul_formal,  # Aplicar gama azul
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         addCoef.col = NULL,      # Coeficientes visibles
         number.cex = 0.7,
         cl.pos = "r",               # Barra de color a la derecha
         mar = c(0, 0, 2, 0))        # Margen interno pequeño


# Histogramas de los contaminantes (Para ver normalidad)

# 1. Ajustar márgenes
par(mar = c(5, 5, 4, 2))

# 2. Generar Histograma (freq = FALSE es clave para la línea)
hist(data_met$MP2_5, 
     freq = FALSE,          # <--- IMPORTANTE: Cambia de conteo a densidad
     main = "Distribución de MP2.5", 
     col = "#1F4E79",       # Tu azul corporativo
     border = "white",
     xlab = "Concentración (µg/m³)", 
     ylab = "Densidad",     # La etiqueta cambia de 'Frecuencia' a 'Densidad'
     las = 1,
     cex.main = 1.2,
     font.main = 2,
     # Truco: Aseguramos que el eje Y sea lo bastante alto para la curva
     ylim = c(0, max(density(data_met$MP2_5, na.rm = TRUE)$y) * 1.1))

# 3. Agregar la línea de distribución (suavizada)
lines(density(data_met$MP2_5, na.rm = TRUE), 
      col = "#FF0000",      # Azul muy oscuro para contraste elegante
      lwd = 3)              # Grosor de línea 3

# 4. Borde limpio
box(bty = "l")

# 7. Series de tiempo (Para ver estacionalidad o cortes raros)

par(mfrow = c(4, 1), mar = c(2, 4, 2, 1))

plot(data_met$fecha, data_met$N02, type = "l", main = "Serie de Tiempo NO2", col = "blue")
plot(data_met$fecha, data_met$MP10, type = "l", main = "Serie de Tiempo MP10", col = "darkgreen")
plot(data_met$fecha, data_met$MP2_5, type = "l", main = "Serie de Tiempo MP2_5", col = "#1F4E79")
plot(data_met$fecha, data_met$NO, type = "l", main = "Serie de Tiempo NO", col = "orange")

par(mfrow = c(1, 1))



## En base al analisis exporatorio se eligio MP2_5 como variable a predecir ##
## No elegi MP10 pq puede tener mas "ruido" al ser zona costera puede haber particulas de 
## sal, arena, polvo grueso, etc.
# en el paper/presentación explicar por que!!



