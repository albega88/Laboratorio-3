
#Marlon Tellez Ortiz
#Alberth Garcia Rosales


## Con esta instalamos las librerías necesarias para la ejecución
install.packages("dplyr")
install.packages("ggplot2")
install.packages("visdat") 
install.packages("naniar")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("visdat")


## Con estos comando cargamos las librerias al script - O
library(dplyr)
library(ggplot2)
library(visdat) 
library(naniar)
library(dplyr)
library(ggplot2)
library(stringr)
library(visdat).



## Leemos el csv

datos <- read.csv("C:/Users/Marlon/Documents/Proyectos Github/Laboratorio-3/estadisticas_vinos.csv")

#----------------------------------------------------------------------------
#          Exploración Inicial de los Datos                                --
#----------------------------------------------------------------------------


#Obtener información sobre las columnas y tipos de datos
## Con este obtenemos el nombre de la columnas, su tipo de dato respectivos y un preview se la data.
str(datos)

#Resumen estadístico de las variables numéricas
## Aqui realizaremos el analisis descriptivo para variables numéricas 'points' y 'price', el resumen total
summary_points <- summary(datos$points)
print(summary_points)

summary_price <- summary(datos$price)
print(summary_price)


## Con esta vemos toda la informacion del dataframe
print(summary(datos))


#----------------------------------------------------------------------------
#          Limpieza y Tratamiento de Datos                               --
#----------------------------------------------------------------------------


#Determine valores nulos
#obtenemos valores nulos y en blanco por columna
sapply(datos, function(x) sum(is.na(x) | x == ""))

#Determine valores atípicos

#valores atipicos de precio
boxplot(datos$price)

#valores atipicos por puntuacion
boxplot(datos$points)


#valores atipicos de precio por pais
boxplot(price ~ country, 
        data = datos,
        col = "blue",
        xlab = "Precio",
        ylab = "País",
        frame = FALSE,
        horizontal = TRUE)


#valores atipicos de precio por provincia
boxplot(price ~ province, 
        data = datos,
        col = "blue",
        xlab = "Precio",
        ylab = "Provincia",
        frame = FALSE,
        horizontal = TRUE)

#valores atipicos de precio vinera
boxplot(price ~ winery, 
        data = datos,
        col = "blue",
        xlab = "Precio",
        ylab = "Vinera",
        frame = FALSE,
        horizontal = TRUE)


#Determine concentraciones y tendencia de datos

#PRECIO
hist(datos$price,
     col = "blue",
     xlab = "Precio",
     main = "Histograma Estadisticas de vinos")

#POINTS
hist(datos$points,
     col = "blue",
     xlab = "CALIFICACION",
     main = "Histograma Estadisticas de vinos")


#Determine acciones correctivas imputación

#eliminar la columna fila que no aporta nada de informacion
datos <- datos[, -which(colnames(datos) == "Fila")]

# Calcular la media del precio
mean_price <- round(mean(datos$price, na.rm = TRUE), digits = 0) 

mean_price


# Rellenar los NA con la media
datos$price[is.na(datos$price)] <- mean_price

sum(is.na(datos$price))

View (datos)

#----------------------------------------------------------------------------
#            Visualización de datos                                       ---
#----------------------------------------------------------------------------


#Realice las gráficas de datos y agrupaciones para las variables categóricas en el dataset

# Crear el gráfico de barras de sumatoria de precios por pais
ggplot(suma_precios_por_pais, aes(x = country, y = suma_precio)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Suma de Precios por País", x = "País", y = "Suma de Precios") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Crear el gráfico de dispersión (puntos por país) osea la puntuacion de los vinos por pais
ggplot(datos, aes(x = country, y = points)) +
  geom_point() +
  labs(title = "Puntuación de Vinos por País", x = "País", y = "Puntuación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#----------------------------------------------------------------------------
#            Análisis de Relaciones de Datos:                                      ---
#----------------------------------------------------------------------------


#Análisis Univariable y Bivariable
# Gráfico de Dispersión (Puntos vs Precio)
# Crear el gráfico de dispersión (manejando los valores faltantes)
ggplot(na.omit(datos), aes(x = price, y = points)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Gráfico de Dispersión: Precio vs Puntaje", x = "Precio", y = "Puntaje") +
  theme_minimal()

#agrupacion de los puntuacion. Univariable
ggplot(datos, aes(x = points)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Puntajes", x = "Puntaje", y = "Frecuencia") +
  theme_minimal()

#univariable de precio en el cual se utiliza binds para mostrar mas variables de precio
# Definir el número de intervalos (bins)
num_bins <- 30

# Crear el histograma
hist(datos$price, 
     breaks = num_bins, 
     main = "Histograma de Precios", 
     xlab = "Precio", 
     ylab = "Frecuencia",
     col = "#0072B2",       # Cambiar color de las barras
     border = "#D55E00",    # Cambiar color del borde de las barras
     xlim = c(0, 500),       # Ajustar el rango del eje x
     ylim = c(0, 3000),      # Ajustar el rango del eje y
     freq = TRUE)           # Mostrar

