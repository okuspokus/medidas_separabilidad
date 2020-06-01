### Script para evaluar la separabilidad de muestras ####
### Autora: Mariela Rajngewerc ##########################
### Referencias: ver medidas_separabilidad.pdf ##########

# Cada muestra corresponde al valor espectral de un conjunto de píxeles para N bandas

##### Primero defino las funciones de las cuatro métricas ######
#### No es necesario corregir nada de este código. Correr las funciones para que se carguen en R, pero modificar sólo desde "Acá empieza el ejemplo" :) 
bhattacharyya <- function(muestra1, muestra2) {
  # Funcion para calcular la distancia de Bhattacharyya (Richards)
  #
  # Inputs
  # ------
  # muestra1 : dataframe donde las columnas son las bandas y las filas las observaciones.  
  # muestra2 : dataframe donde las columnas son las bandas y las filas las observaciones.
  #
  #
  # Output:
  # -------
  # bt : distancia de bhattacharyya entre muestra1 y muestra2. 
  
  mean1 <-  colMeans(muestra1)
  mean2 <- colMeans(muestra2)
  mean_diff <- mean1 - mean2
  
  cov1 <- cov(muestra1)
  cov2 <- cov(muestra2)
  
  cov_ <- (cov1 + cov2)/2
  
  
  bt <- mean_diff %*% solve(cov_, mean_diff)*(1/8) + (1/2) * log(det( cov_ ) / sqrt(det ( cov1 ) * det( cov2 )))
  
  return(bt[1,1])
}


jeffries_matusita <- function(muestra1, muestra2, sqrt_ = FALSE) {
  # Funcion para calcular la distancia de Jeffries Matusita (Richards)
  # 
  #
  # Inputs
  # ------
  # muestra1 : dataframe donde las columnas son las bandas y las filas las observaciones.  
  # muestra2 : dataframe donde las columnas son las bandas y las filas las observaciones.
  # sqrt_    : bool. Por default es FALSE. Si es FALSE no calcula la raiz y si es TRUE calcula la raiz.
  #
  #
  # Output:
  # -------
  # jm : distancia de jeffries.matusita entre muestra1 y muestra2. 
  
  # Richards no le calcula la raiz cuadrada:
  if (sqrt_ == TRUE) {
    jm <-  sqrt(2*(1-exp(-bhattacharyya(muestra1, muestra2))))
  }
  else {
    jm <- 2*(1-exp(-bhattacharyya(muestra1, muestra2)))   
      }
  return(jm)
}

average_distance <- function(muestra1, muestra2){
  # Esta funcion calcula el promedio de las distancias euclideas entre las filas de la muestra1 y las de la
  # muestra 2.  Si los dataframes tienen muchas filas puede tardar.
  #
  # Inputs
  # ------
  # muestra1 : dataframe donde las columnas son las bandas y las filas las observaciones.  
  # muestra2 : dataframe donde las columnas son las bandas y las filas las observaciones.
  #
  #
  # Output:
  # -------
  # d : calcula la distancia de cada fila de la muestra1 contra cada fila de la muestra2 y luego divide por la cantidad
  #     de filas (total).
  
  
  nrows1 <- nrow(muestra1)
  nrows2 <- nrow(muestra2)
  
  d <- 0
  
  for(i in 1:nrows1) {
    row1 <- muestra1[i,]
    for(j in 1:nrows2){
      row2 <- muestra2[j,]
      
      d <- d + sqrt(sum((row1 - row2) ^ 2))
    }
  }
  
  d <- d/(nrow1*nrow2)
  
  return(d)
}

centroid_distance <-function(muestra1, muestra2){
  # Esta funcion calcula el promedio de las distancias euclideas entre el vector de medias de muestra1 y 
  # el vector de medias de la muestra 2.
  #
  # Inputs
  # ------
  # muestra1 : dataframe donde las columnas son las bandas y las filas las observaciones.  
  # muestra2 : dataframe donde las columnas son las bandas y las filas las observaciones.
  #
  #
  # Output:
  # -------
  # d : calcula la distancia de cada fila de la muestra1 contra cada fila de la muestra2 y luego divide por la cantidad
  #     de filas (total).
  
  mean1 <- colMeans(muestra1)
  mean2 <- colMeans(muestra2)
  
  d <- sqrt(sum((mean1 - mean2) ^ 2))
  
  return(d)
}

##################################################################################################
#################### ACA EMPIEZA EL EJEMPLO ######################################################
##################################################################################################

## 1. Levanto el csv que contiene las muestras: #####

## ejemplo 1: tengo un único CSV en el que están cargadas todas las muestras: 
muestras <- read.csv("data/muestras_ba.csv", header=TRUE)
# fin ejemplo 1

## ejemplo 2: tengo múltiples planillas, una para cada muestra. Cargo todas mis planillas, una a continuación de otra: 
muestra1 <- read.csv("data/agua natural.csv", header=TRUE)
muestra2 <- read.csv("data/Humedal.csv", header=TRUE)
muestra3 <- read.csv("data/Plantaciones Forestales.csv", header=TRUE)
# replicar el código para todas las muestras

# asigno una columna con nombres de las muestras (nombres a elección)
#lo importante es que la columna se llame igual "clase", pero el nombre tiene que ser distinto y puede ser el que una quiera
muestra1$clase <- "Muestra1"
muestra2$clase <- "Muestra2"
muestra3$clase <- "Muestra3"

# uno las planillas de todas mis muestras, una debajo de la otra. 
# es importante que coincidan los nombres de columnas
muestras <- rbind(muestra1, muestra2, muestra3)

head(muestras)  #ver qué tal se ve (muestra primeras filas). elimino filas que no sirvan:
muestras$ROI_Name <- NULL
head(muestras) 

str(muestras) #esto es para ver la estructura del objeto "muestra"
#es deseable que el objeto sea un data.frame
#que las columnas de cada banda sean numéricas
#que la columna de Clase o nombre de muestra sea un factor
#en caso de que alguna de estas condiciones no se cumpla, lo puedo corregir:
muestras$clase <- as.factor(muestras$clase) #pasar una columna a tipo factor

# fin ejemplo 2

# en ambos ejemplos, finalizo con una tabla llamada "muestras" 

# 2. Selecciono las bandas con las que trabajare ####
# si voy a usar todas, puedo simplemente hacer:
muestras_selected_bands <- muestras
muestras_selected_bands$clase = NULL #y luego sacarle la columna de clases

# si esto sirve, seguir con paso tres! >>>>

# si en cambio quiero usar un subconjunto de bandas...
# tengo que construir un vector, por ejemplo:
selected_bands <- c('b1','b2','b3','b4','b5','b6') 

#para construir ese vector puedo usar:
colnames(muestras) #observar y registrar qué valores sirven (en principio, todos menos "clase")
selected_bands <- colnames(muestras)[2:7]  # que use todos los valores, desde el 2do hasta el 7mo - es un ejemplo, supongamos que no me interesa la Banda 1

muestras_selected_bands <- muestras[selected_bands] #lo filtro según el vector anterior

## 3. Calculo los índices: ######

## 3.1. Métrica de Bhattacharyya

# ejemplo para dos clases

bhattacharyya(subset(muestras_selected_bands, muestras$clase =="Muestra1"), subset(muestras_selected_bands, muestras$clase =="Muestra2"))

# se debería analizar de este modo cada par de muestras...
# pero mejor: hacerlo para todas las clases en un único loop

firmas <- levels(muestras$clase)


for (i in firmas) {
  for (j in firmas) {
    firma1=subset(muestras_selected_bands, muestras$clase==i)
    firma2=subset(muestras_selected_bands, muestras$clase==j)
    sep_bhatta <- bhattacharyya(firma1, firma2)
    print(c(i, j, sep_bhatta))
    }}

# el print devuelve la combinación de todas las muestras a la consola
# se puede copiar y pegar a donde quieras
# me faltaría agregarle nombres de columnas y exportarlo a un csv, me trabé en esto

## 3.2. Métrica de Jeffreis-Matusita

firmas <- levels(muestras$clase)

condicion = FALSE # configurar si dentro del índice quiero que calcule la raiz cuadrado o no (son dos definiciones distintas del índice). 

for (i in firmas) {
  for (j in firmas) {
    firma1=subset(muestras_selected_bands, muestras$clase==i)
    firma2=subset(muestras_selected_bands, muestras$clase==j)
    sep_JM <- jeffries_matusita(firma1, firma2, sqrt_ = condicion)
    print(c(i, j, sep_JM))
  }}



## 3.3. Métrica centroid_distance
centroid_distance(muestras_c1_selected_bands, muestras_c2_selected_bands)

firmas <- levels(muestras$clase)

for (i in firmas) {
  for (j in firmas) {
    firma1=subset(muestras_selected_bands, muestras$clase==i)
    firma2=subset(muestras_selected_bands, muestras$clase==j)
    sep_CD <- centroid_distance(firma1, firma2)
    print(c(i, j, sep_CD))
  }}



## 3.4. Métrica average_distance

# La que sigue a continuacion tarda un poquito, por eso la comente, pero termina no te preocupes
# la comente solo para que no se quede pensando:


firmas <- levels(muestras$clase)

# for (i in firmas) {
#   for (j in firmas) {
#     firma1=subset(muestras_selected_bands, muestras$clase==i)
#     firma2=subset(muestras_selected_bands, muestras$clase==j)
#     sep_CD <- average_distance(firma1, firma2)
#     print(c(i, j, sep_CD))
#   }}

