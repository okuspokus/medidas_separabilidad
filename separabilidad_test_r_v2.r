# Defino las funciones:
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

# 1. Levanto el csv que contiene las muestras:
muestras <- read.csv("~/Desktop/separabilidad_kandus/muestras_ba.csv", header=TRUE)


# 2. Selecciono las clases que quiero comparar
muestras_c1 <- muestras[which(muestras$clase=='c1'),]
muestras_c2 <- muestras[which(muestras$clase=='c2'),]
#muestras_c3 <- muestras[which(muestras$clase=='c3'),]
#muestras_c4 <- muestras[which(muestras$clase=='c4'),]
#muestras_c5 <- muestras[which(muestras$clase=='c5'),]



# 3. Selecciono las bandas con las que trabajare
selected_bands <- c('b1','b2','b3','b4','b5','b6')
muestras_c1_selected_bands <- muestras_c1[selected_bands] 
muestras_c2_selected_bands <- muestras_c2[selected_bands] 
#muestras_c3_selected_bands <- muestras_c3[selected_bands] 
#muestras_c4_selected_bands <- muestras_c4[selected_bands] 
#muestras_c5_selected_bands <- muestras_c5[selected_bands] 




# 3. Calculo:
bhattacharyya(muestras_c1_selected_bands, muestras_c2_selected_bands)
jeffries_matusita(muestras_c1_selected_bands, muestras_c2_selected_bands, sqrt_ = TRUE)
jeffries_matusita(muestras_c1_selected_bands, muestras_c2_selected_bands, sqrt_ = FALSE)
centroid_distance(muestras_c1_selected_bands, muestras_c2_selected_bands)
# La que sigue a continuacion tarda un poquito, por eso la comente, pero termina no te preocupes
# la comente solo para que no se quede pensando:
# average_distance(muestras_c1_selected_bands, muestras_c2_selected_bands)

