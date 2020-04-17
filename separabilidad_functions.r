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
    for(i in 1:nrows2){
      row2 <- muestra2[i,]
      
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