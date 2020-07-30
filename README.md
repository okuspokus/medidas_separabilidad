# Script para evaluar la separabilidad de muestras 
## Autora: Mariela Rajngewerc 
### Colaboradora: Natalia Morandeira

Cada muestra corresponde al valor espectral de un conjunto de píxeles para N bandas. Las muestras son extraidas en regiones homogéneas de una imagen satelital (por ejemplo, cada muestra corresponde a un polígono de una dada clase de información) y el objetivo es cuantificar cuán separables o disímiles son esas muestras.
En la jerga de la teledetección satelital, a cada una de esas muestras se la conoce como ROI (del inglés, "Region of interest") o AOI (del inglés, "Area of interest"). Evaluar la separabilidad de las muestras es relevante en el marco de las clasificaciones digitales de datos satelitales. 


## Medidas de separabilidad
Este repositorio incluye cuatro funciones para cuantificar la separabilidad entre pares de muestras y dos ejemplos.

+ Bhattacharyya
+ Jeffries-Matusita (JM)
+ Average Distance
+ Distancia entre centroides

Ver más detalle de las funciones en: medidas_separabilidad.pdf

