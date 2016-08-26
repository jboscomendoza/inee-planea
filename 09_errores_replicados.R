library(dplyr)
library(haven)

# Dame datos ----

original <- #Aquí debe ir haven::read_sav("labasededatos.sav"), o foreign::read.spss("base.sav")

# Mi borrador: Operaciones en bruto ----

prefix <- "W_FSTR"

columnas_pesos <- #nombre de las columnas con pesos
  grep(
    x = names(original), #nombres de las columnas
    paste(prefix, "[[:digit:]]", sep = ""), #Pegamos el prefijo con todos los valores
    value = T #Nombres, no columnas
  )

# Calcular 100 medias con pesos muestrales ----

medias_mil <- vector() #Vector para iniciar media ponderadas
contador = 1 #Contador para asignar a vector medias mil
for(i in columnas_pesos) {
  peso <- original[[i]] #Peso muestral
  peso[is.na(peso)] <- 0 #Adios a NA

  RFAB <- na.omit(original[["RFAB"]]) #Variable a estimar

  medias_mil[contador] <-
    weighted.mean(RFAB, peso, na.rm = T) #Media ponderada
  contador <- contador + 1 #Contador hasta llegar al top top top
}

media_uno <- #Media ponderada con resumen
  weighted.mean(original[["RFAB"]], original[["W_FSTUWT"]])

diferencias <- #Todo lo siguiente, calcula el error estándar
  sqrt(
    sum(
      (medias_mil - media_uno)^2
    )/20 # Porqué 20, es un misterio
  )

media_uno + (1.96 * diferencias) #Intervalo superior
media_uno - (1.96 * diferencias) #Intervalo inferior

# ----

#como funcion, morro ----

ee_replicado <-
  function(variable, prefix = "W_FSTR"){

    #nombre de las columnas con pesos
    nombres_columnas <-
      grep(x = names(original), pattern = prefix, value = T) #Por implementar, en lugar de hacer referencia a "original", pedir como argumento el nombre de la tabla que usaremos

    medias_mil <- vector() #Vector para iniciar media ponderadas
    contador = 1 #Contador para asignar a vector medias mil
      vector_variable <- original[[variable]] #Variable a estimar
    vector_variable[is.na(vector_variable)] <- mean(vector_variable) #Imputamos media por NA. Sí, ya sé que no está bien

    for(i in columnas_pesos) {
      peso <- original[[i]] #Peso muestral
      peso[is.na(peso)] <- 0 #Adios a NA


      medias_mil[contador] <-
        weighted.mean(vector_variable, peso, na.rm = T) #Media ponderada
      contador <- contador + 1 #Contador hasta llegar al top top top
    }

    media_uno <- #Media ponderada con resumen
      weighted.mean(original[[variable]], original[["W_FSTUWT"]], na.rm = T)

    diferencias <- #Todo lo siguiente, calcula el error estándar
      sqrt(
        sum(
          (medias_mil - media_uno)^2
        )/20 # Porqué 20, es un misterio
      )
    print(
      unlist(list(
        Media = media_uno,
        Error_estandar = diferencias,
        Intervalo_de_confianza_superior = media_uno + (1.96 * diferencias),
        Intervalo_de_confianza_inferior = media_uno - (1.96 * diferencias)
      )
    ))
  }
