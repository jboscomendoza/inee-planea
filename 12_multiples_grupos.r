#Funcion creada con R 3.3.1 ----

source("09_errores_replicados.r")

multiples_grupos <- function(por_grupo){
  grupo_lista = NULL
  factores <- c("MR1", "MR2", "MR3", "MR4")
  for(i in factores){
    grupo_lista[[i]] <- split(p15, p15$GRADO) %>%
      lapply(ee_replicado, variable = i, prefijo = "W_FSTR", peso_final = "W_FSTUWT", grupo = por_grupo) %>%
      do.call(rbind, .)
  }
  grupo_lista
}
