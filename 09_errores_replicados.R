#Funcion creada con R 3.3.1 ----
ee_replicado <-
  function(tabla, variable, prefix, peso_combi, grupo = NULL){
    
    # Funcion ee
    ee <- function(tabla, variable, prefix, peso_combi) {
      # Nombre de las columnas con pesos
      var_nombres <- grep(x = names(tabla), pattern = prefix, value = T)
      
      # Vector con la variable a estimar
      var_variable <- tabla[[variable]]
      
      # Imputamos valores usando la media. No es ideal, pero es eficiente
      var_variable[is.na(var_variable)] <- mean(var_variable, na.rm = T) 
      
      # Vector para iniciar media ponderadas
      var_medias <- vector()
      
      # Contador para asignar a var_medias
      contador = 1 
      
      # Loop para generar los valores de var_medias
      for(i in var_nombres) {
        # Obtenemos el peso muestral
        peso <- tabla[[i]]
        
        # Eliminamos NA
        peso[is.na(peso)] <- 0 
        
        # Valor a var_medias
        var_medias[contador] <-
          # Media ponderada de la variable con el peso
          weighted.mean(var_variable, peso, na.rm = T) 
        # Incrementamos el contador hasta llegar al total de pesos
        contador <- contador + 1 
      }
      
      # Media ponderada con peso combinado
      var_media_combi <- 
        weighted.mean(tabla[[variable]], tabla[[peso_combi]], na.rm = T)
      
      # Calculo de error estandar
      var_errorestandar <- 
        sqrt(
          sum(
            (var_medias - var_media_combi)^2 # Esta operacion esta vectorizada
          )/20 # Es un misterio
        )
      
      # Tabla resumen
      data.frame(
        Media = var_media_combi,
        Error_estandar = var_errorestandar,
        Intervalo_superior = var_media_combi + (1.96 * var_errorestandar),
        Intervalo_inferior = var_media_combi - (1.96 * var_errorestandar)
      )
    }
    
    # Verifica si se ha especificado un grupo
    if(!is.null(grupo)){
      # Si hay un grupo, tabla se divide a partir de este
      tabla <- split(tabla, tabla[grupo])
      # Se aplica ee la tabla dividida. do.call devuelve una mejor presentacion
      do.call(
        what = rbind, 
        lapply(tabla, function(x){
          ee(x, variable, prefix, peso_combi)
        })
      )
      # Si no hay grupo, se aplica ee a la tabla
    } else {
      ee(tabla, variable, prefix, peso_combi)
    }
    
  }
