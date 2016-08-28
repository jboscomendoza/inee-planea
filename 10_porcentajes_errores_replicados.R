porcentajes_replicados <-
    function(tabla, grupo, prefijo, peso_combi){ #Tabla debe ser una lista, los demás texto

        tabla[grupo][is.na(tabla[grupo])] <- "Perdido" # Etiqueta NA para poder agrupar

        total_parcial <- sum(tabla[[peso_combi]], na.rm = T) # Suma de pesos combinados

        var_nombres <- append(peso_combi, grep(x = names(tabla), prefijo, value = T)) # Vector con nombres de variables de pesos replicados

        var_pesos <- as.vector(sapply(tabla[var_nombres], sum)) # Vector con pesos replicados

        var_split <- split(tabla, tabla[[grupo]]) # Lista con grupos

        var_values <- lapply(var_split, function(x){
            x[, grupo] <- 1 # Cabia valores a 1 para poder estimar la proporción
            (x[, grupo] * x[, var_nombres]) # Multiplica por todos los pesos
        })

        var_suma <- lapply(var_values, function(x) { # Obtiene
            as.vector(
                sapply(x, function(x) {
                    sum(x)/total_parcial #Esto hay que cambiarlo !!! Debe dividir entre la suma de cada peso replicado, no del peso combinado
                })
            )}
        )

        lapply(
            var_suma, function(x){
                perc <- x[1] # Proporcion
                diferencia <- sqrt( # Calculo del Error estandar
                    sum((x[1] - x[2:length(var_nombres)])^2)/20 # x[1] es con peso combinado, de x[2] en adelante con los combinados
                )
                resumen <- data.frame( #Tabla resumen
                    Porcentaje = perc,
                    EE = diferencia,
                    Int_Superior = perc + (diferencia*1.96), # Intervalos de confianza
                    Int_Inferior = perc - (diferencia*1.96)
                )
                round(resumen * 100, 2) # A porcentaje, dos decimales
            }
        )

    }
