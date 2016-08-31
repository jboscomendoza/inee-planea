#Funcion creada con R 3.3.1 ----
porcentajes_replicados <-
    function(tabla, grupo, prefijo, peso_final){ #Tabla debe ser una lista, los demas texto

        # Cambiamos NA a texto para poder agrupar datos perdidos
        tabla[grupo][is.na(tabla[grupo])] <- "Perdidos"

        # Grupo como factor
        tabla[[grupo]] <- as.factor(tabla[[grupo]])

        # Vector con nombres de variables de todos los pesos replicados y peso combinado
        var_nombres <- append(peso_final, grep(x = names(tabla), prefijo, value = T))

        # Vector con sumatoria de pesos replicados y peso combinado de TODO
        var_sumatoria <- as.vector(sapply(tabla[var_nombres], function(x) sum(x, na.rm = T)))

        # La tabla es agrupada por GRUPO
        var_split <- split(tabla, tabla[[grupo]])

        # Vector con la sumatoria de pesos replicados y peso combinado por GRUPO
        var_sumatoria_grupos <- lapply(var_split, function(x){
            sapply(x[, var_nombres], sum, na.rm = T)
        })

        # Obtenemos proporcion, sumatoria de GRUPO entre sumatoria de TODO
        var_proporcion <- lapply(var_sumatoria_grupos, function(x) {
            x / var_sumatoria
        }
        )

        # Tabla resumen
        resumen_general <- lapply(
            var_proporcion, function(x){
                # La proporcion a partir del peso combinado es la que se reporta
                prop <- x[1]
                # Calculo del error estandar
                error_estandar <- sqrt(
                    # x[1] es con peso combinado, de x[2] en adelante con los combinados
                    # Todas las diferencias de x[1] con x[2] se elevan al cuadrado, divide
                    # entre 25 y obtiene raiz cuadrada
                    # Este 25 es igual a 100 * (1 - 0.5) ^ 2, donde 100 es el numero de
                    # pesos y 0.05 es el valor fijado para el coeficiente de Fey
                    sum((x[1] - x[2:length(var_nombres)]) ^ 2 ) / 25
                )
                #  Resumen por grupo
                resumen <- data.frame(
                    Porcentaje = prop,
                    Error_estandar = error_estandar,
                    Intervalo_superior = prop + (error_estandar * 1.96), # Intervalos de confianza
                    Intervalo_inferior = prop - (error_estandar * 1.96)
                )
                # Las proporciones se convierten a porcentaje con dos cifras decimales
                round(resumen * 100, 2)
            }
        )

        # Mejor presentacion para Tabla resumen
        do.call(what = rbind, resumen_general)

    }
