source("09_errores_replicados.R")

valor_replicado <-
    function(tabla, sufijo, prefijo, peso_final, grupo = NULL){
        vector_plausibles <-
            grep(
                pattern = paste0("^PV.", sufijo, "$"),
                x = names(tabla),
                value = T
            )

        lista_grupos <- lapply(vector_plausibles, function(x){
            ee_replicado(tabla, x, prefijo, peso_final, grupo)
        })

        tabla_grupos <- do.call(
            what = rbind,
            lapply(lista_grupos, function(x){
                Grupo <- row.names(x)
                cbind(x, Grupo)
            })
        )

        lista_grupos <- split(tabla_grupos, tabla_grupos$Grupo)

        varianza <- function(df_plausible){
            df_plausible

            df_plausible$Sample_var <- df_plausible$Error_estandar ^2
            df_plausible$Imput_dif <- df_plausible$Media - mean(df_plausible$Media)

            media_plausibles <-  mean(df_plausible$Media)

            Sampling_variance   <- mean(df_plausible$Sample_var)
            Imputation_variance <- sum(df_plausible$Imput_dif) / (length(vector_plausibles) - 1)

            error_plausibles <- sqrt(Sampling_variance + (1.2 * Imputation_variance))

            tabla_finales <-
                data.frame(
                    Media = media_plausibles,
                    Error_estandar = error_plausibles,
                    Intervalo_superior = media_plausibles + (1.96 * error_plausibles),
                    Intervalo_inferior = media_plausibles - (1.96 * error_plausibles)
                )

            tabla_finales
        }

        do.call(
            what = rbind,
            lapply(lista_grupos, varianza)

        )
    }
