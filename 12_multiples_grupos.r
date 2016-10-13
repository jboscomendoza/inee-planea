#Funcion creada con R 3.3.1 ----

source("09_errores_replicados.r")

multiples_grupos <- function(tabla, variables, prefijo, peso_final, grupo = NULL){
    lista_grupos <- 
        lapply(variables,
               function(variable_en_turno){
                   ee_replicado(tabla, variable_en_turno, prefijo, peso_final, grupo)
               })
    
    names(lista_grupos) <- variables
    do.call(
        what = rbind,
        lista_grupos
    )
    
}
