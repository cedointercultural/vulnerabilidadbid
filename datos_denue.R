#' @title Resumir datos DENUE
#' @description  Resumir datos de Unidades economicas por localidad para cada estado costero
#' @details INPUT: 1) Datos DENUE 2020/11 en cvs https://www.inegi.org.mx/app/descarga/?ti=6
#' @details OUTPUT: 1) Unidades economicas por localidad
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


datos_denue <- function(estapagina, unidades.ec){

    estado.datos <- read_xlsx(unidades.ec, sheet = estapagina) # tienes que analizar cada hoja a la vez

    tot.unidades <- estado.datos %>%
      mutate(index = 1) %>%
      group_by(cve_ent, entidad, cve_mun, municipio, cve_loc, localidad) %>%
      summarise(unidades_economicas = sum(index))
    
    return(tot.unidades)

}
