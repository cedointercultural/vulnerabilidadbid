#' @title funcion para exposicion
#' @description  Calcula datos de exposicion y produce graficas espaciales, por estado y por municipio
#' @details INPUT: 1) data
#' @details OUTPUT: 1) resultados pca, 2) graficos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com




expo_analisis <- function(raster.layer, prod.raster.data, coste.buff.sf, cost.data.pob, tabla.edos,  nom.indice, nom.corte, nom.abr){

  raster.data <- raster(raster.layer)
  
  raster.extract <- raster::extract(pol.raster,             # raster layer
                              inegi.cost.utm,   # SPDF with centroids for buffer
                              buffer = 30000,     # buffer size, units depend on CRS
                              fun=thisfunction,         # what to value to extract
                              df=TRUE)
   <- raster(raster.layer)
  
  raster.extract <- raster::extract(pol.raster,             # raster layer
                                    inegi.cost.utm,   # SPDF with centroids for buffer
                                    buffer = 30000,     # buffer size, units depend on CRS
                                    fun=thisfunction,         # what to value to extract
                                    df=TRUE)
  
  proj4string(sst.raster.data) <- crs.proj.wgs
  
  buffer.sst.com <- exact_extract(sst.raster.data, coste.buff.sf, 'mean') %>% 
    as_tibble
  
  proj4string(prod.raster.data) <- crs.proj.wgs
  
  buffer.prod.com <- exact_extract(prod.raster.data, coste.buff.sf, 'mean') %>% 
    as_tibble
  
  buffer.com <- buffer.sst.com %>% 
    bind_cols(buffer.prod.com) %>% 
    setNames(c("sst","prod")) 
  
  expo.datos <- cost.data.pob %>% 
    dplyr::select(NOM_LOC, COM_ID) %>% 
    bind_cols(buffer.com)
  
  pca_factor(expo.datos, datasetname = "Exposicion")
  
  expo.pca <- read_csv("factor_scores_Exposicion.csv") %>% 
    mutate(PC1 = if_else(is.na(PC1), 0, 
                         if_else(is.nan(PC1), 0, 
                                 if_else(is.infinite(PC1), 0, PC1))))
  
    
  grafico_datos(cost.data.pob, datos.pca=expo.pca, datos.indice=expo.datos, tabla.edos, nom.indice, nom.corte, nom.abr)
  #el numero de corte es el valor que se usa para hacer un subjuego de datos y mostrar para municipio. En este caso solo se muestran los valores normalizados mayores a 0.402 que corresponden a una anomalia positiva ~0.5 C
  
  
  
  
}
