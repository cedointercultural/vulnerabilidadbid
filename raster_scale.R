#' @title Funcion para normalizar raster
#' @description  Funcion para normalizar raster
#' @details INPUT: 1) raster
#' @details OUTPUT: 1) raster normalizado
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


raster_scale <- function(s){
  
  mnv <- cellStats(s,'min')
  mxv <- cellStats(s,'max')
  x <- (s - mnv) / (mxv - mnv)
  
  return(x)
}