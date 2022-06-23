#' @title Extract raster values
#' @description  transform polygons to raster and extract values in a 30 km buffer
#' @details INPUT: 1) polygons
#' @details OUTPUT: 1) raster values for each point in a buffer
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



calc_buffer <- function(thispolygon,thisfield,inegi.cost.utm, thisfunction){

  thispolygon.utm <- st_transform(thispolygon,crs.proj.utm)
  
  extent.disp <- extent(thispolygon.utm)
  r.template <- raster(ncols=720, nrows=720, ext = extent.disp)
  
  pol.raster <- raster::rasterize(thispolygon.utm, r.template, field = thisfield, fun=mean)
  
  raster.extract <- raster::extract(pol.raster,             # raster layer
                              inegi.cost.utm,   # SPDF with centroids for buffer
                              buffer = 30000,     # buffer size, units depend on CRS
                              fun=thisfunction,         # what to value to extract
                              df=TRUE)
  return(raster.extract)
}
