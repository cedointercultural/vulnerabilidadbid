#' @title funcion para exposicion
#' @description  Calcula datos de exposicion y produce graficas espaciales, por estado y por municipio
#' @details INPUT: 1) data
#' @details OUTPUT: 1) resultados pca, 2) graficos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



expo_raster <- function(thisnum, vars.expo, coste.buff, cost.data.pob){

  print(thisnum)
  data.info <- vars.expo[thisnum,]
  data.layer <- data.info %>% pull(value)
  
  print(data.layer)
  data.name <- data.info %>% pull(var_nom)
  sc.name <- data.info %>% pull(escenario)
  
  if(grepl("[.]shp$",data.layer)){
    
    print("shapefile")
    this.field <- data.info %>% pull(campo)
    
    data.shp <- st_read(data.layer)
    data.shp.wgs <- st_set_crs(data.shp, crs.proj.wgs) 
    
      
    if(grepl("grinundmgw",data.layer)){
      
      data.shp <- st_read("~/vulnerabilidadbid/outputs/shp_files/comunidades_costeras_30km.shp") %>% 
        mutate(riesgo_in = VPH_DRE / VIVPAR_H) %>% 
        mutate(riesgo_in = if_else(is.na(riesgo_in), 0, riesgo_in)) %>% 
        mutate(riesgo_in =if_else(is.infinite(riesgo_in), 0, riesgo_in))

      data.shp.wgs <- st_set_crs(data.shp, crs.proj.wgs) 
      
      this.field = "riesgo_in"
    }
    
    extent.disp <- extent(data.shp.wgs)
    r.template <- raster(ncols=720, nrows=720, ext = extent.disp)
    crs(r.template) <- crs.proj.wgs
    
    raster.data <- rasterize(data.shp.wgs, r.template, field = this.field, fun=mean)

    if(grepl("mareatoxgw",data.layer)){
      
      data.spd <- as(data.shp.wgs,"Spatial")
      
      # Create an empty grid where n is the total number of cells
      grd              <- as.data.frame(spsample(data.spd, "regular", n=518400))
      names(grd)       <- c("X", "Y")
      coordinates(grd) <- c("X", "Y")
      gridded(grd)     <- TRUE  # Create SpatialPixel object
      fullgrid(grd)    <- TRUE  # Create SpatialGrid object
      
      # Add data.spd projection information to the empty grid
      proj4string(data.spd) <- crs.proj.wgs
      proj4string(grd) <- crs.proj.wgs
      
      # Interpolate the grid cells using a power value of 2 (idp=2.0)
      P.idw <- gstat::idw(DENSIDAD ~ 1, data.spd, newdata=grd, idp=2.0)
      
      # Convert to raster object
      raster.data <- raster(P.idw)
      #https://mgimond.github.io/Spatial/interpolation-in-r.html#idw
      }
    
  } else if (grepl("[.]tif$",data.layer)){

    raster.data <- raster(data.layer)
    
    proj4string(raster.data) <- crs.proj.wgs
    
    }
 
    buffer.com <- exact_extract(raster.data, coste.buff.sf, 'mean') %>% 
      as_tibble
    
    buffer.data <- buffer.com %>% 
      bind_cols(cost.data.pob) %>% 
      mutate(variable = data.name, 
             escenario = sc.name,
             index_var = 1:nrow(.))  %>% 
      mutate(value=if_else(is.nan(value),0,value)) %>% 
      mutate(value=if_else(is.na(value),0,value))
  
  print(buffer.data %>% pull(value) %>% is.na(.) %>% any())
  print(buffer.data %>% pull(value) %>% is.nan(.) %>% any())
  
  write_csv(buffer.data,here("inputs","exposure",paste(data.name,sc.name,"rasterdata.csv",sep="_")))
}
