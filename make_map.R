#' @title map function
#' @description  maps locations
#' @details INPUT: 1) data
#' @details OUTPUT: 1) locations
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


make_map <- function(inegi.cost.wgs, thistitle, thisoutput){
  
  map.locs <- tland + 
  tm_shape(inegi.cost.wgs, projection = "wgs84") +
  tm_dots(size = "POBTOT", 
          contrast=1,
          sizes.legend=c(5000,50000,500000,2000000),
          title.size = thistitle) +
  #  tm_grid(col="white", labels.show = FALSE) +
  tm_scale_bar(breaks=c(0,500,1000),position = c("left", "bottom"), text.size = 0.8) +
  tm_compass(type = "arrow", position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)

tmap_save(map.locs, filename=paste0(here(),thisoutput), width=10, height = 6, dpi = 300) 

}