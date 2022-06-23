#' @title Graficas de indices de vulnerabilidad
#' @description  Graficas espaciales, por estado y por municipio
#' @details INPUT: 1) data
#' @details OUTPUT: 1) graficos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


grafico_datos <- function(cost.data.pob, datos.pca, datos.indice, tabla.edos, nom.indice, num.corte, nom.abr, crs.proj.wgs, crs.proj.utm) {

   
    susc.res <- cost.data.pob %>% 
      dplyr::select(NOM_ENT, NOM_MUN, COM_ID, dec_lon, dec_lat) %>% 
      left_join(datos.indice, by="COM_ID") %>% 
      bind_cols(datos.pca) %>% 
      mutate(PCA_norm = scales::rescale(PC1, to = c(0, 1))) %>% 
      mutate(NOM_ENT = str_to_title(stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"))) %>% 
      #mutate(PCA_norm = (PC1-min(PC1))/(max(PC1)-min(PC1))) %>% 
      left_join(tabla.edos, by=c("NOM_ENT")) %>% 
      arrange(region) %>% 
      mutate(category = if_else(PCA_norm < 0.25, "Baja", if_else(PCA_norm > 0.75, "Alta","Mediana"))) %>% 
      mutate(NOM_MUN = str_to_title(NOM_MUN))
      
      
    
    write_csv(susc.res,here("outputs","analysis",paste0(nom.abr,"_datos_graficos.csv")))
    
    
susc.coords <- susc.res

coordinates(susc.coords) <- c("dec_lon","dec_lat")

#definir proyeccion geografica y proyectar a utm
proj4string(susc.coords) <- crs.proj.wgs
susc.coords.sf <- st_as_sf(susc.coords)
susc.coords.proj <- st_transform(susc.coords.sf, crs.proj.utm) %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord))  %>% # This trick update the factor levels
  mutate(category_fac=factor(category, levels=c("Alta","Mediana","Baja")))  

ent.ord <- susc.res  %>%  distinct(NOM_ENT) %>% pull(NOM_ENT)

susc.res.ord <-  susc.res %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord))  %>% # This trick update the factor levels
  mutate(category_fac=factor(category, levels=c("Alta","Mediana","Baja")))    # This trick update the factor levels

cols <- c("forestgreen","gold","firebrick")

if(nom.indice == "Exposición actual") cols <- c("forestgreen","gold","firebrick")

if(nom.indice == "Susceptibilidad") cols <- c("firebrick","gold","forestgreen")

if(nom.indice == "Exposición SSP585 2050") cols <- c("forestgreen","gold","firebrick")

if(nom.indice == "Exposición SSP126 2050") cols <- c("forestgreen","gold","firebrick")

boxplot.estados <- susc.res.ord %>% 
  ggplot()+
  geom_boxplot(aes(x = PCA_norm, y=NOM_ENT_fac, color = category_fac), show.legend = TRUE) +
  scale_color_manual(name = nom.indice,
                     values = cols)+
  theme_light()+
  labs(y="Entidad",
       x=nom.indice)



ggsave(here("outputs","figures",paste0(nom.indice,"_edos.png")), boxplot.estados, device="png", width = 8, height = 8, dpi = 600)


barplot.data <- susc.res %>% 
  group_by(NOM_ENT,NOM_MUN) %>% 
  summarise(mean_pca=mean(PCA_norm), std_error_pca = std.error(PCA_norm)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(mean_pca_norm = scales::rescale(mean_pca, to = c(0, 1))) %>% 
  mutate(category = if_else(mean_pca_norm < 0.25, "Baja", if_else(mean_pca_norm > 0.75, "Alta","Mediana"))) %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord),
         std_error_max = mean_pca_norm + std_error_pca,
         std_error_min = mean_pca_norm + (-1*std_error_pca)) %>% 
  filter(mean_pca_norm < 0.25) %>% 
  droplevels()


if(nom.indice == "Susceptibilidad") {
  barplot.data <- susc.res %>% 
  group_by(NOM_ENT,NOM_MUN) %>% 
  summarise(mean_pca=mean(PCA_norm), std_error_pca = std.error(PCA_norm)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(mean_pca_norm = scales::rescale(mean_pca, to = c(0, 1))) %>% 
  mutate(category = if_else(mean_pca_norm < 0.25, "Baja", if_else(mean_pca_norm > 0.75, "Alta","Mediana"))) %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord),
         std_error_max = mean_pca_norm + std_error_pca,
         std_error_min = mean_pca_norm + (-1*std_error_pca)) %>% 
  filter(mean_pca_norm > 0.75) %>% 
  droplevels()
}

barplot.data.ord <-  barplot.data %>% 
  mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord))  %>% # This trick update the factor levels
  mutate(category_fac=factor(category, levels=c("Alta","Mediana","Baja")))    # This trick update the factor levels

if(nom.indice == "Capacidad adaptativa") cols <- c("firebrick","gold","forestgreen")

barplot.municipios <- barplot.data.ord %>% 
  #   std_error_susc_max = if_else(is.na(std_error_susc_max),0,std_error_susc_max),
  #   std_error_susc_min = if_else(is.na(std_error_susc_min),0,std_error_susc_min)) %>%    # This trick update the factor levels
  ggplot()+
  # geom_bar(aes(y= mean_susc, x=NOM_MUN, fill=region), stat="identity", show.legend = FALSE) +
  geom_col(aes(y= mean_pca_norm, x=NOM_MUN, fill=category_fac), show.legend = TRUE) +
  #geom_errorbar(aes(y= mean_pca, x=NOM_MUN, ymin = std_error_min, ymax = std_error_max), color="gray50") +
  # scale_fill_brewer()+
  facet_wrap(~ NOM_ENT_fac, scales = "free") +
  coord_flip() +
  theme_light() +
  labs(x = "Municipio",
       y=paste(nom.indice,"media normalizada")) +
  scale_fill_manual(name = nom.indice,
                     values = cols)+
    scale_y_continuous(breaks = c(0,0.25,0.5, 0.75, 1),
                       limits = c(0,1))+
  theme(legend.position="bottom")+
  ggtitle("Valores normalizados, las barras vacias indican los valores mas bajos")
  


ggsave(here("outputs","figures",paste0(nom.indice,"_mun.png")), barplot.municipios, device="png", width = 24, height = 12, dpi=600)


world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

#escribir archivo geografico como shapefile
susc.geo.shp <- susc.coords.proj %>% 
  dplyr::select(COM_ID,NOM_ENT,NOM_MUN,NOM_LOC,PCA_norm, geometry) %>% 
  dplyr::rename(susc_norm=PCA_norm) %>% 
  mutate(NOM_LOC = str_to_title(NOM_LOC))

st_write(susc.geo.shp, here("outputs","shp_files",paste(nom.indice,"shapefile.shp",sep="_")), layer_options = "ENCODING=UTF-8", append = FALSE)


#escribir archivo geografico como raster

susc.coords <- susc.res %>% 
  dplyr::select(dec_lon, dec_lat, PCA_norm)
  
coordinates(susc.coords) <- c("dec_lon","dec_lat")

#definir proyeccion geografica y proyectar a utm
proj4string(susc.coords) <- crs.proj.wgs

#simbolos http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

extent.susc <- extent(susc.coords)
r <- raster(ncols=720, nrows=720, ext = extent.susc)

susc.raster <- rasterize(susc.coords, r, field = "PCA_norm", fun=mean)

susc.raster.scale <- raster_scale(susc.raster)

writeRaster(susc.raster.scale , filename=here("outputs","raster",paste(nom.indice,"raster",sep="_")), format="GTiff", overwrite = TRUE)

if(nom.indice == "Capacidad adaptativa") cols <- c("forestgreen","gold","firebrick")

#mapa por paneles

mapa.susc1 <- ggplot(data = world) +
  geom_sf() +
  #geom_sf(data = susc.coords.proj, aes(size=PCA_norm, color = category), shape = 16) +
  geom_sf(data = susc.coords.proj, aes(color = category_fac), size= 2, shape = 16) +
  scale_color_manual(name = nom.indice,
                     values = cols)+
  # scale_colour_steps(low="aliceblue", high = "dodgerblue4", breaks = c(0.25, 0.5,0.75)) +
  # geom_sf(data = est.2019, fill= NA) + 
  coord_sf(xlim = c(-118, -109), ylim = c(22, 33), expand = FALSE) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                        size = 0.1)) +
  theme_light() +
  labs(color = nom.indice) +
theme(legend.position = "none")
  
mapa.susc2 <- ggplot(data = world) +
  geom_sf() +
  #geom_sf(data = susc.coords.proj, aes(size=PCA_norm, color = category), shape = 16) +
  geom_sf(data = susc.coords.proj, aes(color = category_fac), size= 2, shape = 16) +
  scale_color_manual(name = nom.indice,
                     values = cols)+
  coord_sf(xlim = c(-109, -97), ylim = c(12, 28), expand = FALSE) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                        size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
  theme_light() +
  labs(color = nom.indice) +
  theme(legend.position = "none")


mapa.susc3 <- ggplot(data = world) +
  geom_sf() +
  #geom_sf(data = susc.coords.proj, aes(size=PCA_norm, color = category), shape = 16) +
  geom_sf(data = susc.coords.proj, aes(color = category_fac), size= 2, shape = 16) +
  scale_color_manual(name = nom.indice,
                     values = cols)+
  coord_sf(xlim = c(-97, -85), ylim = c(12, 28), expand = FALSE) +
  xlab("Longitud") + ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                        size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
  theme_light() +
  labs(color = nom.indice)+
  theme(legend.text=element_text(size=rel(1.1)),
        legend.title=element_text(size=12))

mapa.susc.panel <- mapa.susc1 + mapa.susc2 + mapa.susc3 + guide_area() + 
  plot_layout(guides = 'collect')


ggsave(here("outputs","figures",tolower(paste0(nom.indice,"_panel_mapa.png"))), mapa.susc.panel, device="png", width = 10, height = 12)

if(nom.indice == "Capacidad adaptativa") cols <- c("forestgreen","gold","firebrick")

for(eachcategory in c("Baja", "Mediana", "Alta")){

    if(eachcategory=="Baja") thiscol = cols[3]
    if(eachcategory=="Mediana") thiscol = cols[2]
    if(eachcategory=="Alta") thiscol = cols[1]
    
   susc.coords.proj.cat <- susc.coords.proj %>% 
    filter(category==eachcategory)
  
  mapa.susc1 <- ggplot(data = world) +
    geom_sf() +
    #geom_sf(data = susc.coords.proj.cat, aes(size=PCA_norm, color = category), shape = 16) +
    geom_sf(data = susc.coords.proj.cat, aes(color = category_fac), size= 2, shape = 16) +
    scale_color_manual(name = nom.indice,
                       values = thiscol)+
    # scale_colour_steps(low="aliceblue", high = "dodgerblue4", breaks = c(0.25, 0.5,0.75)) +
    # geom_sf(data = est.2019, fill= NA) + 
    coord_sf(xlim = c(-118, -109), ylim = c(22, 33), expand = FALSE) +
    xlab("Longitud") + ylab("Latitud") +
    theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                          size = 0.1)) +
    theme_light() +
    labs(color = nom.indice)+
    theme(legend.text=element_text(size=rel(1.1)),
          legend.title=element_text(size=12))
  
  ggsave(here("outputs","figures",tolower(paste0("mapa_",eachcategory,"_",nom.indice,"_1.png"))), mapa.susc1, device="png", width = 10, height = 12)
  
  mapa.susc2 <- ggplot(data = world) +
    geom_sf() +
    #geom_sf(data = susc.coords.proj.cat, aes(size=PCA_norm, color = category), shape = 16) +
    geom_sf(data = susc.coords.proj.cat, aes(color = category_fac), size= 2, shape = 16) +
        scale_color_manual(name = nom.indice,
                       values = thiscol)+
    coord_sf(xlim = c(-109, -97), ylim = c(12, 28), expand = FALSE) +
    xlab("Longitud") + ylab("Latitud") +
    theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                          size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
    theme_light() +
    labs(color = nom.indice)+
    theme(legend.text=element_text(size=rel(1.1)),
          legend.title=element_text(size=12))

  ggsave(here("outputs","figures",tolower(paste0("mapa_",eachcategory,"_",nom.indice,"_2.png"))), mapa.susc2, device="png", width = 10, height = 12)
  
  mapa.susc3 <- ggplot(data = world) +
    geom_sf() +
   # geom_sf(data = susc.coords.proj.cat, aes(size=PCA_norm, color = category), shape = 16) +
    geom_sf(data = susc.coords.proj.cat, aes(color = category_fac), size= 2, shape = 16) +
    scale_color_manual(name = nom.indice,
                       values = thiscol)+
    coord_sf(xlim = c(-97, -85), ylim = c(12, 28), expand = FALSE) +
    xlab("Longitud") + ylab("Latitud") +
    theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                          size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
    theme_light() +
    labs(color = nom.indice)+
    theme(legend.text=element_text(size=rel(1.1)),
          legend.title=element_text(size=12))

  ggsave(here("outputs","figures",tolower(paste0("mapa_",eachcategory,"_",nom.indice,"_3.png"))), mapa.susc3, device="png", width = 10, height = 12)
  
  #map.grid <- grid.arrange(mapa.susc1, mapa.susc2, mapa.susc3, nrow=2,ncol=2, widths = c(1.5,2),
#heights= c(1.5,2), padding = 0.01)

#map.grid <-ggarrange(mapa.susc1, mapa.susc2, mapa.susc3, widths = c(2,2), heights = c(1,2), ncol = 2, nrow = 2)

#ggsave("susceptibilidad.png", map.grid,device="png", width = 8)

#mapa.susc.panel <- mapa.susc1 + mapa.susc2 + mapa.susc3 + guide_area() + 
#  plot_layout(guides = 'collect')


}





}