#' @title funcion para vulnerabilidad
#' @description  Calcula datos de vulnerabilidad y produce graficas espaciales, por estado y por municipio
#' @details INPUT: 1) data
#' @details OUTPUT: 1) resultados pca, 2) graficos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


vul_analisis <- function(expo_dat, adap_dat, susc_dat, escenario.nom, expo.base){
  
  crs.proj.wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs.proj.utm <- ("+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 
  
  adap.datos <- read_csv(adap_dat) %>% 
    dplyr::rename(adaptacion=PCA_norm) %>% 
    dplyr::select(adaptacion, NOM_ENT, NOM_MUN, NOM_LOC, COM_ID, region, dec_lon, dec_lat) %>% 
    mutate(NOM_MUN = stri_trans_general(str = NOM_MUN, id = "Latin-ASCII")) %>% 
    mutate(NOM_ENT = stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>% 
    mutate(NOM_LOC = stri_trans_general(str = NOM_LOC, id = "Latin-ASCII")) %>% 
    mutate(NOM_MUN = tolower(NOM_MUN)) %>% 
    mutate(NOM_ENT = tolower(NOM_ENT)) %>% 
    mutate(NOM_LOC = tolower(NOM_LOC))
 
  expo.datos <- read_csv(expo_dat) %>% 
    dplyr::rename(exposicion=PCA_norm)%>% 
    dplyr::select(exposicion, NOM_ENT, NOM_MUN, NOM_LOC, COM_ID, dec_lon, dec_lat) %>% 
    left_join(expo.base, by=c("COM_ID")) %>% 
    mutate(delta_expo = (exposicion - exposicion_base)) %>% 
    mutate(delta_expo_norm = scales::rescale(delta_expo, to = c(0, 1))) %>% 
    mutate(NOM_MUN = stri_trans_general(str = NOM_MUN, id = "Latin-ASCII")) %>% 
    mutate(NOM_ENT = stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>% 
    mutate(NOM_LOC = stri_trans_general(str = NOM_LOC, id = "Latin-ASCII")) %>% 
    mutate(NOM_MUN = tolower(NOM_MUN)) %>% 
    mutate(NOM_ENT = tolower(NOM_ENT)) %>% 
    mutate(NOM_LOC = tolower(NOM_LOC))
  
  susc.datos <- read_csv(susc_dat) %>% 
    dplyr::rename(susceptibilidad=PCA_norm)%>% 
    dplyr::select(susceptibilidad, NOM_ENT, NOM_MUN, NOM_LOC, COM_ID, dec_lon, dec_lat) %>% 
    mutate(NOM_MUN = stri_trans_general(str = NOM_MUN, id = "Latin-ASCII")) %>% 
    mutate(NOM_ENT = stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>% 
    mutate(NOM_LOC = stri_trans_general(str = NOM_LOC, id = "Latin-ASCII")) %>% 
    mutate(NOM_MUN = tolower(NOM_MUN)) %>% 
    mutate(NOM_ENT = tolower(NOM_ENT)) %>% 
    mutate(NOM_LOC = tolower(NOM_LOC)) 
    
  vulne.datos <- adap.datos %>% 
    left_join(susc.datos, by=c("NOM_ENT","NOM_MUN","COM_ID","NOM_LOC","dec_lon","dec_lat")) %>% 
    left_join(expo.datos, by=c("NOM_ENT","NOM_MUN","COM_ID","NOM_LOC","dec_lon","dec_lat")) %>% 
    mutate(vulnerabilidad = ((exposicion + susceptibilidad) - adaptacion)) %>% 
    mutate(vul_norm = scales::rescale(vulnerabilidad, to = c(0, 1))) %>% 
    arrange(region) %>% 
    mutate(escenario = escenario.nom) %>% 
    mutate(category = if_else(vul_norm < 0.25, "Baja", if_else(vul_norm > 0.75, "Alta","Mediana")))
  
  
  write_csv(vulne.datos, here("outputs","analysis",paste0("vulne_datos_",escenario.nom,".csv")))
 
  ent.ord <- vulne.datos  %>%  distinct(NOM_ENT) %>% pull(NOM_ENT)
  
  vul.res.ord <-  vulne.datos %>% 
    mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord)) %>% # This trick update the factor levels
    mutate(category_fac=factor(category, levels=c("Alta","Mediana","Baja")))   # This trick update the factor levels
  
  cols <- c("firebrick","gold","forestgreen")
  
  pointplot.vulne <- vul.res.ord %>% 
    ggplot()+
    geom_point(aes(y= susceptibilidad, x=adaptacion, color=delta_expo_norm)) +
    facet_wrap(~ NOM_ENT_fac) +
    #scale_colour_gradientn(colours= cols) +
    scale_color_gradient(low=cols[2],high=cols[1]) +
    theme_light() +
    labs(x = "Capacidad de adaptación",
         y="Susceptibilidad",
         title = paste("Escenario",escenario.nom),
         color = "Exposición") +
    annotate("text",x = 0.3, y = 0.9, label = "Alta \n vulnerabilidad", color = "slategrey",size = 2.2) +
    annotate("text",x = 0.7, y = 0.1, label = "Baja \n vulnerabilidad", color = "slategrey",size = 2.2) +
    scale_x_reverse()
  
  
   ggsave(here("outputs","figures",paste0(escenario.nom,"_ejes_vulnerabilidad.png")), pointplot.vulne, device="png", width = 10, height = 8)
  
  
  
  
   boxplot.vul <- vul.res.ord %>% 
     ggplot()+
    geom_boxplot(aes(x = vul_norm, y=NOM_ENT_fac, color = category_fac), show.legend = TRUE) +
     scale_color_manual(name=escenario.nom,
                          values = c("firebrick","gold","forestgreen")) +
    theme_light()+
    labs(y="Entidad",
         x="Vulnerabilidad",
         title = escenario.nom)
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_vulnerabilidad_edos.png")), boxplot.vul, device="png", width = 6, height = 8)
  
  
  barplot.data <- vul.res.ord %>% 
    group_by(category_fac,NOM_ENT,NOM_MUN) %>% 
    summarise(mean_pca=mean(vul_norm), std_error_pca = std.error(vul_norm)) %>% 
    filter(mean_pca>0.3) %>% #vulnerabilidad media 
    mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord),
           std_error_max = mean_pca + std_error_pca,
           std_error_min = mean_pca + (-1*std_error_pca))
  
  plot.list <- list()
  
  for(eachstate in 1:length(ent.ord)){
    
    
    this.state <- ent.ord[eachstate]
    print(this.state)
    
    
    barplot.municipios <- barplot.data %>% 
      #   std_error_susc_max = if_else(is.na(std_error_susc_max),0,std_error_susc_max),
      #   std_error_susc_min = if_else(is.na(std_error_susc_min),0,std_error_susc_min)) %>%    # This trick update the factor levels
      filter(NOM_ENT==this.state) %>% 
      ggplot()+
      # geom_bar(aes(y= mean_susc, x=NOM_MUN, fill=region), stat="identity", show.legend = FALSE) +
      geom_col(aes(y= mean_pca, x=NOM_MUN, fill=category_fac), show.legend = FALSE) +
      geom_errorbar(aes(y= mean_pca, x=NOM_MUN, ymin = std_error_min, ymax = std_error_max), color="gray50") +
      scale_fill_manual(values=cols)+
      coord_flip() +
      theme_light() +
      labs(x = "Municipio",
           y=paste(escenario.nom,"media"),
           title = this.state) 
    
    plot.list[[eachstate]] <- barplot.municipios
    
  }
  
  edos.a <- (plot.list[[1]] + plot.list[[2]]) / (plot.list[[3]] + plot.list[[4]]) 
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos1_vul_mun.png")), edos.a, device="png", width = 8, height = 5)
  
  
  edos.b <- plot.list[[5]] 
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos2_vul_mun.png")), edos.b, device="png", width = 6, height = 12)
  
  edos.c <- plot.list[[6]] 
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos3_vul_mun.png")), edos.c, device="png", width = 6, height = 8)
  
  
  edos.d <- (plot.list[[7]] + plot.list[[8]]) / (plot.list[[16]] + plot.list[[17]])
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos4_vul_mun.png")), edos.d, device="png", width = 8, height = 5)
  
  
  edos.e <- (plot.list[[12]] + plot.list[[9]]) 
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos5_vul_mun.png")), edos.e, device="png", width = 8, height = 5)
  
  
  edos.f <- plot.list[[11]] + plot.list[[10]]
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos6_vul_mun.png")), edos.f, device="png", width = 8, height = 5)
  
  
  edos.g <-(plot.list[[13]] + plot.list[[14]]) 
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos7_vul_mun.png")), edos.g, device="png", width = 9, height = 5)
  
  edos.h <- plot.list[[15]] 
  
  ggsave(here("outputs","figures",paste0(escenario.nom,"_edos8_vul_mun.png")), edos.h, device="png", width = 6, height = 12)
  
  
  susc.coords <- vulne.datos
  
  coordinates(susc.coords) <- c("dec_lon","dec_lat")
  
  #definir proyeccion geografica y proyectar a utm
  proj4string(susc.coords) <- crs.proj.wgs
  susc.coords.sf <- st_as_sf(susc.coords)
  susc.coords.proj <- st_transform(susc.coords.sf, crs.proj.utm) %>% 
    mutate(NOM_ENT_fac=factor(NOM_ENT, levels=ent.ord))  %>% # This trick update the factor levels
    mutate(category_fac=factor(category, levels=c("Alta","Mediana","Baja"))) 
  
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  class(world)
  
  extent.susc <- extent(susc.coords)
  r <- raster(ncols=720, nrows=720, ext = extent.susc)
  
  susc.raster <- rasterize(susc.coords, r, field = "vul_norm", fun=mean)
  
  susc.raster.scale <- raster_scale(susc.raster)
  
  writeRaster(susc.raster.scale , filename=here("outputs","raster",paste(escenario.nom,"raster",sep="_")), format="GTiff", overwrite = TRUE)
  
  cols <- c("firebrick","gold","forestgreen")
  
  #mapa por paneles
  
  mapa.susc1 <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = susc.coords.proj, aes(color = category_fac), size= 2, shape = 16) +
    #geom_sf(data = susc.coords.proj, aes(size=PCA_norm, color = category), shape = 16) +
    scale_color_manual(name = escenario.nom,
                       values = cols)+
    # scale_colour_steps(low="aliceblue", high = "dodgerblue4", breaks = c(0.25, 0.5,0.75)) +
    # geom_sf(data = est.2019, fill= NA) + 
    coord_sf(xlim = c(-118, -109), ylim = c(22, 33), expand = FALSE) +
    xlab("Longitud") + ylab("Latitud") +
    theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                          size = 0.1)) +
    theme_light() +
    labs(color = escenario.nom) +
    theme(legend.position = "none")
  
  mapa.susc2 <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = susc.coords.proj, aes(color = category_fac), size= 2, shape = 16) +
    scale_color_manual(name = escenario.nom,
                       values = cols)+
    coord_sf(xlim = c(-109, -97), ylim = c(12, 28), expand = FALSE) +
    xlab("Longitud") + ylab("Latitud") +
    theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                          size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
    theme_light() +
    labs(color = escenario.nom) +
    theme(legend.position = "none")
  
  
  mapa.susc3 <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = susc.coords.proj, aes(color = category_fac), size= 2, shape = 16) +
    scale_color_manual(name = escenario.nom,
                       values = cols)+
    coord_sf(xlim = c(-97, -85), ylim = c(12, 28), expand = FALSE) +
    xlab("Longitud") + ylab("Latitud") +
    theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                          size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
    theme_light() +
    labs(color = escenario.nom)+
    theme(legend.text=element_text(size=rel(1.1)),
          legend.title=element_text(size=12))
  
  mapa.susc.panel <- mapa.susc1 + mapa.susc2 + mapa.susc3 + guide_area() + 
    plot_layout(guides = 'collect')
  
  
  ggsave(here("outputs","figures",tolower(paste0(escenario.nom,"_panel_mapa.png"))), mapa.susc.panel, device="png", width = 10, height = 12)
  
  
  for(eachcategory in c("Alta", "Mediana","Baja")){
    
    if(eachcategory=="Baja")thiscol = cols[3]
    if(eachcategory=="Mediana")thiscol = cols[2]
    if(eachcategory=="Alta")thiscol = cols[1]
    
    susc.coords.proj.cat <- susc.coords.proj %>% 
      filter(category==eachcategory)
    
    mapa.susc1 <- ggplot(data = world) +
      geom_sf() +
      geom_sf(data = susc.coords.proj.cat, aes(color = category), size=2, shape = 16) +
      scale_color_manual(name = escenario.nom,
                         values = thiscol)+
      # scale_colour_steps(low="aliceblue", high = "dodgerblue4", breaks = c(0.25, 0.5,0.75)) +
      # geom_sf(data = est.2019, fill= NA) + 
      coord_sf(xlim = c(-118, -109), ylim = c(22, 33), expand = FALSE) +
      xlab("Longitud") + ylab("Latitud") +
      theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                            size = 0.1)) +
      theme(legend.position = "none") +
      theme_light() +
      labs(color = escenario.nom)
    
    ggsave(here("outputs","figures",tolower(paste0("mapa_",eachcategory,"_",escenario.nom,"_1.png"))), mapa.susc1, device="png", width = 10, height = 12)
    
    mapa.susc2 <- ggplot(data = world) +
      geom_sf() +
      geom_sf(data = susc.coords.proj.cat, aes(size=vul_norm, color = category), shape = 16) +
      scale_color_manual(name = escenario.nom,
                         values = thiscol)+
      coord_sf(xlim = c(-109, -97), ylim = c(12, 28), expand = FALSE) +
      xlab("Longitud") + ylab("Latitud") +
      theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                            size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
      theme_light() +
      labs(color = escenario.nom,
           size = escenario.nom)
    
    ggsave(here("outputs","figures",tolower(paste0("mapa_",eachcategory,"_",escenario.nom,"_2.png"))), mapa.susc2, device="png", width = 10, height = 12)
    
    mapa.susc3 <- ggplot(data = world) +
      geom_sf() +
      geom_sf(data = susc.coords.proj.cat, aes(color = category), size = 2, shape = 16) +
      scale_color_manual(name = escenario.nom,
                         values = thiscol)+
      coord_sf(xlim = c(-97, -85), ylim = c(12, 28), expand = FALSE) +
      xlab("Longitud") + ylab("Latitud") +
      theme(panel.grid.major = element_line(color = gray(0.4), linetype = "dashed", 
                                            size = 0.1), panel.background = element_rect(fill = "aliceblue")) +
      theme_light() +
      labs(color = escenario.nom)
    
    ggsave(here("outputs","figures",tolower(paste0("mapa_",eachcategory,"_",escenario.nom,"_3.png"))), mapa.susc3, device="png", width = 10, height = 12)
    
    #map.grid <- grid.arrange(mapa.susc1, mapa.susc2, mapa.susc3, nrow=2,ncol=2, widths = c(1.5,2),
    #heights= c(1.5,2), padding = 0.01)
    
    #map.grid <-ggarrange(mapa.susc1, mapa.susc2, mapa.susc3, widths = c(2,2), heights = c(1,2), ncol = 2, nrow = 2)
    
    #ggsave("susceptibilidad.png", map.grid,device="png", width = 8)
    
    #mapa.susc.panel <- mapa.susc1 + mapa.susc2 + mapa.susc3 + guide_area() + 
    #  plot_layout(guides = 'collect')
    
    
  }
  
  
  # atl.vul <- vul.res.ord %>% 
  #   arrange(vul_norm) 
  # 
  # loc.ord <- atl.vul  %>%  
  #   distinct(NOM_LOC, NOM_ENT, region) %>% 
  #   arrange(region,NOM_ENT,NOM_LOC) %>% 
  #   distinct(NOM_ENT) %>% 
  #   pull(NOM_ENT)
  # 
  # vul.loc.ord <-  atl.vul %>% 
  #   mutate(NOM_ENT_fac=factor(NOM_ENT, levels=loc.ord)) %>% 
  #   droplevels()# This trick update the factor levels
  # 
  # levels.ord <- vul.loc.ord %>% 
  #   distinct(NOM_ENT) %>% 
  #   pull(NOM_ENT) %>% 
  #   length
  #   
  # 
  #"#DFE3E5"= BC
  #"#335B74"=BCS
  #"#1CADE4" = Sonora
  #"#2683C6" = Sinaloa
  #Veracruz "#27CED7"
  
 #  if(escenario.nom=="SSP126 2050") col.pal <- c("#CEDBE6","#373545","#27CED7","#335B74","#3494BA","#58B6C0","#75BDA7","#7A8C8E","#84ACB6")
 #  if(escenario.nom=="SSP126 2100") col.pal <- c("#CEDBE6","#373545","#27CED7","#335B74","#3494BA","#58B6C0","#75BDA7","#7A8C8E","#84ACB6")
 #  if(escenario.nom=="SSP585 2050") col.pal <- c("#CEDBE6","#373545","#27CED7","#335B74","#3494BA","#58B6C0","#75BDA7","#7A8C8E","#84ACB6")
 #  if(escenario.nom=="SSP585 2100") col.pal <- c("#CEDBE6","#373545","#27CED7","#335B74","#3494BA","#58B6C0","#75BDA7","#7A8C8E","#84ACB6")
 #  
 #  alt.vul.plot <- vul.loc.ord %>% 
 #    order(vulnerabilidad)
 #    ggplot()+
 #    geom_col(aes(y= vul_norm, x=NOM_ENT_fac, fill = category)) +
 #   scale_fill_manual(values=cols, name = "Entidad")+
 #   theme_light()+
 #   labs(y="Vulnerabilidad",
 #        x="Localidad",
 #        title = paste0("10 comunidades más vulnerables ", escenario.nom))+
 #   scale_x_discrete(guide = guide_axis(angle = 90))
 # 
 # ggsave(here("outputs","figures",paste0(escenario.nom,"com_vulnerabilidad.png")), alt.vul.plot, device="png", width = 6, height = 6)
 # 
}
