#' @title Graficar vulnerabilidad entidades especificas
#' @description  Produce graficas por estado
#' @details INPUT: 1) data
#' @details OUTPUT: 1) graficos
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


vul_analisis_entidad <- function(escenario.nom, ent.ord){
  


  vulne.datos <- read_csv(here("outputs","analysis",paste0("vulne_datos_",escenario.nom,".csv"))) %>% 
    filter(NOM_ENT %in% ent.ord) 
  
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


ggsave(here("outputs","figures",paste0(escenario.nom,"_ejes_vulnerabilidad_ent.png")), pointplot.vulne, device="png", width = 10, height = 8)




boxplot.vul <- vul.res.ord %>% 
  ggplot()+
  geom_boxplot(aes(x = vul_norm, y=NOM_ENT_fac, color = category_fac), show.legend = TRUE) +
  scale_color_manual(name=escenario.nom,
                     values = c("firebrick","gold","forestgreen")) +
  theme_light()+
  labs(y="Entidad",
       x="Vulnerabilidad",
       title = escenario.nom)

ggsave(here("outputs","figures",paste0(escenario.nom,"_vulnerabilidad_edos_ent.png")), boxplot.vul, device="png", width = 6, height = 8)


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

ggsave(here("outputs","figures",paste0(escenario.nom,"_edos1_vul_mun_ent.png")), edos.a, device="png", width = 8, height = 5)

}