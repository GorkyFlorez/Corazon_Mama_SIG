#  Cargamos las Librerias ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, ggplot2, ggspatial, colorspace, ggpubr, sf, elevatr, tmap, ggnewscale)

Coranzo   <- st_read("SHP/Corazon.shp")
Coranzon  <- st_transform(Coranzo,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Rio_cora  <- st_read("SHP/Rio_cora.shp")
Rio_cor   <- st_transform(Rio_cora,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

elev = get_elev_raster(Coranzon, z=12)

Poligo_alt    <- crop(elev, Coranzon)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Coranzon)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)


Mapa =ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  geom_sf(data = Rio_cor  ,color="blue")+
  geom_sf(data = Coranzon  ,color="red", fill=NA, size=1.2)+
  theme_void()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))+
  annotate(geom = "text", x = -71.62963, y = -13.4, hjust = 0, vjust = 1, 
           label = "Ing. Gorky Florez Castillo",size = 4, family="serif", color = "black")

ggsave(plot=Mapa,"Mapa/Mama.png",units = "cm",width = 21,height = 25, dpi=2000)

