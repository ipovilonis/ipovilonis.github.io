#version de R 3.6.1
#tomado de https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel","ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("raster")
#creo un objeto world sobre la base de la info rnaturalearth, con ne_countries puedo seleccionar                  
#que entra del mundo a nivel continente o pais/es.
#Esto da el mundo entero, el objeto world tiene tambien estadisticas
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
geom_sf()
#De esta manera solo selecciona america
america <- ne_countries(scale = "medium", continent = c("south america", "north america"), returnclass = "sf")
ggplot(data = america) + geom_sf()  

#tambien puedo seleccionar paises
paises <- ne_countries(scale = "medium", country= c( "mexico", "argentina", "bolivia", "chile", "uruguay", "peru"), continent = NULL, returnclass = "sf")
ggplot(data = paises) + geom_sf()
#para ver los nombres de los paises
world$name_sort
#para ver los nombres de los continentes
levels(as.factor(world$continent))
#para recortar el mapa mundial puedo usar coord_sf
#tambien agrego el norte y la barra
#puedo agregar anotaciones
ggplot(data = world) +  geom_sf(fill= "pink") + coord_sf(xlim = c(-125.15, -33.12), ylim = c(-40.65, 40.00), expand = FALSE) + annotation_scale(location = "bl", width_hint = 0.3) + annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) + annotate(geom = "text", x = -102.183, y = 20.367, label = "Central", fontface = "italic", color = "grey22", size = 4)  +annotate(geom = "text", x = -63.717, y = 9.017, label = "Costeno", fontface = "italic", color = "red", size = 4)+annotate(geom = "text", x = -82.667, y = 23.083, label = "Cuban Flint", fontface = "italic", color = "red", size = 4)     
#si quiero guardarlo como tiff con buena resolucion     
#si quiero guardarlo como tiff con buena resolucion
tiff("mapablanco.tiff", res=600, width=4800, height=4450)
ggplot(data = world) +  geom_sf(fill= "pink") + coord_sf(xlim = c(-125.15, -33.12), ylim = c(-40.65, 43.00), expand = FALSE) + annotation_scale(location = "bl", width_hint = 0.3) + annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) + annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", fontface = "italic", color = "grey22", size = 4)  +annotate(geom = "text", x = -90, y = -26, label = "Gulf of Mexico", fontface = "italic", color = "red", size = 4)
dev.off()

#Para poner la division administrativa primero baje el shapefile y luego lo leo con la opcion st_read de sf
arg<- st_read("ARG_adm1.shp")
#para plotearlo
ggplot(arg) + geom_sf()
#para plotearlo y recortarlo junto con el otro. Sacado de #https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
ggplot(data = world) +  geom_sf() + geom_sf(data=arg) + coord_sf(xlim = c(-125.15, -33.12), ylim = c(-40.65, 43.00), expand = FALSE) + annotation_scale(location = "bl", width_hint = 0.3) + annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) 
           
#para poner puntos en el mapa.Primero crear el dataframe con los puntos
sites <- data.frame(longitude = c(-65.144005, -67.109), latitude = c(-26.479005,-26.83))

#para plotearlo sobre el mapa uso geom_point
ggplot(data = world) +  geom_sf(fill="pink") + geom_sf(data=arg, fill="pink") + geom_point(data = sites, aes(x = longitude, y = latitude), size = 1,shape = 23, fill = "darkred") +coord_sf(xlim = c(-125.15, -33.12), ylim = c(-40.65, 40.00), expand = FALSE) + annotation_scale(location = "bl", width_hint = 0.3) + annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) 
#para poner puntos y nombres 
sites2<- data.frame(longitude = c(-102.183, -63.717, -82.667, -112.667, -80.417, -71.500, -105.383, -106.350, -97.883,-95.233,-74.250), latitude = c(20.367, 9.017, 23.083, 36.250, 37.217, 42.250, 22.333, 35.517, 22.283, 16.333, -12.933))
ggplot(data = world) +  geom_sf(fill= "pink") + coord_sf(xlim = c(-125.15, -33.12), ylim = c(-40.65, 43.00), expand = FALSE) + annotation_scale(location = "bl", width_hint = 0.3) + annotation_north_arrow(location = "bl", which_north = "true",pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) + annotate(geom = "text", x = -102.183, y = 20.367, label = "Central", fontface = "italic", color = "grey22", size = 2)  +annotate(geom = "text", x = -63.717, y = 9.017, label = "Costeno", fontface = "italic", color = "red", size = 2)+annotate(geom = "text", x = -82.667, y = 23.083, label = "Cuban Flint", fontface = "italic", color = "red", size = 2)+ geom_point(data = sites2, aes(x = longitude, y = latitude), size = 1.5,shape = 23, fill = "black") + annotate(geom = "text", x = -112.667, y = 36.250, label = "Havasupai", fontface = "italic", color = "red", size = 2) + annotate(geom = "text", x = -80.417, y = 37.217, label = "Hickory", fontface = "italic", color = "red", size = 2)  + annotate(geom = "text", x = -71.500, y = 42.250, label = "Longfellow", fontface = "italic", color = "red", size = 2) + annotate(geom = "text", x = -105.383, y = 22.333, label = "Reventador", fontface = "italic", color = "red", size = 2) + annotate(geom = "text", x = -106.350, y = 35.517, label = "Santo Domingo", fontface = "italic", color = "red", size = 2)+ annotate(geom = "text", x = -97.883, y = 22.283, label = "Tuxpeno", fontface = "italic", color = "red", size = 2) + annotate(geom = "text", x = -95.233	, y = 16.333, label = "Zapalote Chico", fontface = "italic", color = "red", size = 2) + annotate(geom = "text", x = -74.250	, y = -12.933, label = "Chullpi", fontface = "italic", color = "red", size = 2) 
#Para poner relieve (tambien puede ser otro raster, pero hay que ver los detalles)
#instrucciones mas completas en https://www.r-bloggers.com/2018/08/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/
#Bajo los datos SRTM, estan divididos en cuadrantes de 5x5, se da un punto cualquiera dentro del cuadrante para elegirlo
dem.raster <- getData("SRTM", lat = -23.8, lon = -67.1, download = TRUE)
#Este objeto hay que trasnformarlo en dataframe, son muy grandes, conviene cortar, pero aqui no lo hago
dem.m  <-  rasterToPoints(dem.raster)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")
head(dem.df)
#Para plotearlo sobre el mapa de argentina, pinte en escala de grises el raster y recorte solo una parte del mapa de arg
ggplot(arg) + geom_sf() + geom_raster(data = dem.df, aes(lon, lat, fill = alt), alpha = .45) + scale_fill_gradient(low="black", high= "white") + coord_sf(xlim = c(-72.15, -60.12), ylim = c(-31.65, -25.00), expand = FALSE)
#Para plotearlo pero con colores de relieve
ggplot(arg) + geom_sf() + geom_raster(data = dem.df, aes(lon, lat, fill = alt), alpha = .45) + scale_fill_gradientn(colours = terrain.colors(100)) + coord_sf(xlim = c(-72.15, -60.12), ylim = c(-28, -20), expand = FALSE)
#Si quiero usar mas de un cuadrante de relieve bajo los que quiero, los convierto por separado en dataframe y luego los uno
dem.df3<- rbind(dem.df, dem.df2)
