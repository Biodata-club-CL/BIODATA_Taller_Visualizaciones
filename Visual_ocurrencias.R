
###############################################################################
### 1. Cargar datos biológicos
###############################################################################
bio.data <- read.delim2("C:/Users/camu_/Downloads/BIODATA/02_original_data/02_bio_data/test_Occurrence_Potter_CNeder.txt",
                          header=TRUE)
View(bio.data)

# ################################################################################
# ### 2. Seleccionar la especie con la que se quiere trabajar
# ################################################################################
{
  chosen_col <- c() # crea una lista vacia para la columna elegida
  answer <- "no"
  while (answer=="no"){
    svDialogs::dlgMessage("Choose the species you want to work with")$res
    col_names <-c(names(bio.data))
    chosen_col <- utils::select.list(col_names, preselect = NULL, multiple = TRUE,
                                         title = "Choose the columns (one or more) to work with", graphics = TRUE)
    if(length(chosen_col)>=1){
      answer <- "yes"
    }
  }
  rm(answer)
}

################################################################################
###  3. Eliminar 'NA's'
################################################################################
#conservar los datos originales
bio.data1<-bio.data
#trabajar con los nuevos datos posibles de modificar
names(bio.data1)[names(bio.data1) == chosen_col] <- "species"  # Rename the chosen column to 'species'

# eliminar las filas con valores NA en la columna de la especie seleccionada
bio.data1 <- bio.data1[!is.na(bio.data1$species), ]

################################################################################
###  4. Conservar proyección espacial
################################################################################

# Convertir los daatos biológicos en un objeto espacial (sf)
# Aqu? la proyecci?n es WGS 84 (EPSG4326)
# porque las coordenadas de x e y est?n en grados decimales (= longitude and latitude)
sf.bio.data = sf::st_as_sf(bio.data1, coords=c("x","y"), crs= "+proj=longlat +datum=WGS84 +no_defs" )

shape_01 <- sf.bio.data
rm(sf.bio.data)

################################################################################
###  5. Plot de datos biol?gicos sobre variable ambiental
################################################################################
#### graficar ocurrencias sobre mapa de profundidad

library(stars)
###Cargar variable ambiental Tiff
depth <- stars::read_stars(paste("C:/Users/camu_/Downloads/BIODATA/02_original_data/01_raster_data", "bathy.tif", sep="/"))
print(depth)

### Cambiar datos de  0 y 1 a ausencia y presencia
shape_01$species <- factor(shape_01$species, levels = c("0", "1"), labels = c("Abscence", "Presence"))

library(tmap)

Pre_Abs_plot<-tmap::tm_shape(depth) +
    tm_raster(palette = "Blues", n=7,
              style = "pretty") +
    tm_graticules() +
    # tm_layout(legend.show = FALSE) +
    tm_scale_bar(breaks = c(0, 0.5, 1),
                 text.size = 0.5, bg.color = "white", bg.alpha = 0.4, position = c(0.4,0))+ #position = c("center", "BOTTOM")
    tm_credits("BIODATA [CN & DV], 2024",size = 0.5, bg.color = "white", bg.alpha = 0.4, position = c(0.77,0)) + #position = c("RIGHT", "BOTTOM")
    tm_layout(legend.outside = TRUE, legend.outside.position = "right") +
    tm_compass(north = 10,
               type = "4star", size = 1, bg.color = "white",
               bg.alpha = 0.2, position = c(0,0.3)) + #position = c("left", "top")
    tm_shape(shape_01) + tm_dots(col = "species", size = 0.6, shape = 20, palette = c("Absence" = "indianred3", "Presence" = "darkolivegreen3")) #más colores en https://r-charts.com/es/colores/

   Pre_Abs_plot

   # Guardar el mapa como un archivo PNG
   tmap::tmap_save(Pre_Abs_plot, filename = file.path(path_output, "Pre_Abs_plot.png"), width = 10, height = 8, units = "in", dpi = 300)

   rm(shape_01)   

################################################################################
###  6. Plot de riqueza por sitios o ?reas de muestreo
################################################################################
# 
# eliminar las filas con valores NA en la columna de la especie seleccionada
bio.data2 <- bio.data[!is.na(bio.data$Richness), ]   

# Graficar la riqueza de especies
color_area <- c("chocolate4", "yellow", "cadetblue3") #paleta de colores manualmente para conservar mismos colores en cada estació o área

ggplot(bio.data2, aes(x = site, y = Richness, fill = as.factor(site))) +
  geom_bar(stat = "identity") +
  labs(title = "Riqueza acumulada de Especies por Lugar de Muestreo", x = "Lugar de Muestreo", y = "Riqueza de Especies") +
  scale_fill_manual(values = color_area) +  # Usar la paleta de colores manualmente
  theme_minimal() +
  theme(legend.position = "none")

################################################################################
###  7. Plot de ?rea y riqueza por punto
################################################################################
#
bio.data3 <- bio.data[!is.na(bio.data$site), ] # eliminar las filas con valores NA en la columna de la especie seleccionada
sf.bio.data = sf::st_as_sf(bio.data3, coords=c("x","y"), crs= "+proj=longlat +datum=WGS84 +no_defs" )
shape_01 <- sf.bio.data
rm(sf.bio.data)

Locations_plot<-tmap::tm_shape(depth) +
  tm_raster(palette = "Blues", n=7, style = "pretty") +
  tm_graticules() +
  # tm_layout(legend.show = FALSE) +
  tm_scale_bar(breaks = c(0, 0.5, 1),
               text.size = 0.5, bg.color = "white", bg.alpha = 0.4, position = c(0.4,0))+ #position = c("center", "BOTTOM")
  tm_credits("BIODATA [CN & DV], 2024",size = 0.5, bg.color = "white", bg.alpha = 0.4, position = c(0.77,0)) + #position = c("RIGHT", "BOTTOM")
  tm_layout(legend.outside = TRUE, legend.outside.position = "right") +
  tm_compass(north = 10,
             type = "4star", size = 1, bg.color = "white",
             bg.alpha = 0.2, position = c(0,0.3)) + #position = c("left", "top")
  tm_shape(shape_01) + 
  #tm_dots(col = "site", size = 0.6, shape = 20) #para colores aleatorios según la cantidad de sitios
  tm_dots(col = "site", size = "Richness", shape = 20, #ajustar el tamaño según la riqueza 
          scale = 2, # Factor de escala para ajustar el tamaño general
          breaks = c(1, 15, 20, 30), # Control de los pasos de tamaño
          palette = c("InnerCove" = "chocolate4", "Island" = "yellow", "OuterCove" = "cadetblue3" )) #  para colores determinados indicando el nombre del sitio exacto

Locations_plot
tmap::tmap_save(Locations_plot, filename = file.path(path_output, "Puntos de riqueza por área.png"), width = 10, height = 8, units = "in", dpi = 300)
