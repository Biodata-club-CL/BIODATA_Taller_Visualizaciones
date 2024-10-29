################################################################################
###
###    Visualización de indicadores de biodiversidad
###    Autores script: Daniel Valdés y Dra. Camila Neder
###                 ?ltimos cambios: 2024-Octubre-24
###                              R-4.4.1
###
################################################################################

#Si el paquete está instalado, se cargaran. Sino, se intalarán de CRAN y luego load
packages <- c("tidyverse",
              "stars",
              "tmap",
              "ggplot2",
              "maps",
              "svDialogs",
              "RColorBrewer"

)

## Ahora instalar y cargar
if (!require("pacman")) install.packages("pacman")
# pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
pacman::p_load(packages, install = TRUE, character.only = TRUE)

rm(packages)

#######################################
## A. CASO GENÉRICO AUTOS
#######################################
library(tidyverse)
library(ggplot2)

# data de mtcars, se muta para agregar una columna al final llamada Modelos
data = mtcars %>%
  mutate(cyl= factor(cyl),
         Model= rownames(mtcars))
head(data)

# creando un gr?fico de burbujas con peso en el eje x y millas por gall?n en eje y (distancia por litro de nafta)
# el tama?o de cada punto es dictado por horsepower
plot1 = data %>% ggplot(aes(x= wt, y= mpg, size= hp)) +
  geom_point(alpha= .5)
plot1
# gr?fico de burbujas con colores agregados en base a los cilindros de cada auto
plot2 = data %>% ggplot(aes(x= wt, y= mpg, size= hp,
                            color= cyl, label= Model)) + # label no parece afectar el grafico
  geom_point(alpha= .5) +
  scale_size(range= c(.1, 15))
plot2


#######################################
## B. CASO ESTUDIO MERLUZA EUROPEA
#######################################
# Gr?fico de burbuja usando data de gbif de Merluccius merluccius
# Referencia
# GBIF.org (13 October 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.seg7f8
# 
# El proceso es similar, solo que esta vez se filtrar? primero el dataframe de ocurrencias porque hay muchas columnas que no tienen datos ?tiles para la visualizaci?n que queremos hacer
# 
# Queremos
# Coordenadas
# Latitud y Longitud
# 
# Cantidad de especimenes observados

# designando directorio de trabajo donde habra que subir el csv
# correr una vez
setwd("sample_data")

# dependencias
library(ggplot2)
library(tidyverse)
# correr una vezd
install.packages("maps")
library(maps)

# designando directorio de trabajo donde habr? que subir el csv
# correr una vez
setwd("sample_data")

# dependencias
library(ggplot2)
library(tidyverse)
# correr una vez
install.packages("maps")
library(maps)

# descargando data de merluccius desde github
url_merluccius = "https://github.com/pocketfall/biodata/raw/refs/heads/main/data/visualizacion_data_biodiversidad/data_merluccius/occurrence.txt"
download.file(url_merluccius, destfile= "data_merluccius.txt")

# leyendo csv
merluccius = read.csv("data_merluccius.txt", sep= "\t", header= T)
head(merluccius)

# filtrando data
merluccius = merluccius %>% filter(!is.na(merluccius$individualCount))
merluccius = data.frame(lat= merluccius$decimalLatitude,
                        lon= merluccius$decimalLongitude,
                        group= merluccius$group,
                        basis_record= merluccius$basisOfRecord,
                        individual_count= merluccius$individualCount)
merluccius = merluccius %>% filter(!is.na(merluccius$lat))
head(merluccius)

# consiguiendo data de mapa
map_merl = map_data("world")
# recortando el mapa para que este mas cerca de la distribucion de Merluccius
map_merl = map_merl[map_merl$long >= -75, ]
map_merl = map_merl[map_merl$long <= 100, ]
map_merl = map_merl[map_merl$lat >= -25, ]
map_merl = map_merl[map_merl$lat <= 80, ]

# dibujando mapa
options(repr.plot.width=15, repr.plot.height=8) #regular tama?o del mapa
plot_merl = ggplot(data= map_merl, aes(x= long, y= lat, group= group)) +
  geom_polygon(col= "white") +
  geom_point(data= merluccius, aes(x= lon, y= lat, group= group,
                                   size= individual_count, col= basis_record),
             alpha= .5) +
  scale_size(range= c(3, 9), breaks= c(10, 30, 50))
plot_merl


####DIRECTORIO Definir path para distintas carpetas dentro
setwd("C:/Users/camu_/Downloads/BIODATA")
path_output <-"C:/Users/camu_/Downloads/BIODATA/03_R_output"

#######################################
## c. CASO ESTUDIO ESTUDIO ANT?RTICO: Caleta Potter
#######################################

###############################################################################
### 1. Cargar datos biol?gicos
###############################################################################
bio.data <- read.delim2("C:/Users/camu_/Downloads/BIODATA/02_original_data/02_bio_data/test_Occurrence_Potter_CNeder.txt",
                          header=TRUE)
View(bio.data)

# ################################################################################
# ### 2. Seleccionar la especie con la que se quiere trabajar
# ################################################################################
{
  chosen_col <- c() # crea una lista vac?a para la columna elegida
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


#######################################
## D. BAR PLOT: ABUNDANCIAS RELATIVAS - OTU Unidad Taxon?mica Operable
#######################################
################################################################################
###  1. Datos desde GitHub
################################################################################
urls_barplots = c(
  "https://github.com/surh/scip_barplot/raw/refs/heads/master/data/rhizo/otu_table.tsv",
  "https://github.com/surh/scip_barplot/raw/refs/heads/master/data/rhizo/otu_taxonomy.tsv",
  "https://github.com/surh/scip_barplot/raw/refs/heads/master/data/rhizo/sample_metadata.tsv"
)
# loop a traves de los links
for (i in urls_barplots) {
  # consigue el ultimo item de separar los links por cada "/"
  file_name = str_split(i, "/")[[1]][12]
  # descargar el contenido del link y nombrarlo
  download.file(i, destfile= file_name)
}

# leyendo data de otus (Cada OTU es un organismo que proviene del mismo origen, 
# cada columna es una muestra, y el valor en cada fila es la abundancia de 
# dicho OTU)
otu_data = read_tsv("otu_table.tsv",
                    col_types = cols(otu_id = col_character (),
                                     .default = col_number()))

head(otu_data)
