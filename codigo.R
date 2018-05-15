# ---------------------------------------------------------------------------- #
#                           PROYECTO DE FIN DE GRADO                           #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# TITULO: Análisis de Información Geográfica mediante QGIS y R 
# SUBTITULO: Modelo Digital del Terreno (MDT)
# AUTORA: Gema Correa Fernández
# TUTOR: José Samos Jiménez
#
# Ingeniería Informática
# Universidad de Granada
# ---------------------------------------------------------------------------- #

# Establecemos una semilla para obtener siempre la misma salida
set.seed(427)

# Establecemos el directorio de trabajo
setwd("/Users/gema/Dropbox/TFG/codigo")

# Cargamos las librerías necesarías
library(raster)   # para trabajar con archivos raster
library(rgdal)    # para trabajar con archivos raster
library(mapview)  # mapa interactivo en R

#### ------------------------ Leer archivos raster ------------------------ ####
load_raster <- function (path_raster) {
  raster <- raster(path_raster)
  raster
}

mdt1 <- load_raster("datos/h10_1009_3-4/h10_1009_3-4.tif")
mdt2 <- load_raster("datos/h10_1042_1-2/h10_1042_1-2.tif")
mdt <- load_raster("datos/PNOA_MDT25_ETRS89_HU30_1009_LID.ASC")

#### ----------------------------- Visualizar ----------------------------- ####
visualize <- function (raster, titulo, plot=T, contour=F) {
  if (plot==T) 
    plot(raster, main=titulo)
  else 
    spplot(raster)
  
  if (contour==T)
    contour(raster, add=TRUE)
}

visualize(mdt)
mdt1
visualize(mdt1, titulo="Modelo Digital del Terreno")
visualize(mdt1, titulo="Modelo Digital del Terreno", plot=T, contour=T)
spplot(mdt1, xlim=c(438855,446505), ylim=c(4113425,4118315))

mdt2
visualize(mdt2, titulo="Modelo Digital del Terreno")
visualize(mdt2, titulo="Modelo Digital del Terreno", plot=T, contour=T)
spplot(mdt2, xlim=c(453495,461165), ylim=c(4085605,4090485))


# ---------------------------------------------------------------------------- #
# Para trabajar con MDT podremos tanto usar archivos raster descargados de 
# fuentes de información oficiales, como archivos de base de datos de paquetes,
# como crearnos nosotros un raster.

# Vamos a visualizar en el mapa, los datos con los que vamos a trabajar
# Podemos ver de la zona que hemos extraído del mapa
esp <- getData('alt', country = 'ESP')
mapview(esp) # Plot elevation data example 
# y visualizamos España en el mapa

# Visualizamos el trocito de archivos que hemos descargado
mapview(mdt1)

# Visualizamos el trocito de archivos que hemos descargado
mapview(mdt2)


#### -------------------- Punto máximo y mínimo de un DEM ------------------- ####
# A partir de un archivo, vamos a extraer su punto máximo y mínimo para un raster
get_max_min_point <- function(raster) {
  points_max <- rasterToPoints(raster)
  points_min <- rasterToPoints(raster)
  
  row_max <- which(points_max == max(points_max[,3]), arr.ind = TRUE) # vemos la fila del maximo
  row_min <- which(points_min == min(points_min[,3]), arr.ind = TRUE) # vemos la fila del minimo
  
  xy_max <- points_max[row_max[1],]; xy_max <- xy_max[1:2] # obtenemos sus coordenadas
  xy_min <- points_min[row_min[1],]; xy_min <- xy_min[1:2] # obtenemos sus coordenadas
  
  xy_max <- as.vector(xy_max) # convertimos a vector
  xy_min <- as.vector(xy_min) # convertimos a vector
  
  plot(raster); points(xy_max[1], xy_max[2], col="blue", pch=18)
  plot(raster); points(xy_min[1], xy_min[2], col="red", pch=20)
}  
  
get_max_min_point(mdt1) # para mdt1
get_max_min_point(mdt2) # para mdt2


# ---------------------------------------------------------------------------- #
library(fasteraster)
mdt1 <- raster("datos/h10_1009_3-4/h10_1009_3-4.tif")
mdt1[] <- runif(ncell(mdt1), 0,1)
mdt111 <- as.matrix(mdt1[] )
class(mdt111)
min(mdt111)
max(mdt111)
# raster, from = 0, to = 1, step = 0.1, precision = 1L,exclave = TRUE
polygons <- raster2vector(mdt111, 0, 1, 1, 1) 
polygons
# image(volcano)
image(polygons, col = rev(terrain.colors(5)), useRaster = TRUE)


# ---------------------------------------------------------------------------- #
climate <- getData('worldclim', var='bio', res=2.5)
plot(climate)

tmin <- getData("worldclim", var = "tmin", res = 10)  # this will download 
plot(tmin)


# ---------------------------------------------------------------------------- #
tmin1 <- raster(paste(getwd(), "/wc10/tmin1.bil", sep = ""))  # Tmin for January
tmin1
library(dismo) 
laurus <- gbif("Laurus", "nobilis")
locs <- subset(laurus, select = c("country", "lat", "lon"))
head(locs) 
projection(tmin1) <- crs.geo
locs$tmin1 <- extract(tmin1, locs)  # raster values 
# are incorporated to the dataframe
head(locs)
plot(tmin1.c)
reg.clim <- extract(tmin1.c, drawExtent())  # click twice to 
# draw extent of the region of interest
summary(reg.clim)

locs2ras <- rasterize(locs.gb, tmin1, field = rep(1, nrow(locs.gb)))
locs2ras
plot(locs2ras, xlim = c(-10, 10), ylim = c(45, 60), legend = F)
data(wrld_simpl)
plot(wrld_simpl, add = T)


#### -------------------------- Pendiente (slope) ------------------------- ####
# Para obtener la pendiente del terreno, hacemos uso de 'terrain'
# Por defecto 'terrain' usa radianes, cambiar a grados

slope.mdt <- terrain(mdt1, opt='slope', unit = 'degrees') # entre 0 y 90º
plot(slope.mdt, col = rainbow(8)); 
title("Pendiente del terreno")

slope.mdt <- terrain(mdt2, opt='slope', unit = 'degrees') # entre 0 y 90º
plot(slope.mdt, col = rainbow(8)); 
title("Pendiente del terreno")


# ---------------------------------------------------------------------------- #
logo <- stack(system.file("external/rlogo.grd", package="raster"))
pts <- matrix(c(48.243420, 48.243420, 47.985820, 52.880230, 49.531423, 46.182616, 54.168232,
                69.624263, 83.792291, 85.337894, 74.261072, 83.792291, 95.126713, 
                84.565092, 66.275456, 41.803408, 25.832176, 3.936132, 18.876962, 17.331359,7.048974, 
                13.648543, 26.093446, 28.544714, 39.104026, 44.572240, 51.171810, 56.262906, 46.269272, 38.161230, 
                30.618865, 21.945145, 34.390047, 59.656971,
                69.839163, 73.233228, 63.239594, 45.892154, 43.252326, 28.356155) , ncol=2)
v <- extract(logo, pts)
bc <- bioclim(v)
p1 <- predict(logo, bc)
p2 <- predict(logo, bc, tails=c('both', 'low', 'high'))


# ---------------------------------------------------------------------------- #
d <- getValues(slope.mdt)
d <- d[!is.na(d)]
max <- max(d)
max
max1 <-  rasterToPoints(d) 

max1# Coerce < max to NA and coerce result to points 
rMax <- mdt1
rMax
rMax[rMax != rMax@data@max] <- NA 
( mdt1.pts <- rasterToPoints (rMax) )  

plot(slope.mdt, col = rainbow(8)); 
points(4118080, 441400, col="blue", pch=19)


# ---------------------------------------------------------------------------- #
# You could also use the raster specific Which or which.max functions. 
i <- which.max(r)
xy.max <- xyFromCell(r, i)
plot(r, col=terrain.colors(4))
points(xy.max, pch=19, col="black")

# Or for a more general application of Which
i <- Which(r >= 0.85, cells=TRUE)
xy.max <- xyFromCell(r, i)
plot(r)
points(xy.max, pch=19, col="black")

# If you prefer a raster object set cells=FALSE
i <- Which(r >= 0.85, cells=FALSE)
plot(i)


# ---------------------------------------------------------------------------- #
require(raster)
r <- raster(ncols=100, nrows=100)
r[] <- runif(ncell(r), 0,1)

# Coerce < max to NA and coerce result to points 
rMax <- r
rMax[rMax != rMax@data@max] <- NA 
( r.pts <- rasterToPoints (rMax) )  

# You could also use the raster specific Which or which.max functions. 
i <- which.max(r)
xy.max <- xyFromCell(r, i)
plot(r)
points(xy.max, pch=19, col="black")

# Or for a more general application of Which
i <- Which(r >= 0.85, cells=TRUE)
xy.max <- xyFromCell(r, i)
plot(r)
points(xy.max, pch=19, col="black")

# If you prefer a raster object set cells=FALSE
i <- Which(r >= 0.85, cells=FALSE)
plot(i)


# ---------------------------------------------------------------------------- #
# leamos un raster de altitud con resolución de 1 km
altitud <- raster("datos/h10_1042_1-2/h10_1042_1-2.tif")

plot(altitud)

poligonos <- readOGR(("poligonos.shp"),layer="poligonos")

plot(poligonos,col=colores[poligonos$intensidad],add=TRUE)
plot(puntos,add=TRUE)

puntos <- readOGR(dsn=paste0(getwd(),"/puntos.shp"),layer="puntos")
lineas <- readOGR(dsn=paste0(getwd(),"/lineas.shp"),layer="lineas")
poligonos <- readOGR(dsn=paste0(getwd(),"/poligonos.shp"),layer="poligonos")


extraccion_puntos <- extract(altitud,puntos)

# valor de altitud de cada punto
extraccion_puntos

extraccion_poligonos <- extract(altitud,poligonos,fun=mean,na.rm=TRUE)

# valor de altitud promedio por polígono
extraccion_poligonos


#### ------------------------- Orientación (aspect) ------------------------ ####

# Para obtener la orientación del terreno, hacemos uso de 'terrain' 
# Por defecto 'terrain' usa radianes, usar grados

aspect.mdt <- terrain(mdt, opt='aspect', unit = 'degrees') # entre 0 y 360º
plot(aspect.mdt, col = terrain.colors(30));


#### ------------------------ Sombreado (hillshade) ------------------------ ####

# http://neondataskills.org/R/NEON-lidar-flood-CO13

hillshade.mdt <- hillShade(slope.mdt, aspect.mdt, angle=45, direction=190)
plot(hillshade.mdt, col = terrain.colors(40));


#### -------------------------------- LiDAR -------------------------------- ####

DTM_pre <- raster("datos/lidar/pre-flood/preDTM3.tif")
DTM_post <- raster("datos/lidar/post-flood/postDTM3.tif")
plot(DTM_pre)


LASfile <- system.file("datos/lidar/pre-flood/preDTM3.tifz")
class(LASfile)
lidar <- readLines(LASfile)
plot(lidar)

DTMpre_hill <- raster("datos/lidar/pre-flood/preDTMhill3.tif")
DTMpost_hill <- raster("datos/lidar/post-flood/postDTMhill3.tif")

# plot Pre-flood w/ hillshade
plot(DTMpre_hill,
     col=grey(1:100/100),  # create a color ramp of grey colors for hillshade
     legend=FALSE,         # no legend, we don't care about the grey of the hillshade
     main="Four Mile Canyon Creek, Boulder County\nPre-Flood",
     axes=FALSE)           # makes for a cleaner plot, if the coordinates aren't necessary

plot(DTM_pre, 
     axes=FALSE,
     alpha=0.5,   # sets how transparent the object will be (0=transparent, 1=not transparent)
     add=T)  # add=TRUE (or T), add plot to the previous plotting frame

# plot Post-flood w/ hillshade
# note, no add=T in this code, so new plotting frame. 
plot(DTMpost_hill,
     col=grey(1:100/100),  
     legend=FALSE,
     main="Four Mile Canyon Creek, Boulder County\nPost-Flood",
     axes=FALSE)

plot(DTM_post, 
     axes=FALSE,
     alpha=0.5,
     add=T)

DoD <- DTM_post-DTM_pre

plot(DoD,
     main="Digital Elevation Model of Difference (DoD)",
     axes=FALSE)

# histogram of values in DoD
hist(DoD)

# Color palette for 5 categories
difCol5 = c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2c7bb6")

# Alternate palette for 7 categories - try it out!
#difCol7 = c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4")

# plot hillshade first
plot(DTMpost_hill,
     col=grey(1:100/100),  # create a color ramp of grey colors
     legend=FALSE,
     main="Elevation Change Post Flood\nFour Mile Canyon Creek, Boulder County",
     axes=FALSE)

# add the DoD to it with specified breaks & colors
plot(DoD,
     breaks = c(-5,-1,-0.5,0.5,1,10),
     col= difCol5,
     axes=FALSE,
     alpha=0.4,
     add =T)


# plot the rasters you want to crop from 
plot(DTMpost_hill,
     col=grey(1:100/100),  # create a color ramp of grey colors
     legend=FALSE,
     main="Four Mile Canyon Creek, Boulder County\nPre-Flood",
     axes=FALSE)

plot(DoD,
     breaks = c(-5,-1,-0.5,0.5,1,10),
     col= difCol5,
     axes=FALSE,
     alpha=0.4,
     add =T)

# crop by designating two opposite corners
cropbox1<-drawExtent()  

cropbox1

cropbox2<-c(473792.6,474999,4434526,4435453)

# crop desired layers to this cropbox
DTM_pre_crop <- crop(DTM_pre, cropbox2)
DTM_post_crop <- crop(DTM_post, cropbox2)
DTMpre_hill_crop <- crop(DTMpre_hill,cropbox2)
DTMpost_hill_crop <- crop(DTMpost_hill,cropbox2)
DoD_crop <- crop(DoD, cropbox2)

# plot all again using the cropped layers

# PRE
plot(DTMpre_hill_crop,
     col=grey(1:100/100),  # create a color ramp of grey colors
     legend=FALSE,
     main="Four Mile Canyon Creek, Boulder County\nPre-Flood",
     axes=FALSE)
# note \n in the title forces a line break in the title
plot(DTM_pre_crop, 
     axes=FALSE,
     alpha=0.5,
     add=T)

# POST
# plot Post-flood w/ hillshade
plot(DTMpost_hill_crop,
     col=grey(1:100/100),  # create a color ramp of grey colors
     legend=FALSE,
     main="Four Mile Canyon Creek, Boulder County\nPost-Flood",
     axes=FALSE)

plot(DTM_post_crop, 
     axes=FALSE,
     alpha=0.5,
     add=T)

# CHANGE - DoD
plot(DTMpost_hill_crop,
     col=grey(1:100/100),  # create a color ramp of grey colors
     legend=FALSE,
     main="Elevation Change Post Flood\nFour Mile Canyon Creek, Boulder County",
     axes=FALSE)

plot(DoD_crop,
     breaks = c(-5,-1,-0.5,0.5,1,10),
     col= difCol5,
     axes=FALSE,
     alpha=0.4,
     add =T)


# Create an example data.frame
set.seed(65.7)
examp_df <- data.frame(x = runif(10, min = -73, max = -71), y = runif(10, min = 41, 
                                                                      max = 45))
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Create and example data.frame with additional columns
cats <- data.frame(category = c("H", "H", "L", "L", "L", "M", "H", "L", "M", 
                                "M"))

examp_df2 <- data.frame(examp_df, cats)

# Create an example SpatialPoints
examp_sp <- SpatialPoints(examp_df, proj4string = CRS(prj_dd))

# Create an example SpatialPointsDataFrame
examp_spdf <- SpatialPointsDataFrame(examp_sp, proj4string = CRS(prj_dd), data = cats)

# Example using data.frame with longitude and latitude
df_elev <- get_elev_point(examp_df, prj = prj_dd, src = "mapzen")

library(elevatr)
df_elev_epqs <- get_elev_point(examp_df, prj = prj_dd, src = "epqs")
data.frame(df_elev_epqs)
data(lake)

elevation <- get_elev_raster(lake, z = 9)
plot(elevation)
plot(lake, add = TRUE)