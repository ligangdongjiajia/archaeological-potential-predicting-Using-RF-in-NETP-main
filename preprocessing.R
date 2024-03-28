
setwd("D:/R/NETP-modeling")
library(sf)
library(raster)
library(sp)
library(ggplot2)
#################################################################### read independent data
random_point <- st_read("culture.point/random_point_pj.shp")
study_area <- st_read("study_area/study_area.shp")
study_area <- st_transform(study_area, crs = crs(random_point)) #Transforming coordinate

cultural_relics <- read.csv("culture.point/cultural_relics.csv")
dated_sites <- read.csv("culture.point/dated_sites.csv")

cultural_relics1 <- st_as_sf(cultural_relics, coords = c("Longitude", "Latitude"), crs = 4326)
cultural_relics1 <- st_transform(cultural_relics1, crs = crs(random_point))

dated_sites1 <- st_as_sf(dated_sites, coords = c("Longitude.E.", "Latitude.N."), crs = 4326)
dated_sites1 <- st_transform(dated_sites1, crs = crs(random_point))

#################################################################### data merging

colnames(random_point)[1:3] <- c("site_name", "culture", "geometry")
colnames(cultural_relics1)[c(1,6,10)] <- c("site_name", "culture", "geometry")
colnames(dated_sites1)[c(2,4,11)] <- c("site_name", "culture", "geometry")

all_point <- rbind(random_point[,1:3], cultural_relics1[,c(1,6,10)], dated_sites1[,c(2,4,11)])

all_point1 <- st_intersection(study_area, all_point) #point data clipping
all_point1[,1:15]<- NULL

ggplot() + 
  geom_sf(data= all_point1, color = "green", size=1)+
  geom_sf(data = study_area, fill = "grey", color = "black", alpha = 0.5)

#################################################################### read dependent data and data extraction

DEM <- raster("variables/TPDEM_prj")  ## source: https://www.gscloud.cn/
all_point1 <- st_transform (all_point1, crs = crs(DEM))
all_point1$elevation<- extract(DEM,all_point1)

slope <- terrain(DEM, opt = "slope", unit = "degrees")
all_point1$slope<- extract(slope, all_point1)

aspect <- terrain(DEM, opt = "aspect", unit = "degrees")
all_point1$aspect<- extract(aspect, all_point1)

fluctuation <- raster("variables/fluctuation")
all_point1$fluctuation<- extract(fluctuation, all_point1)

curvature <- terrain(slope, opt = "slope", unit = "degrees")
all_point1$curvature<- extract(curvature,all_point1)

permanent <- raster("variables/hydrology/permanent")  ## source: https://data.tpdc.ac.cn
all_point1$permanent <- extract(permanent,all_point1)

intermitte <- raster("variables/hydrology/intermitte")  ## source: https://data.tpdc.ac.cn
all_point1$intermitte <- extract(intermitte,all_point1)

lake <- raster("variables/hydrology/lake")  ## source: https://data.tpdc.ac.cn
all_point1$lake <- extract(lake,all_point1)

all_point1[, 9:11] <- lapply(all_point1[, 9:11], function(x) ifelse(is.na(x), 20, x))

NDVI <- raster("variables/Vegetation/NDVI/NDVI.tif")  ## source: https://www.resdc.cn/
all_point1 <- st_transform (all_point1, crs = crs(NDVI ))  
all_point1$NDVI <- extract(NDVI,all_point1)

veg_type <- st_read("variables/Vegetation/type/veg.shp")  ## source: https://www.resdc.cn/

veg_type <- veg_type[, "VEGETATI_4", drop = FALSE]
veg_type <- st_transform (veg_type, crs = crs(DEM))
colnames(veg_type)[1]<-"veg_type"

all_point1 <- st_transform (all_point1, crs = crs(veg_type))
all_point1 <- st_intersection(veg_type, all_point1)
unique(all_point1$veg_type)

all_point1$veg_type <- sub("deserts", "Deserts", all_point1$veg_type) 
all_point1$veg_type <- sub("steppes", "Steppes", all_point1$veg_type)
all_point1$veg_type <- sub("meadows", "Meadows", all_point1$veg_type)

soil_type <- raster("variables/soil/soil_type")   ## source: https://www.resdc.cn/
all_point1 <- st_transform (all_point1, crs = crs(soil_type))
all_point1$soil_type <- extract(soil_type, all_point1)

soil_erosi <- raster("variables/soil/soil_erosion")   ## source: https://www.resdc.cn/
all_point1 <- st_transform (all_point1, crs = crs(soil_erosi))
all_point1$soil_erosi <- extract(soil_erosi, all_point1)

grassland <- raster("variables/land suitability/pastural/pastural.tif")  ## source: https://www.resdc.cn/
all_point1 <- st_transform (all_point1, crs = crs(grassland))
all_point1$grassland <- extract(grassland, all_point1)

cultivated <- raster("variables/land suitability/cultivated/cultivated.tif")  ## source: https://www.resdc.cn/
all_point1 <- st_transform (all_point1, crs = crs(cultivated))
all_point1$cultivated <- extract(cultivated, all_point1)

temperature <- raster("variables/climate/MAT.tif")    ## source: https://www.resdc.cn/
all_point1 <- st_transform (all_point1, crs = crs(temperature))
all_point1$temperature <- extract(temperature, all_point1)

precipitation <- raster("variables/climate/MAP.tif")  ## source: https://www.resdc.cn/
all_point1 <- st_transform (all_point1, crs = crs(precipitation))
all_point1$precipitation <- extract(precipitation, all_point1)

#write.csv(all_point1,"all_point.csv")

################################################################################
#The raster data was transformed into points with a resolution of 1*1km, where each raster patch was replaced by a point.
#Geographic variables were extracted, and a model was employed to predict the archaeological probabilities for each patch (point).

DEM_1000 <- resample(DEM, res = 1000)
res(DEM)

DEM_1000 <- aggregate(DEM, fact = 1000/res(DEM), fun = mean)
res(DEM_1000)
plot(DEM_1000)
points <- rasterToPoints(DEM_1000,crs = crs(DEM_1000))
points <- as.data.frame(points)
colnames(points)[3] <- "elevation"

points_sf <- st_as_sf(points, coords = c("x", "y"), crs =  crs(DEM_1000))
crs(points_sf)

slope_1000 <- aggregate(slope, fact = 1000/res(slope), fun = mean)
points_sf$slope<- extract(slope_1000, points_sf)

aspect_1000 <- aggregate(aspect, fact = 1000/res(aspect), fun = mean)
points_sf$aspect<- extract(aspect_1000, points_sf)

curvature_1000 <- aggregate(curvature, fact = 1000/res(curvature), fun = mean)
points_sf$curvature<- extract(curvature_1000, points_sf)

points_sf$permanent<- extract(permanent, points_sf)
points_sf$intermitte<- extract(intermitte, points_sf)
points_sf$lake<- extract(lake, points_sf)

points_sf[, 6:8] <- lapply(points_sf[, 6:8], function(x) ifelse(is.na(x), 20, x))

points_sf <- st_transform (points_sf, crs = crs(NDVI))
points_sf$NDVI <- extract(NDVI,points_sf)

points_sf<- st_transform (points_sf, crs = crs(veg_type))
points_sf <- st_intersection(veg_type,points_sf)

unique(points_sf$veg_type)

points_sf$veg_type <- sub("deserts", "Deserts", points_sf$veg_type) 
points_sf$veg_type <- sub("steppes", "Steppes", points_sf$veg_type)
points_sf$veg_type <- sub("meadows", "Meadows", points_sf$veg_type)
points_sf$veg_type <- sub("mesdows", "Meadows", points_sf$veg_type)

points_sf <- st_transform (points_sf, crs = crs(soil_type))
points_sf$soil_type <- extract(soil_type, points_sf)

points_sf <- st_transform (points_sf, crs = crs(soil_erosi))
points_sf$soil_erosi <- extract(soil_erosi, points_sf)

points_sf <- st_transform (points_sf, crs = crs(grassland))
points_sf$grassland <- extract(grassland, points_sf)

points_sf <- st_transform (points_sf, crs = crs(cultivated))
points_sf$cultivated <- extract(cultivated, points_sf)

points_sf <- st_transform (points_sf, crs = crs(precipitation))
points_sf$precipitation <- extract(precipitation, points_sf)

points_sf <- st_transform (points_sf, crs = crs(DEM_1000))

st_write(points_sf, "points.shp")
