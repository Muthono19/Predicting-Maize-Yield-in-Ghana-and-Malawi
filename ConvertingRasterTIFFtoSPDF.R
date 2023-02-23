library(raster)
library(gdalUtilities)
library(grid)
library(rgdal)
library(sf)
library(terra)

### Set the working directory###
setwd("~/NG DATA/Season 2013")

### Load AOI (Only clusters with household locations)
ghana <- readOGR("North_Ghana_Cluster.shp")
malawi <- readOGR("Malawi_Cluster.shp")

### Define names of retained predictors for caret modeling
p1_cat <- c("Yield","Lat","Long","Silt","octprec","LivDens","DEM","julprec","pdsi","junEVI","augprec",
            "Orgcab","aprEVI","Soilmoist","mayEVI","Soilbulk","augEVI","lst","Clay","seasEVI",
            "Mintemp","seasprec","CEC","sepEVI","julEVI","Sand","sepprec","mayprec","aprprec",
            "Fert","octEVI","junprec","Maxtemp","MarketAccess","Minpltdis","Pest","Manure","Labour","Intercrop")

p2_cat <- c("DEM","Long","Mintemp","augprec","Lat","Maxtemp","augEVI","junprec",
            "LivDens","Silt","Soilmoist","seasEVI","mayEVI","Orgcab","mayprec","pdsi",
            "octEVI","sepEVI","sepprec","Clay","octprec","Sand","julEVI","junEVI",
            "Soilbulk","seasprec","CEC","aprEVI","aprprec","lst","julprec","MarketAccess",
            "Minpltdis","Credit","Rotation","Manure","Residue","Pest","Fertilizer","Labour")

p3_cat <- c("CEC","DEM","Long","LivDens","Soilmoist","novprec","Orgcab","Sand",
            "Clay","Lat","Soilbulk","aprprec","decEVI","aprEVI","Fertilizer","janEVI",
            "octEVI","lst","seasEVI","novEVI","janprec","Silt","pdsi","febEVI",
            "febprec","marEVI","Mintemp","MarketAccess","decprec","seasprec","Labour","Maxtemp",
            "marprec","octprec","Residue","Intercrop","Pest","Mindis","Manure")

p4_cat <- c("LivDens","DEM","Labour","Silt","Lat","Clay","CEC","Long",
            "Sand","Orgcab","Fertilizer","marEVI","aprEVI","novprec","novEVI","janEVI",
            "decEVI","lst","seasEVI","febEVI","Soilmoist","Maxtemp","febprec","Mintemp",
            "octEVI","Soilbulk","decprec","aprprec","Femhead","seasprec","marprec","MarketAccess",
            "janprec","Pest","octprec","Rotation","pdsi","Erosion","Intercrop","Residue",
            "Mindis","Fallow")

#### Stack the raster and convert to grid
ras_cat1 <- stack("DEM.tif","Long.tif","Mintemp.tif","augprec.tif","Lat.tif","Maxtemp.tif","augEVI.tif","junprec.tif",
                 "LivDens.tif","Silt.tif","Soilmoist.tif","seasEVI.tif","mayEVI.tif","Orgcab.tif","mayprec.tif","pdsi.tif",
                 "octEVI.tif","sepEVI.tif","sepprec.tif","Clay.tif","octprec.tif","Sand.tif","julEVI.tif","junEVI.tif",
                 "Soilbulk.tif","seasprec.tif","CEC.tif","aprEVI.tif","aprprec.tif","lst.tif","julprec.tif","MarketAccess.tif")

ras_cat2 <- stack("Lat.tif","Long.tif","Silt.tif","octprec.tif","LivDens.tif","DEM.tif","julprec.tif","pdsi.tif","junEVI.tif","augprec.tif",
                 "Orgcab.tif","aprEVI.tif","Soilmoist.tif","mayEVI.tif","Soilbulk.tif","augEVI.tif","lst.tif","Clay.tif","seasEVI.tif",
                 "Mintemp.tif","seasprec.tif","CEC.tif","sepEVI.tif","julEVI.tif","Sand.tif","sepprec.tif","mayprec.tif","aprprec.tif",
                 "octEVI.tif","junprec.tif","Maxtemp.tif","MarketAccess.tif")

ras_cat3 <- stack("CEC.tif","DEM.tif","Long.tif","LivDens.tif","Soilmoist.tif","novprec.tif","Orgcab.tif","Sand.tif",
                 "Clay.tif","Lat.tif","Soilbulk.tif","aprprec.tif","decEVI.tif","aprEVI.tif","janEVI.tif",
                 "octEVI.tif","lst.tif","seasEVI.tif","novEVI.tif","janprec.tif","Silt.tif","pdsi.tif","febEVI.tif",
                 "febprec.tif","marEVI.tif","Mintemp.tif","MarketAccess.tif","decprec.tif","seasprec.tif","Maxtemp.tif",
                 "marprec.tif","octprec.tif")


ras_cat4 <- stack("LivDens.tif","DEM.tif","Silt.tif","Lat.tif","Clay.tif","CEC.tif","Long.tif",
                 "Sand.tif","Orgcab.tif","marEVI.tif","aprEVI.tif","novprec.tif","novEVI.tif","janEVI.tif",
                 "decEVI.tif","lst.tif","seasEVI.tif","febEVI.tif","Soilmoist.tif","Maxtemp.tif","febprec.tif","Mintemp.tif",
                 "octEVI.tif","Soilbulk.tif","decprec.tif","aprprec.tif","seasprec.tif","marprec.tif","MarketAccess.tif",
                 "janprec.tif","octprec.tif","pdsi.tif")

#### crop the raster stack and save
rast_cat1.prj <- mask(ras_cat1,ghana)
writeRaster(rast_cat1.prj,"Masked_ras_stackGhana2013.tif")

rast_cat2.prj <- mask(ras_cat2,ghana)
writeRaster(rast_cat2.prj,"Masked_ras_stackGhana2019.tif")

rast_cat3.prj <- mask(ras_cat3,malawi)
writeRaster(rast_cat3.prj,"Masked_ras_stackMalawi2013.tif")

rast_cat4.prj <- mask(ras_cat4,malawi)
writeRaster(rast_cat4.prj,"Masked_ras_stackMalawi2019.tif")

#### Convert the stacked raster to spatial dataframe and save as RDS files
ras_cat1.grid = as(rast_cat1.prj, "SpatialPixelsDataFrame")
write_rds(ras_cat1.grid, "ras_cat_gridGhana2013.rds")

ras_cat2.grid = as(rast_cat2.prj, "SpatialPixelsDataFrame")
write_rds(ras_cat2.grid, "ras_cat_gridGhana2019.rds")

ras_cat3.grid = as(rast_cat3.prj, "SpatialPixelsDataFrame")
write_rds(ras_cat3.grid, "ras_cat_gridMalawi2013.rds")

ras_cat4.grid = as(rast_cat4.prj, "SpatialPixelsDataFrame")
write_rds(ras_cat4.grid, "ras_cat_gridMalawi2019.rds")


###### THE END #####











