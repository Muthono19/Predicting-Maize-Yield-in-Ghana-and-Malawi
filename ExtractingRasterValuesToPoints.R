library(raster)
library(rasterVis)
library(rgdal)
library(stars)

setwd("~/NG DATA/Season 2013")

###Read the household survey data
HH <- readOGR("HH_Ghana2013_New.shp")
sr="+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs "
hh.prj <- spTransform(HH,crs("+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs "))

### stack all the variables together
rasstack <- stack("Clusters_NG.tif","aprEVI.tif","mayEVI.tif","junEVI.tif","julEVI.tif","augEVI.tif","sepEVI.tif","octEVI.tif","seasEVI.tif",
                  "aprprec.tif","mayprec.tif","junprec.tif","julprec.tif","augprec.tif","sepprec.tif","octprec.tif","seasprec.tif",
                  "pdsi.tif","lst.tif","Maxtemp.tif","Mintemp.tif","Soilmoist.tif","Silt.tif","Sand.tif","Clay.tif","CEC.tif","Orgcab.tif",
                  "Soilbulk.tif","DEM.tif","MarketAccess.tif","LivDens.tif")

names(rasstack) <- c("Clusters","aprEVI","mayEVI","junEVI","julEVI","augEVI","sepEVI","octEVI","seasEVI",
                     "aprprec","mayprec","junprec","julprec","augprec","sepprec","octprec","seasprec",
                     "pdsi","lst","Maxtemp","Mintemp","Soilmoist","Silt","Sand","Clay","CEC","Orgcab",
                     "Soilbulk","DEM","MarketAccess","LivDens")

names(rasstack)

##### Extract values to point
rastervalues.var <- extract(rasstack,hh.prj, method="bilinear")
rastervalues.combine <- cbind(HH,rastervalues.var)
write.table(rastervalues.combine,file="dBaseGhana2013_New.csv",append=FALSE, sep=",",row.names = FALSE, col.names = TRUE)
