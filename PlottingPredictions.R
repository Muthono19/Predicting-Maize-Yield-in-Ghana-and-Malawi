## Load the relevant libraries
library(raster)
library(rasterVis)
library(sf)
library(rgdal)
library(RColorBrewer)
library(lattice)
library(plotly)
library(maptools)
library(sp)

### set the working directory
setwd("C:/Users/sgachoki/Desktop/IITA/Predictions")

### Load the outer boundaries
malawi <- readOGR("mwi_admbnda_adm1_nso_20181016.shp")
srmalawi = "+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs"
malawi.prj = spTransform(malawi,CRSobj = srmalawi)

ghana <- readOGR("NorthGhanaPRJ.shp")
plot(ghana)

## Load the predicted rasters
notreat.g13 <- stack("NoTreatGhana2013.tif","FertGhana2013.tif","FertPestGhana2013.tif") / 1000
ghananames13 <- c("","","")

notreat.g19 <- stack("NoTreatGhana2019.tif","FertGhana2019.tif","FertPestGhana2019.tif") /1000
ghananames19 <- c("","","")

myTheme=rasterTheme(region=brewer.pal('RdYlGn', n=11))

png(file = "predictionsGhana19.png", width = 4000, height = 1550, units = "px", res = 500, type = "cairo")
levelplot(notreat.g19,par.settings=myTheme, names.attr=ghananames19, layout=c(3,1), margin=FALSE,ylab="2019",
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ghana, fill = NA, col = "black", lwd=0.3)
          })
dev.off()



notreat.m13 <- stack("NoTreatMalawi2013.tif","FertMalawi2013.tif","FertPestMalawi2013.tif")/1000
namesmalawi13 <- c("","","")
notreat.m19 <- stack("NoTreatMalawi2019.tif","FertMalawi2019.tif","FertPestMalawi2019.tif") /1000
namesmalawi19 <- c("","","")

png(file = "predictionsMalawi19.png", width = 3500, height = 2500, units = "px", res = 500, type = "cairo")
levelplot(notreat.m19,par.settings=myTheme, names.attr=namesmalawi19, layout=c(3,1), margin=FALSE,ylab="2019",
          scales=list(x=list(rot=30, cex=0.8)),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(malawi.prj, fill = NA, col = "black", lwd=0.3)
          })
dev.off()

?levelplot

