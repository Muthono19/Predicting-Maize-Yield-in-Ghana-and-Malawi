#####
#### randomforest model using ranger in Caret package
# Created by Stella Gachoki

### Load the relevant libraries ####
library(caret)
library(ranger)
library(tidyverse)
library(e1071)
library(CAST)
library(randomForest)
library(VSURF)


### Set the working directory###
setwd("C:\\Users\\sgachoki\\Desktop\\IITA")

### Load the database
d1 <- read.csv("dBaseGhana2013_out.csv")
d2 <- read.csv("dBaseGhana2019_out.csv")
y1<- d1$Yield
y2 <- d2$Yield


### Make the relevant columns categorical
### Make the relevant columns categorical
cols <- c("Clusters","Intercrop","Manure","Labour","Pest","Residue","Rotation","Credit",
          "Fertilizer","Fallow","Femhead","Minpltdis")
d2[cols] <- lapply(d2[cols],factor)
d1[cols] <- lapply(d1[cols],factor)

names(d2)

### Names of the predictors for each dataset 
p1_cat <- c("Lat","Long", "Intercrop","Manure",   "Labour",   "Pest",
            "Residue",  "Rotation", "Credit",   "Fertilizer"  , "Fallow",   "Femhead",  "Minpltdis",  "aprEVI",  
            "mayEVI",   "junEVI",   "julEVI",   "augEVI",   "sepEVI",   "octEVI",   "seasEVI",  "aprprec", 
            "mayprec",  "junprec",  "julprec",  "augprec",  "sepprec",  "octprec",  "seasprec", "pdsi",
            "lst",  "Maxtemp",  "Mintemp",  "Soilmoist","Silt", "Sand", "Clay", "CEC", 
            "Orgcab",   "Soilbulk", "DEM",  "MarketAccess" ,"LivDens" )

p1_all <- c( "Lat","Long","Manure",   "Labour",
             "Residue",  "Rotation", "Credit", "Fallow",   "Femhead",  "Minpltdis","Hhsize",  
             "Headage",  "Headedu",  "Meanedu",  "Landsize", "Nplots",   "IntercropH"   ,"FempltsF", "TLU",
             "CropD","LvstD","Ntreat",   "FertHa",   "Ncrop","Totincome","PestHa",   "aprEVI",  
             "mayEVI",   "junEVI",   "julEVI",   "augEVI",   "sepEVI",   "octEVI",   "seasEVI",  "aprprec", 
             "mayprec",  "junprec",  "julprec",  "augprec",  "sepprec",  "octprec",  "seasprec", "pdsi",
             "lst",  "Maxtemp",  "Mintemp",  "Soilmoist","Silt", "Sand", "Clay", "CEC", 
             "Orgcab",   "Soilbulk", "DEM",  "MarketAccess" ,"LivDens" )

p2_cat <- c( "Lat","Long", "Intercrop","Manure",   "Labour",   "Pest",
             "Residue",  "Rotation", "Credit",   "Fertilizer"  , "Fallow",   "Femhead",  "Minpltdis",  "aprEVI",  
             "mayEVI",   "junEVI",   "julEVI",   "augEVI",   "sepEVI",   "octEVI",   "seasEVI",  "aprprec", 
             "mayprec",  "junprec",  "julprec",  "augprec",  "sepprec",  "octprec",  "seasprec", "pdsi",
             "lst",  "Maxtemp",  "Mintemp",  "Soilmoist","Silt", "Sand", "Clay", "CEC", 
             "Orgcab",   "Soilbulk", "DEM",  "MarketAccess" ,"LivDens")

p2_all <- c( "Lat","Long","Manure",   "Labour",
             "Residue",  "Rotation", "Credit", "Fallow",   "Femhead",  "Minpltdis","Hhsize",  
             "Headage",  "Headedu",  "Meanedu",  "Landsize", "Nplots",   "IntercropH"   ,"FempltsF", "TLU",
             "CropD","LvstD","Ntreat",   "FertHa",   "Ncrop","Totincome","PestHa",   "aprEVI",  
             "mayEVI",   "junEVI",   "julEVI",   "augEVI",   "sepEVI",   "octEVI",   "seasEVI",  "aprprec", 
             "mayprec",  "junprec",  "julprec",  "augprec",  "sepprec",  "octprec",  "seasprec", "pdsi",
             "lst",  "Maxtemp",  "Mintemp",  "Soilmoist","Silt", "Sand", "Clay", "CEC", 
             "Orgcab",   "Soilbulk", "DEM",  "MarketAccess" ,"LivDens"  )


vsurf.d1_cat <- VSURF_thres(d1[,which(names(d1)%in%p1_cat)],y1,ntree=2000, mtry = 10,nfor.thres = 50,
                      nmin = 1,RFimplem = "ranger")
vsurf.d1_cat$varselect.thres


vsurf.d2_cat <- VSURF_thres(d2[,which(names(d2)%in%p2_cat)],y2,ntree=2000, mtry = 10,nfor.thres = 50,
                      nmin = 1,RFimplem = "ranger")
vsurf.d2_cat$varselect.thres

vsurf.d1_all <- VSURF_thres(d1[,which(names(d1)%in%p1_all)],y1,ntree=2000, mtry = 10,nfor.thres = 50,
                            nmin = 1,RFimplem = "ranger")
vsurf.d1_all$varselect.thres

vsurf.d2_all <- VSURF_thres(d2[,which(names(d2)%in%p2_all)],y2,ntree=2000, mtry = 10,nfor.thres = 50,
                            nmin = 1,RFimplem = "ranger")
vsurf.d2_all$varselect.thres

dat <- d2[,which(names(d2)%in%p2_all)]
var1 <- dat[,c(54,2,46,39,45,1,25,37,47,31,43,36,40,48,41,56,23,28,34,52,26,32,35,53,42,33,55,50,38,30,49,29,27,44,51,19,15,14,24,22,11,21,10
               ,20,3,7,5,17,16,6,12)]
names(var1)

##### THE END #####
