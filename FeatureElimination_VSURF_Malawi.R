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
d3 <- read.csv("dBaseMalawi2013_out.csv")
d4 <- read.csv("dBaseMalawi2019_out.csv")
y3<- d3$Yield
y4 <- d4$Yield


### Make the relevant columns categorical
### Make the relevant columns categorical
cols <- c("Clusters", "Intercrop","Fertilizer"  , "Manure",   "Labour",  
          "Pest", "Residue",  "Rotation", "Fallow",   "Femhead",  "Mindis",   "Erosion")
d3[cols] <- lapply(d3[cols],factor)
d4[cols] <- lapply(d4[cols],factor)

names(d4)

### Names of the predictors for each dataset 
p3_cat <- c( "Long",    "Lat","Intercrop"  ,  "Fertilizer" ,  "Manure",  "Labour", 
             "Pest",    "Residue", "Rotation","Fallow",  "Femhead", "Mindis",  "Erosion",  "octEVI",  "novEVI",  "decEVI", 
             "janEVI",  "febEVI",  "marEVI",  "aprEVI",  "seasEVI", "octprec", "novprec", "decprec",
             "janprec", "febprec", "marprec", "aprprec", "seasprec","pdsi",    "lst","Maxtemp",
             "Mintemp", "Soilmoist"  ,  "Silt",    "Sand",    "Clay",    "CEC","Orgcab",  "Soilbulk" ,   
             "DEM","MarketAccess" ,"LivDens" )

p3_all <- c( "Long",    "Lat","Intercrop"  ,  "Manure",  "Labour", 
             "Pest",    "Residue", "Rotation","Fallow",  "Femhead", "Mindis",  "Erosion", "Hhsize", 
             "Headage", "Meanedu", "Educmax", "Landsize","Nplots",  "FempltsF","TLU","CropD",  
             "LvstD",   "Ncrop",   "FertHa",  "Totincome"  ,  "MaizeA",  "octEVI",  "novEVI",  "decEVI", 
             "janEVI",  "febEVI",  "marEVI",  "aprEVI",  "seasEVI", "octprec", "novprec", "decprec",
             "janprec", "febprec", "marprec", "aprprec", "seasprec","pdsi",    "lst","Maxtemp",
             "Mintemp", "Soilmoist"   , "Silt",    "Sand",    "Clay",    "CEC","Orgcab",  "Soilbulk" ,   
             "DEM","MarketAccess" ,"LivDens")

p4_cat <- c( "Long",    "Lat","Intercrop"    ,"Fertilizer"  , "Manure",  "Labour", 
             "Pest",    "Residue", "Rotation","Fallow",  "Femhead", "Mindis",  "Erosion","octEVI",  "novEVI", 
             "decEVI",  "janEVI",  "febEVI",  "marEVI",  "aprEVI",  "seasEVI", "octprec", "novprec",
             "decprec", "janprec", "febprec", "marprec", "aprprec", "seasprec","pdsi",    "lst",    
             "Maxtemp", "Mintemp", "Soilmoist"    ,"Silt",    "Sand",    "Clay",    "CEC","Orgcab", 
             "Soilbulk","DEM","MarketAccess" ,"LivDens")

p4_all <- c( "Long",    "Lat","Intercrop"  , "Manure",  "Labour", 
             "Pest",    "Residue", "Rotation","Fallow",  "Femhead", "Mindis",  "Erosion", "Hhsize", 
             "Headage", "Meanedu", "Educmax", "Landsize","Nplots",  "FempltsF","TLU","CropD",  
             "LvstD",   "Ntreat",  "Ncrop",   "FertHa",  "Totincome"   , "MaizeA",  "octEVI",  "novEVI", 
             "decEVI",  "janEVI",  "febEVI",  "marEVI",  "aprEVI",  "seasEVI", "octprec", "novprec",
             "decprec", "janprec", "febprec", "marprec", "aprprec", "seasprec","pdsi",    "lst",    
             "Maxtemp", "Mintemp", "Soilmoist"   , "Silt",    "Sand",    "Clay",    "CEC","Orgcab", 
             "Soilbulk","DEM","MarketAccess" ,"LivDens"  )


vsurf.d3_cat <- VSURF_thres(d3[,which(names(d3)%in%p3_cat)],y3,ntree=2000, mtry = 10,nfor.thres = 50,
                      nmin = 1,RFimplem = "ranger")
vsurf.d3_cat$varselect.thres


vsurf.d4_cat <- VSURF_thres(d4[,which(names(d4)%in%p4_cat)],y4,ntree=2000, mtry = 10,nfor.thres = 50,
                      nmin = 1,RFimplem = "ranger")
vsurf.d4_cat$varselect.thres

vsurf.d3_all <- VSURF_thres(d3[,which(names(d3)%in%p3_all)],y3,ntree=2000, mtry = 10,nfor.thres = 50,
                            nmin = 1,RFimplem = "ranger")
vsurf.d3_all$varselect.thres

vsurf.d4_all <- VSURF_thres(d4[,which(names(d4)%in%p4_all)],y4,ntree=2000, mtry = 10,nfor.thres = 50,
                            nmin = 1,RFimplem = "ranger")
vsurf.d4_all$varselect.thres

dat <- d4[,which(names(d4)%in%p4_all)]
var1 <- dat[,c(25,26,27,55,57,5,2,17,51,1,50,53,37,20,22,49,33,48,34,46,47,52,45,40,24,29,21,15,35,31,30,42,38,32,43,28,41,16,23,18,14,39,36
               ,19,10,56,9,6,44,3,54,11,8)]
names(var1)

##### THE END #####
