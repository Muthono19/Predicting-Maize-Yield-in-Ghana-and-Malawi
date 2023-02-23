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
library(CAST)
library(rgdal)

### set the working directory
setwd("~/Gridded data")

### Load the databases without outliers
d1 <- read.csv("dBaseGhana2013_Out.csv")
d2 <- read.csv("dBaseGhana2019_Out.csv")
d3 <- read.csv("dBaseMalawi2013_Out.csv")
d4 <- read.csv("dBaseMalawi2019_Out.csv")

### convert relevant variables to categorical # Important for model including all predictors
cols.g <- c("Clusters","Intercrop","Manure","Labour","Pest","Residue","Rotation","Credit",
            "Fert","Fallow","Femhead","Minpltdis")

cols.m <- c("Clusters", "Intercrop","Fert"  , "Manure",   "Labour",  
            "Pest", "Residue",  "Rotation", "Fallow",   "Femhead",  "Mindis",   "Erosion" )

d1[cols.g] <- lapply(d1[cols.g],factor)
d2[cols.g] <- lapply(d2[cols.g],factor)

d3[cols.m] <- lapply(d3[cols.m],factor)
d4[cols.m] <- lapply(d4[cols.m],factor)

### indicate the predictor names retained for prediction using all variables # for categorical variables these are loaded in the grids
p1_all <- c("Lat","FertHa", "octprec","Totincome","Long", "Silt", "julprec","augprec", "LivDens","DEM","pdsi", "Soilmoist","mayEVI", "seasprec", "junEVI", "Soilbulk",
            "aprEVI", "Orgcab", "TLU","seasEVI","lst","sepprec","mayprec","Mintemp", "CEC","PestHa", "Sand", "Clay", "augEVI", "Landsize" ,"sepEVI", "julEVI",
            "aprprec","Maxtemp","junprec","octEVI", "MarketAccess", "Headage","Ncrop","Hhsize","CropD","Minpltdis","Nplots", "Meanedu","Headedu","LvstD","Manure", "Labour","Ntreat")

p2_all <- c("DEM","Long","Mintemp","augprec","Maxtemp","Lat","Totincome","junprec","Soilmoist","augEVI","pdsi","mayprec","sepprec","Silt","octprec","LivDens",
            "FertHa","mayEVI","seasEVI","Orgcab","PestHa","sepEVI","aprprec","Soilbulk","seasprec","octEVI","MarketAccess","Clay","julprec","julEVI","Sand","junEVI",
            "aprEVI","lst","CEC","TLU","Landsize","Meanedu","Ncrop","Ntreat","Hhsize","LvstD","Minpltdis","CropD","Manure","Credit","Residue","IntercropH","Nplots","Rotation","Headage")

p3_all <- c("FertHa","MaizeA","CEC","Soilmoist","DEM","Long","Ncrop","novprec","LivDens","Sand","Orgcab","CropD","Landsize","Clay","Lat","Totincome","TLU","Soilbulk",
            "LvstD","aprprec","janprec","Mintemp","janEVI","Meanedu","marEVI","decEVI","lst","MarketAccess","octEVI","Nplots","Silt","aprEVI","febEVI","seasEVI","Educmax","pdsi",
            "novEVI","Maxtemp","febprec","marprec","decprec","seasprec","octprec","Labour","Headage","Hhsize","Manure","Intercrop","Residue","Pest","Rotation","Mindis")

p4_all <- c("FertHa","Totincome","MaizeA","DEM","LivDens","Labour","Lat","Landsize","Clay","Long","Sand","Orgcab","novprec","TLU","LvstD","Silt", "marEVI","Soilmoist",
            "aprEVI","Maxtemp","Mintemp","CEC","lst","febprec","Ncrop","novEVI","CropD","Meanedu","seasEVI","janEVI","decEVI","aprprec","decprec","febEVI","seasprec","octEVI",
            "marprec","Educmax","Ntreat","Nplots", "Headage","janprec","octprec","FempltsF","Femhead","MarketAccess","Fallow","Pest","pdsi","Intercrop","Soilbulk","Mindis","Rotation")

### Load the SPDF grids to be used for models with only categorical data; noted are categorical variables to be appended
### Append all the categorical variables
grid.d1 <- readRDS("ras_cat_gridGhana2013.rds") ###"Pest","Manure","Labour","Intercrop", "Fertilizer"
pest1.name <- c("Pest")
manure1.name <- c("Manure")
labour1.name <- c("Labour")
intercrop1.name <- c("Intercrop")
fert1.name <- c("Fert")
for(i in c(paste(pest1.name),paste(manure1.name),paste(labour1.name),paste(intercrop1.name),paste(fert1.name))){
  grid.d1@data[,i] = 0
}

for(i in names(grid.d1)){
  if(sum(is.na(grid.d1@data[,i]))>0){
    grid.d1@data[,i] = ifelse(is.na(grid.d1@data[,i]), quantile(grid.d1@data[,i], 0.5, na.rm=TRUE), grid.d1@data[,i])
  }
}
names(grid.d1)

grid.d2 <- readRDS("ras_cat_gridGhana2019.rds") ###"Credit","Rotation","Manure","Residue","Pest","Fertilizer","Labour"
credit2.name <- c("Credit")
rotation2.name <- c("Rotation")
manure2.name <- c("Manure")
residue2.name <- c("Residue")
pest2.name <- c("Pest")
fert2.name <- c("Fert")
labour2.name <- c("Labour")
for(i in c(paste(credit2.name),paste(rotation2.name),paste(manure2.name),paste(residue2.name),paste(pest2.name),paste(fert2.name),paste(labour2.name))){
  grid.d2@data[,i] = 0
}

for(i in names(grid.d2)){
  if(sum(is.na(grid.d2@data[,i]))>0){
    grid.d2@data[,i] = ifelse(is.na(grid.d2@data[,i]), quantile(grid.d2@data[,i], 0.5, na.rm=TRUE), grid.d2@data[,i])
  }
}
names(grid.d2)

grid.d3 <- readRDS("ras_cat_gridmalawi2013.rds") ###"Residue","Intercrop","Pest","Mindis","Manure", "Labour", "Fertilizer" 
residue3.name <-("Residue")
intercrop3.name <-("Intercrop")
pest3.name <- c("Pest")
mindis3.name <- c("Mindis")
manure3.name <- c("Manure")
labour3.name <- c("Labour")
fert3.name <- c("Fert")
for(i in c(paste(residue3.name),paste(intercrop3.name),paste(pest3.name),paste(mindis3.name),paste(manure3.name),paste(labour3.name),paste(fert3.name))){
  grid.d3@data[,i] = 0
}

for(i in names(grid.d3)){
  if(sum(is.na(grid.d3@data[,i]))>0){
    grid.d3@data[,i] = ifelse(is.na(grid.d3@data[,i]), quantile(grid.d3@data[,i], 0.5, na.rm=TRUE), grid.d3@data[,i])
  }
}
names(grid.d3)


grid.d4 <- readRDS("ras_cat_gridmalawi2019.rds") ###"Pest","Rotation","Erosion","Intercrop","Residue","Mindis","Fallow", "Femhead", "Fertilizer"
pest4.name <- c("Pest")
rotation4.name <- c("Rotation")
erosion4.name <- c("Erosion")
intercrop4.name <- c("Intercrop")
residue4.name <- c("Residue")
mindis4.name <- c("Mindis")
fallow4.name <- c("Fallow")
femhead4.name <- c("Femhead")
fert4.name <- c("Fert")
for(i in c(paste(pest4.name),paste(rotation4.name),paste(erosion4.name),paste(intercrop4.name),paste(residue4.name),paste(mindis4.name),paste(fallow4.name)
           ,paste(femhead4.name),paste(fert4.name))){
  grid.d4@data[,i] = 0
}

for(i in names(grid.d4)){
  if(sum(is.na(grid.d4@data[,i]))>0){
    grid.d4@data[,i] = ifelse(is.na(grid.d4@data[,i]), quantile(grid.d4@data[,i], 0.5, na.rm=TRUE), grid.d4@data[,i])
  }
}
names(grid.d4)

gc()

### Create Space-time cross valitadion for each country and season separately; These will be used for both data with all predictors and categorical.
fold1 <- CreateSpacetimeFolds(d1,spacevar="Clusters",k=4)
fold2 <- CreateSpacetimeFolds(d2,spacevar="Clusters",k=4)
fold3 <- CreateSpacetimeFolds(d3,spacevar="Clusters",k=3)
fold4 <- CreateSpacetimeFolds(d4,spacevar="Clusters",k=3)
#### different control parameters
ctrl_LLO_d1 <- trainControl(method="cv",savePredictions = TRUE,returnResamp = "final",index=fold1$index,indexOut=fold1$indexOut)
ctrl_LLO_d2 <- trainControl(method="cv",savePredictions = TRUE,returnResamp = "final",index=fold2$index,indexOut=fold2$indexOut)
ctrl_LLO_d3 <- trainControl(method="cv",savePredictions = TRUE,returnResamp = "final",index=fold3$index,indexOut=fold3$indexOut)
ctrl_LLO_d4 <- trainControl(method="cv",savePredictions = TRUE,returnResamp = "final",index=fold4$index,indexOut=fold4$indexOut)
## Tune grid
ranger_grid <- expand.grid(.mtry = 10,.splitrule = c("variance"),.min.node.size = 30)

##### Build models for all predictors; Hold mtry and numtrees constant
model1_all <- caret::train(d1[,which(names(d1)%in%p1_all)],d1$Yield,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d1,num.trees=1000)
model1_all$finalModel
sqrt(model1_all$finalModel$prediction.error)

model2_all <- caret::train(d2[,which(names(d2)%in%p2_all)],d2$Yield,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d2,num.trees=1000)
model2_all$finalModel
sqrt(model2_all$finalModel$prediction.error)

model3_all <- caret::train(d3[,which(names(d3)%in%p3_all)],d3$Yield,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d3,num.trees=1000)
model3_all$finalModel
sqrt(model3_all$finalModel$prediction.error)

model4_all <- caret::train(d4[,which(names(d4)%in%p4_all)],d4$Yield,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d4,num.trees=1000)
model4_all$finalModel
sqrt(model4_all$finalModel$prediction.error)

##### Build models for the categorical datasets
fm1 = as.formula(paste(' Yield ~ ', paste(names(grid.d1), collapse="+")))
model1_cat <- caret::train(fm1,d1,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d1,num.trees=1000)
model1_cat$finalModel
sqrt(model1_cat$finalModel$prediction.error)

fm2 = as.formula(paste(' Yield ~ ', paste(names(grid.d2), collapse="+")))
model2_cat <- caret::train(fm2,d2,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d2,num.trees=1000)
model2_cat$finalModel
sqrt(model2_cat$finalModel$prediction.error)

fm3 = as.formula(paste(' Yield ~ ', paste(names(grid.d3), collapse="+")))
model3_cat <- caret::train(fm3,d3,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d3,num.trees=1000)
model3_cat$finalModel
sqrt(model3_cat$finalModel$prediction.error)

fm4 = as.formula(paste(' Yield ~ ', paste(names(grid.d4), collapse="+")))
model4_cat <- caret::train(fm4,d4,method="ranger",metric="RMSE",importance="permutation", tuneGrid = ranger_grid,trControl = ctrl_LLO_d4,num.trees=1000)
model4_cat$finalModel
sqrt(model4_cat$finalModel$prediction.error)


#### Variable importance plots
impall.d1 <- varImp(model1_all)
impall2.d1 <- as.data.frame(impall.d1$importance)
impall2.d1$varnames <- rownames(impall2.d1)
plt1_all <- ggplot(impall2.d1, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Ghana 2013 (D1)")+ xlab("All predictors") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

impall.d2 <- varImp(model2_all)
impall2.d2 <- as.data.frame(impall.d2$importance)
impall2.d2$varnames <- rownames(impall2.d2)
plt2_all <- ggplot(impall2.d2, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Ghana 2019 (D2)")+ xlab("") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

impall.d3 <- varImp(model3_all)
impall2.d3 <- as.data.frame(impall.d3$importance)
impall2.d3$varnames <- rownames(impall2.d3)
plt3_all <- ggplot(impall2.d3, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Malawi 2013 (D3)")+ xlab("") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

impall.d4 <- varImp(model4_all)
impall2.d4 <- as.data.frame(impall.d4$importance)
impall2.d4$varnames <- rownames(impall2.d4)
plt4_all <- ggplot(impall2.d4, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Malawi 2019 (D4)")+ xlab("") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

impcat.d1 <- varImp(model1_cat)
impcat2.d1 <- as.data.frame(impcat.d1$importance)
impcat2.d1$varnames <- rownames(impcat2.d1)
plt1_cat <- ggplot(impcat2.d1, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Ghana 2013 (D1)")+ xlab("categorical predictors") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

impcat.d2 <- varImp(model2_cat)
impcat2.d2 <- as.data.frame(impcat.d2$importance)
impcat2.d2$varnames <- rownames(impcat2.d2)
plt2_cat <- ggplot(impcat2.d2, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Ghana 2019 (D2)")+ xlab("") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

impcat.d3 <- varImp(model3_cat)
impcat2.d3 <- as.data.frame(impcat.d3$importance)
impcat2.d3$varnames <- rownames(impcat2.d3)
plt3_cat <- ggplot(impcat2.d3, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Malawi 2013 (D3)")+ xlab("") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

impcat.d4 <- varImp(model4_cat)
impcat2.d4 <- as.data.frame(impcat.d4$importance)
impcat2.d4$varnames <- rownames(impcat2.d4)
plt4_cat <- ggplot(impcat2.d4, aes(x=reorder(varnames, Overall), y=Overall)) +  geom_point(color="blue",size=2)+
  ggtitle("Malawi 2019 (D4)")+ xlab("") + ylab("")+ coord_flip()+theme_bw() + theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 16))

png(file = "VariableImportanceALL.png", width = 13000, height = 14000, units = "px", res = 700, type = "cairo")
gridExtra::grid.arrange(plt1_all,plt2_all,plt3_all,plt4_all,plt1_cat,plt2_cat,plt3_cat,plt4_cat,nrow=2,ncol=4)
dev.off()

#### Partial dependent plots
library(pdp)
top6.1_all = topPredictors(model1_all,n=6)
pd1_all <- NULL
for (i1 in top6.1_all) {
  tmp <- partial(model1_all, pred.var = i1)
  names(tmp) <- c("x", "y")
  pd1_all <- rbind(pd1_all, cbind(tmp, predictor = i1))
}

top_plt_all1<- ggplot(pd1_all, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5),plot.title = element_text(hjust = 0.5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("Ghana 2013 (D1)")+ ylab("All predictors") +xlab("")


top6.2_all = topPredictors(model2_all,n=6)
pd2_all <- NULL
for (i2 in top6.2_all) {
  tmp <- partial(model2_all, pred.var = i2)
  names(tmp) <- c("x", "y")
  pd2_all <- rbind(pd2_all, cbind(tmp, predictor = i2))
}

top_plt_all2<- ggplot(pd2_all, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5),plot.title = element_text(hjust = 0.5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("Ghana 2019 (D2)")+ ylab("") +xlab("")

top6.3_all = topPredictors(model3_all,n=6)
pd3_all <- NULL
for (i3 in top6.3_all) {
  tmp <- partial(model3_all, pred.var = i3)
  names(tmp) <- c("x", "y")
  pd3_all <- rbind(pd3_all, cbind(tmp, predictor = i3))
}

top_plt_all3<- ggplot(pd3_all, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5),plot.title = element_text(hjust = 0.5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("Malawi 2013 (D3)")+ ylab("") +xlab("")

top6.4_all = topPredictors(model4_all,n=6)
pd4_all <- NULL
for (i4 in top6.4_all) {
  tmp <- partial(model4_all, pred.var = i4)
  names(tmp) <- c("x", "y")
  pd4_all <- rbind(pd4_all, cbind(tmp, predictor = i4))
}

top_plt_all4<- ggplot(pd4_all, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5),plot.title = element_text(hjust = 0.5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("Malawi 2019 (D4)")+ ylab("") +xlab("")

top6.1_cat = topPredictors(model1_cat,n=6)
pd1_cat <- NULL
for (i5 in top6.1_cat) {
  tmp <- partial(model1_cat, pred.var = i5)
  names(tmp) <- c("x", "y")
  pd1_cat <- rbind(pd1_cat, cbind(tmp, predictor = i5))
}

top_plt_cat1<- ggplot(pd1_cat, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5),plot.title = element_text(hjust = 0.5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("")+ ylab("Categorical predictors") +xlab("")

top6.2_cat = topPredictors(model2_cat,n=6)
pd2_cat <- NULL
for (i6 in top6.2_cat) {
  tmp <- partial(model2_cat, pred.var = i6)
  names(tmp) <- c("x", "y")
  pd2_cat <- rbind(pd2_cat, cbind(tmp, predictor = i6))
}

top_plt_cat2<- ggplot(pd2_cat, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5),plot.title = element_text(hjust = 0.5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("")+ ylab("") +xlab("")

top6.3_cat = topPredictors(model3_cat,n=6)
pd3_cat <- NULL
for (i7 in top6.3_cat) {
  tmp <- partial(model3_cat, pred.var = i7)
  names(tmp) <- c("x", "y")
  pd3_cat <- rbind(pd3_cat, cbind(tmp, predictor = i7))
}

top_plt_cat3<- ggplot(pd3_cat, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5),plot.title = element_text(hjust = 0.5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("")+ ylab("") +xlab("")

top6.4_cat = topPredictors(model4_cat,n =6)
pd4_cat <- NULL
for (i8 in top6.4_cat) {
  tmp <- partial(model4_cat, pred.var = i8)
  names(tmp) <- c("x", "y")
  pd4_cat <- rbind(pd4_cat, cbind(tmp, predictor = i8))
}

top_plt_cat4<- ggplot(pd4_cat, aes(x, y, group=1)) + geom_line() + facet_wrap(~ predictor, scales = "free") +
  theme_bw() + theme(text = element_text(size = 10),axis.text.y = element_text(size=5))+ 
  scale_x_discrete(labels = function(x) round(as.numeric(x), digits=1))+
  theme(axis.text = element_text(size = 3))+ ggtitle("")+ ylab("") +xlab("")

top6.4_cat

png(file = "PartialpltsALL.png", width = 8000, height = 5000, units = "px", res = 700, type = "cairo")
gridExtra::grid.arrange(top_plt_all1,top_plt_all2,top_plt_all3,top_plt_all4,top_plt_cat1,top_plt_cat2,
                        top_plt_cat3,top_plt_cat4,nrow=2,ncol=4)
dev.off()

gc()

###### Making spatial predictions
ranger.d1 <- ranger(fm1, d1, mtry = 10, importance="permutation",write.forest=TRUE, num.trees=1000)
ranger.d2 <- ranger(fm2, d2, mtry = 10, importance="permutation",write.forest=TRUE, num.trees=1000)
ranger.d3 <- ranger(fm3, d3, mtry = 10, importance="permutation",write.forest=TRUE, num.trees=1000)
ranger.d4 <- ranger(fm4, d4, mtry = 10, importance="permutation",write.forest=TRUE, num.trees=1000)

## scenario 1 - No agronomic practice
grid.d1$pred = predict(ranger.d1,grid.d1@data, num.threads=56)$predictions
writeGDAL(grid.d1["pred"], "NoTreatGhana2013.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d2$pred = predict(ranger.d2,grid.d2@data, num.threads=56)$predictions
writeGDAL(grid.d2["pred"], "NoTreatGhana2019.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d3$pred = predict(ranger.d3,grid.d3@data, num.threads=56)$predictions
writeGDAL(grid.d3["pred"], "NoTreatMalawi2013.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d4$pred = predict(ranger.d4,grid.d4@data, num.threads=56)$predictions
writeGDAL(grid.d4["pred"], "NoTreatMalawi2019.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()


## scenario 2 - with fertilizer application
grid.d1@data[,"Fert"] = 1
grid.d2@data[,"Fert"] = 1
grid.d3@data[,"Fert"] = 1
grid.d4@data[,"Fert"] = 1

gc()

grid.d1$pred1 = predict(ranger.d1,grid.d1@data, num.threads=56)$predictions
writeGDAL(grid.d1["pred1"], "FertGhana2013.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d2$pred1 = predict(ranger.d2,grid.d2@data, num.threads=56)$predictions
writeGDAL(grid.d2["pred1"], "FertGhana2019.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d3$pred1 = predict(ranger.d3,grid.d3@data, num.threads=56)$predictions
writeGDAL(grid.d3["pred1"], "FertMalawi2013.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d4$pred1 = predict(ranger.d4,grid.d4@data, num.threads=56)$predictions
writeGDAL(grid.d4["pred1"], "FertMalawi2019.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()

## scenario 3 - with fertilizer and Pesticide application
grid.d1@data[,"Fert"] = 1
grid.d2@data[,"Fert"] = 1
grid.d3@data[,"Fert"] = 1
grid.d4@data[,"Fert"] = 1

grid.d1@data[,"Pest"] = 1
grid.d2@data[,"Pest"] = 1
grid.d3@data[,"Pest"] = 1
grid.d4@data[,"Pest"] = 1

grid.d1$pred2 = predict(ranger.d1,grid.d1@data, num.threads=56)$predictions
writeGDAL(grid.d1["pred2"], "FertPestGhana2013.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d2$pred2 = predict(ranger.d2,grid.d2@data, num.threads=56)$predictions
writeGDAL(grid.d2["pred2"], "FertPestGhana2019.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d3$pred2 = predict(ranger.d3,grid.d3@data, num.threads=56)$predictions
writeGDAL(grid.d3["pred2"], "FertPestMalawi2013.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()
grid.d4$pred2 = predict(ranger.d4,grid.d4@data, num.threads=56)$predictions
writeGDAL(grid.d4["pred2"], "FertPestMalawi2019.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")
gc()

######%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$$$$$$$$ THE END &&&&&&&&&&&&&&&&&&&&&&&&&&&&&$%%%%%%%%%%%%%%%%%%%%%%%######
grid.d1$pred <- cbind(x2)

grid.d1$pred

x2 <- as.data.frame(x)
?merge
library(raster)
writeRaster(grid.d1["pred"],"NotreatGhana13_LLO.tif")
writeGDAL(grid.d1["pred"], "NotreatGhana13_LLO.tif", type="Int16", mvFlag=-32678, options="COMPRESS=DEFLATE")


