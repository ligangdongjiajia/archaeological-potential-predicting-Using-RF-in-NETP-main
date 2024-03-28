setwd("D:/R/NETP-modeling")
library(dplyr)
###########################################################Read and tidy data
allsite_original<- read.csv("all_point.csv", header = TRUE)
allsite <- allsite_original[,-c(1,3,4)]
# Balanced data, sample the number of non-sites to 1600
set.seed(123)
allsite <- allsite %>%
  filter(culture == "None") %>%
  slice_sample(n = 1600) %>%
  bind_rows(allsite %>% filter(culture!= "None"))

table(allsite$culture)
sum(is.na(allsite))
# Uniform vegetation type name
allsite$veg_type <- sub("deserts", "Deserts", allsite$veg_type) 
allsite$veg_type <- sub("steppes", "Steppes", allsite$veg_type)
allsite$veg_type <- sub("meadows", "Meadows", allsite$veg_type)
allsite$veg_type <- sub("mesdows", "Meadows", allsite$veg_type)
# Hydrologic data set to ordered factors
allsite$permanent <- factor(allsite$permanent, ordered = TRUE,
                     levels=c("1","2","5","10","20"))
allsite$intermitte <- factor(allsite$intermitte, ordered = TRUE,
                     levels=c("1","2","5","10","20"))
allsite$lake<- factor(allsite$lake, ordered = TRUE,
                      levels=c("1","2","5","10","20"))
#soil type ordered by soil fertility
sort(unique(allsite_original$soil_type))
breaks_2<- c(0, 66, 76, 91, 125, 156, 175, 192, 213, 227, 228, 237)
allsite$soil_type <- cut(allsite$soil_type , breaks = breaks_2, labels = c (1, 3, 4, 4, 2, 4, 2, 3, 3, 2, 5))
allsite$soil_type <- factor(allsite$soil_type,ordered = TRUE , levels = c ("1", "2", "3", "4", "5"))
unique(allsite$soil_type)
#classfy aspect 
break_asp<- c(-2, 0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
allsite$aspect <- cut(allsite$aspect, breaks = break_asp, 
                     labels = c ("flat", "N","NE", "E", "SE", "S", "SW", "W", "NW", "N"))
unique(allsite$aspect)
# soil erosion and vegetation types set to factors
allsite$soil_erosi<- as.factor(allsite$soil_erosi)
allsite$veg_type<- as.factor(allsite$veg_type)
str(allsite)
# Character correction 
unique(allsite$culture)
allsite$culture <- sub(" Majiayao", "Majiayao", allsite$culture)
allsite$culture <- sub("Xndian",  "Xindian", allsite$culture)
# Remove unnecessary cultural attributes 
cultures_to_remove <- c("undistinguished Neolithic cultures", "undistinguished Neolithic to Iron Age cultures","Shajing", "Siba","Siwa")
data_filtered <- allsite[!(allsite$culture %in% cultures_to_remove), ]
str(data_filtered )
# Archaeological culture is reclassified into three categories
data_filtered <- data_filtered %>%
  mutate(culture = case_when(
    culture %in% c("Majiayao", "Banshan", "Machang", "Yangshao", "Shilingxia","Zongri") ~ "YS-MJY", 
    culture %in% c("Qijia") ~ "QJ",  
    culture %in% c("Kayue", "Xindian", "Nuomuhong") ~ "KXN",
    culture %in% c("None") ~ "Non-sites",
    TRUE ~ culture  
  ))

table(data_filtered$culture)
str(data_filtered)

###################################### correlation analysis
data_filtered_new <- data_filtered %>%
  filter(culture != "None")
allsite_num <-  data_filtered_new[, -c(1, 4, 7, 8, 9, 11, 12, 13)] #Numerical variables
allsite_factor <- data_filtered_new[, c( 4, 7, 8, 9, 11, 12, 13)] #factor variables
str(allsite_num)
str(allsite_factor)
site_cor_data <- cbind(allsite_num,allsite_factor[, c(2:4,6)] ) 
site_cor_data [10:13]<- lapply(site_cor_data[10:13], function(x) as.numeric(x))

str(site_cor_data)

cor_matrix <- cor(site_cor_data, use = "pairwise.complete.obs")
library(gplots)
heatmap.2(cor_matrix,
          cellnote = round(cor_matrix, 2), 
          notecex=0.7,
          main = "Correlation Heatmap", 
          col = colorRampPalette(c("blue", "white", "red"))(30), 
          key = TRUE, 
          key.title = NA, 
          key.xlab = "Correlation", 
          trace = "none",
          cexRow = 0.8,
          cexCol = 0.8)

str(data_filtered)
table(data_filtered$culture)

########################  Single factor comparison

data_filtered$culture <- factor(data_filtered$culture, levels = c("YS-MJY","QJ","KXN","Non-sites"))
data_filtered2<-data_filtered 
text(x = barplot(table(data_filtered$culture),ylim = c(0, 2000)), y = table(data_filtered$culture), labels = table(data_filtered$culture), pos = 3)

library(ggplot2)
fill_colors <- c("#E74C3C", "#1ABC9C", "#3498DB", "#CCCCCC" ) 

###################################### numeric type ploting

ggplot(data_filtered, aes(x = culture, y = data_filtered$elevation, fill = data_filtered$culture)) +
  geom_violin() +
  scale_fill_manual(values = fill_colors)+
  geom_boxplot(fill="white", width=.1, size=0.1)

ggplot(data_filtered, aes(x = culture, y = data_filtered$slope, fill = data_filtered$culture)) +
  geom_violin() +
  geom_point(stat = "summary", shape = 23, size = 3, color = "black")+
  scale_fill_manual(values = fill_colors) 

ggplot(data_filtered, aes(x = culture, y = data_filtered$fluctuation, fill = data_filtered$culture)) +
  geom_violin() +
  geom_point(stat = "summary", shape = 23, size = 3, color = "black")+
  scale_fill_manual(values = fill_colors)  

ggplot(data_filtered, aes(x = culture, y = data_filtered$curvature, fill = data_filtered$culture)) +
  geom_violin() +
  geom_point(stat = "summary", shape = 23, size = 3, color = "black")+
  scale_fill_manual(values = fill_colors)  

ggplot(data_filtered, aes(x = culture, y = data_filtered$NDVI, fill = data_filtered$culture)) +
  geom_violin() +
  scale_fill_manual(values = fill_colors)+
  geom_boxplot(fill="white", width=.1, size=0.1)

ggplot(data_filtered, aes(x = culture, y = data_filtered$grassland, fill = data_filtered$culture)) +
  geom_violin() +
  scale_fill_manual(values = fill_colors)+ 
  geom_boxplot(fill="white", width=.1, size=0.1)+  
  geom_hline(yintercept = c(20, 43.59, 56.25), linetype = "dashed", color = "black") 

ggplot(data_filtered, aes(x = culture, y = data_filtered$cultivated, fill = data_filtered$culture)) +
  geom_violin() +
  scale_fill_manual(values = fill_colors)+ 
  geom_boxplot(fill="white", width=.1, size=0.1)+  
  geom_hline(yintercept = c(42.1, 57.33, 67.07), linetype = "dashed", color = "black")

ggplot(data_filtered, aes(x = culture, y = data_filtered$temperature, fill = data_filtered$culture)) +
  geom_violin() +
  scale_fill_manual(values = fill_colors)+ 
  geom_boxplot(fill="white", width=.1, size=0.1)

ggplot(data_filtered, aes(x = culture, y = data_filtered$precipitation, fill = data_filtered$culture)) +
  geom_violin() +
  scale_fill_manual(values = fill_colors)+ 
  geom_boxplot(fill="white", width=.1, size=0.1)

########################################### Factored data ploting
str(data_filtered)

data_filtered$aspect <- factor(data_filtered$aspect, levels = c("flat", "N", "NW", "NE", "W", "E", "SW", "SE", "S"))
ggplot(data_filtered, aes(x = culture, fill = aspect)) +
  geom_bar(position = "fill", width = 0.7 )

data_filtered$permanent <- factor(data_filtered$permanent, levels = c( "20", "10", "5", "2","1"))
ggplot(data_filtered, aes(x = culture, fill = permanent)) +
  geom_bar(position = "fill", width = 0.7 )

data_filtered$intermitte <- factor(data_filtered$intermitte, levels = c( "20", "10", "5", "2","1"))
ggplot(data_filtered, aes(x = culture, fill = intermitte)) +
  geom_bar(position = "fill", width = 0.7 )

data_filtered$lake<- factor(data_filtered$lake, levels = c( "20", "10", "5", "2","1"))
ggplot(data_filtered, aes(x = culture, fill = lake)) +
  geom_bar(position = "fill", width = 0.7 )

colors_veg <- c("#666666", "#AAAAAA", "#a61c00", "#f6b26b", "#006400", "#008B45", "#6aa84f", 
                "#a64d79", "#45818e", "#00FA9A", "#00EE76", "#00FF00")

data_filtered$veg_type <- factor(data_filtered$veg_type, levels = c( "No vegetation", "Deserts", "Marshes" , "Alpine Vegetation"  ,"Broadleaf Forests", "Needleleaf and Broadleaf Mixed Forests"  , "Needleleaf Forests" , "Cultivated Vegetation","Scrubs" , "Meadows","Grasslands","Steppes" ))
ggplot(data_filtered, aes(x = culture, fill = veg_type)) +
  geom_bar(position = "fill", width = 0.7 )+
  scale_fill_manual(values = colors_veg )

data_filtered$soil_type <- factor(data_filtered$soil_type, levels = c( "5", "4", "3", "2","1"))
ggplot(data_filtered, aes(x = culture, fill = soil_type)) +
  geom_bar(position = "fill", width = 0.7 )

colors_soil <- c("#43a7ea", "#43d4ea", "#43ead5", "#43ea9a", "#a61c00", "#ff0000", 
                 "#ff5b00", "#ff9e00", "#ffd700", "#e4ff00",  "#1636b2","#2600ff","#1378ec","#49b6ce",
                 "#49ceab", "#bdeae4")

data_filtered$soil_erosi <- factor(data_filtered$soil_erosi, levels = c( "34", "33", "32", "31",
                                                                             "26","25","24","23", "22", "21",
                                                                             "16","15", "14", "13", "12","11"))
ggplot(data_filtered, aes(x = culture, fill = soil_erosi)) +
  geom_bar(position = "fill", width = 0.7 )+
  scale_fill_manual(values = colors_soil )

###################################### data pre-processing  #Delete data that correlation coefficient is high
library(MASS)
data_filtered2$temperature<- NULL
data_filtered2$fluctuation<- NULL  

######################################classification tree model
library(partykit)
library(rpart)
set.seed(123)
tree<- rpart(culture~., data = na.omit(data_filtered2))
tree$cptable 
cp<- min(tree$cptable[5,])
tree2<- prune(tree, cp<-cp)
library(rpart.plot)
par(mar=c(0,0,0,0))
rpart.plot(tree2,cex=0.7,type=5)
################################################################# RF parameter optimization

library(mlr)
library(tidyverse)
data<- na.omit(data_filtered2)
Task<- makeClassifTask(data = data, target = "culture")
forest3<- makeLearner("classif.randomForest")
getParamSet(forest3)
forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 100, upper = 1500),
  makeIntegerParam("mtry", lower = 2, upper = 14),
  makeIntegerParam("nodesize", lower = 1, upper = 20),
  makeLogicalParam("importance", default = FALSE)
) #makeIntegerParam("maxnodes", lower = 1, upper = 400),
randSearch <- makeTuneControlRandom(maxit=100)
cvForTuning<-makeResampleDesc("CV", iters = 5)

library(parallel)
library(parallelMap)
parallelStartSocket(cpus=detectCores())
set.seed(123)
tunedForestPars<- tuneParams(forest3, task=Task,
                             resampling = cvForTuning,
                             par.set = forestParamSpace,
                             control = randSearch)
parallelStop()
tunedForestPars
tunedForest <- setHyperPars(forest3, par.vals = tunedForestPars$x)

ForestModel <- train(tunedForest, Task)
forest4<- getLearnerModel(ForestModel)
forest4
plot(forest4)

##################################################################importance ranking
varImpPlot(forest4, scale = TRUE)
b<-importance(forest4)
b
par(mar=c(4,10,2,1))
par(las=2)
barplot(sort(b[,6]),horiz = TRUE)
################################################################### OOB confusion Matrix
c<- as.data.frame(forest4$confusion)
c<- c[-5]
col_totals <- rowSums(c)
confusion_matrix_percent <- c / col_totals
library(reshape2)
confusion_matrix_percent$prediction<- names(confusion_matrix_percent)
CM<- melt(confusion_matrix_percent, id="prediction")
colnames(CM)[2]<- "reference"
CM$value<- round(CM$value,2)
CM$reference <- factor(CM$reference, levels = c("YS-MJY","QJ","KXN","Non-sites"))#
CM$prediction <- factor(CM$prediction, levels = c("YS-MJY","QJ","KXN","Non-sites"))#
ggplot(data = CM, aes(x = reference , y = prediction, fill = value)) +
  geom_tile() +
  geom_text(aes(label =value), vjust = 1) +
  labs(fill = "Count") +
  scale_fill_gradient(low = "white", high = "lightblue3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################################################  cross validation
Task<- makeClassifTask(data = data, target = "culture")
forest5<- makeLearner("classif.randomForest", par.vals = 
                        list(ntree=926, mtry=5, nodesize=20),  predict.type = "prob")

kFold <- makeResampleDesc(method = "RepCV", folds=10, reps = 5, stratify = TRUE)
kfoldCV <- resample(learner = forest5, task = Task, resampling = kFold, measures = list(mmce, acc, kappa, multiclass.aunu))
kfoldCV$aggr

write.csv(kfoldCV$measures.test, file="CV.csv")
################################################## cross validation mean confusion matrix
c<-calculateConfusionMatrix(kfoldCV$pred)

c<- as.data.frame(c$result)
c<- c[-5,-5]
col_totals <- rowSums(c)
confusion_matrix_percent <- c / col_totals
library(reshape2)
confusion_matrix_percent$prediction<- names(confusion_matrix_percent)
CM<- melt(confusion_matrix_percent, id="prediction")
colnames(CM)[2]<- "reference"
CM$value<- round(CM$value,2)
CM$reference <- factor(CM$reference, levels = c("YS-MJY","QJ","KXN","Non-sites"))#
CM$prediction <- factor(CM$prediction, levels = c("YS-MJY","QJ","KXN","Non-sites"))#
ggplot(data = CM, aes(x = reference , y = prediction, fill = value)) +
  geom_tile() +
  geom_text(aes(label =value), vjust = 1) +
  labs(fill = "Count") +
  scale_fill_gradient(low = "white", high = "lightblue3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##############################################################  holdout validation
set.seed(123)
ind <- sample(2, nrow(data_filtered2), replace = TRUE, prob = c(0.8, 0.2))
train<- na.omit(data_filtered2[ind==1, ])
test<- na.omit(data_filtered2[ind==2, ])

table(train$culture)
table(test$culture)
###################### train data
library(randomForest)
library(caret)
set.seed(123)
forest<- randomForest(culture ~., data = train, importance=TRUE, na.action = na.exclude, ntree = 926, mtry = 5, nodesize = 20)
forest
###################### There is no parameter optimization model
forest2<- randomForest(culture ~., data = data_filtered2, importance=TRUE, na.action = na.exclude, ntree = 1500)
forest2
#####################  test data
forest_predict_test <- predict(forest, newdata = test[, -1], type= "response")
forest_test<-confusionMatrix(as.factor(test$culture),as.factor(forest_predict_test)  ) 
forest_test
confusion_df<- forest_test$table
col_totals <- rowSums(confusion_df)
confusion_matrix_percent <- confusion_df / col_totals
CM<-as.data.frame(confusion_matrix_percent)
CM$Freq<- round(CM$Freq, 2)
ggplot(data = CM, aes(x =Reference , y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(fill = "Count") +
  scale_fill_gradient(low = "white", high = "lightblue3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
####################  calculate AUC
library(pROC)
predict_test_prob<- predict(forest, newdata = test[, -1], type<- "prob")
multiclass.roc(test$culture, predict_test_prob)
############################################################## prediction
library(sf)
library(raster)
library(sp)
prediction <- st_read("points.shp")
str(prediction)
colnames(prediction)
#########################################preprocessing  points data of whole NETP
pre<- prediction[,c(  "elevatn" , "slope"  ,  "aspect" ,  "curvatr" , "permnnt" , "intrmtt" , "lake"  ,   "NDVI" , "veg_typ" ,  "sol_typ" , "soil_rs" ,
                    "grsslnd" , "cultvtd" , "prcpttn" , "geometry")]
str(pre)
colnames(pre)[1:14] <- colnames(data_filtered2)[-1]

# Hydrologic data set to ordered factors
pre$permanent <- factor(pre$permanent, ordered = TRUE,
                              levels=c("1","2","5","10","20"))
pre$intermitte <- factor(pre$intermitte, ordered = TRUE,
                               levels=c("1","2","5","10","20"))
pre$lake<- factor(pre$lake, ordered = TRUE,
                        levels=c("1","2","5","10","20"))

#soil type ordered by soil fertility
sort(unique(pre$soil_type))
sort(unique(data_filtered2$soil_type))
breaks_2<- c(0, 66, 76, 91, 125, 156, 175, 192, 213, 227, 228, 237)
pre$soil_type <- cut(pre$soil_type , breaks = breaks_2, labels = c (1, 3, 4, 4, 2, 4, 2, 3, 3, 2, 5))
pre$soil_type <- factor(pre$soil_type,ordered = TRUE , levels = c ("1", "2", "3", "4", "5"))
unique(pre$soil_type)
#classfy aspect 
pre$soil_type <- factor(pre$soil_type,ordered = TRUE , levels = c ("1", "2", "3", "4", "5"))
break_asp<- c(-2, 0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360)
pre$aspect <- cut(pre$aspect, breaks = break_asp, 
                        labels = c ("flat", "N","NE", "E", "SE", "S", "SW", "W", "NW", "N"))
unique(pre$aspect)
unique(data_filtered2$aspect)

# soil erosion and vegetation types set to factors
levels(pre$veg_type)
levels(data_filtered2$veg_type)
pre$veg_type <- factor(pre$veg_type, levels = levels(data_filtered2$veg_type))

pre$soil_erosi <- sub("34", "33", pre$soil_erosi) 
levels(pre$soil_erosi)
levels(data_filtered2$soil_erosi)
pre$soil_erosi <- as.factor(pre$soil_erosi)
pre$veg_type <- as.factor(pre$veg_type)
str(pre)
str(data_filtered2)
forest4
culture_predict <- predict(forest4, newdata = pre[,1:14], type= "prob")
pre_result <- cbind(pre, culture_predict)
pre_result2 <- pre_result[,15:19, drop = FALSE]

pre_result2 [, 1:3] <- lapply(pre_result2 [, 1:3], function(x) ifelse(is.na(x), 0, x))
pre_result2 [, 4] <- lapply(pre_result2 [, 4], function(x) ifelse(is.na(x), 1, x))

DEM <- raster("D:/R/NETP/variables/TPDEM_prj")
DEM_1000 <- aggregate(DEM, fact = 1000/res(DEM), fun = mean)
YS_MJY <- rasterize(x = pre_result2, y= DEM_1000,  field = "YS.MJY")
QJ <- rasterize(x = pre_result2, y= DEM_1000,  field = "QJ")
KXN <- rasterize(x = pre_result2, y= DEM_1000,  field = "KXN")
Non.sites <- rasterize(x = pre_result2, y= DEM_1000,  field = "Non.sites")

plot(YS_MJY)
plot(QJ)
plot(KXN)
plot(Non.sites)

writeRaster(YS_MJY, filename="YS_MJY.tif", format="GTiff", overwrite=TRUE)
writeRaster(QJ, filename="QJ.tif", format="GTiff", overwrite=TRUE)
writeRaster(KXN, filename="KXN.tif", format="GTiff", overwrite=TRUE)
writeRaster(Non.sites, filename="Non.sites.tif", format="GTiff", overwrite=TRUE)

############################################################# calculating Kvamme gain
cultural_relics <- read.csv("culture.point/cultural_relics.csv")
dated_sites <- read.csv("culture.point/dated_sites.csv")

cultural_relics1 <- st_as_sf(cultural_relics, coords = c("Longitude", "Latitude"), crs = 4326)
cultural_relics1 <- st_transform(cultural_relics1, crs = crs(DEM))

dated_sites1 <- st_as_sf(dated_sites, coords = c("Longitude.E.", "Latitude.N."), crs = 4326)
dated_sites1 <- st_transform(dated_sites1, crs = crs(DEM))

colnames(cultural_relics1)[c(2,7,41)] <- c("site_name", "culture", "geometry")
colnames( dated_sites1)[c(2,4,12)] <- c("site_name", "culture", "geometry")

all_point <- rbind(cultural_relics1[,c(2,7,41)], dated_sites1[,c(2,4,12)])

study_area <- st_read("study_area/study_area.shp")
study_area <- st_transform(study_area, crs = crs(DEM))

all_point <- st_intersection(study_area, all_point)
all_point[,1:15]<- NULL

plot(all_point)

all_point$pro <- extract(Non.sites, all_point)

k<-sum(pre_result2$Non.sites <= 0.5)/nrow(pre_result2)
l<-sum(all_point$pro <= 0.5)/nrow(all_point)

gain=1-k/l
gain
############################################################# Three classification models for different cultural stages
data_filtered3<- data_filtered2 %>%
  filter(culture!= "Non-sites")
table(data_filtered3$culture)
data_filtered3$culture <- factor(data_filtered3$culture, levels = c("YS-MJY","QJ","KXN"))

data<- na.omit(data_filtered3)
Task<- makeClassifTask(data = data, target = "culture")

forest6 <- makeLearner("classif.randomForest")
getParamSet(forest3)

forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 100, upper = 1500),
  makeIntegerParam("mtry", lower = 2, upper = 14),
  makeIntegerParam("nodesize", lower = 1, upper = 20),
  makeLogicalParam("importance", default = FALSE)
) #makeIntegerParam("maxnodes", lower = 1, upper = 400),
randSearch <- makeTuneControlRandom(maxit=100)
cvForTuning<-makeResampleDesc("CV", iters = 5)

library(parallel)
library(parallelMap)
parallelStartSocket(cpus=detectCores())

set.seed(123)
tunedForestPars<- tuneParams(forest6, task=Task,
                             resampling = cvForTuning,
                             par.set = forestParamSpace,
                             control = randSearch)

parallelStop()
tunedForestPars
tunedForest <- setHyperPars(forest6, par.vals = tunedForestPars$x)

ForestModel <- train(tunedForest, Task)

forest7<- getLearnerModel(ForestModel)
forest7
plot(forest7)

###########importance ranking

varImpPlot(forest7, scale = TRUE)
b<-importance(forest7)
b
par(mar=c(4,10,2,1))
par(las=2)
barplot(sort(b[,1]),horiz = TRUE)

########## OOB confusion Matrix

c<- as.data.frame(forest7$confusion)
c<- c[,-4]
col_totals <- rowSums(c)
confusion_matrix_percent <- c / col_totals
library(reshape2)
confusion_matrix_percent$prediction<- names(confusion_matrix_percent)
CM<- melt(confusion_matrix_percent, id="prediction")
colnames(CM)[2]<- "reference"
CM$value<- round(CM$value,2)
CM$reference <- factor(CM$reference, levels = c("YS-MJY","QJ","KXN"))#
CM$prediction <- factor(CM$prediction, levels = c("YS-MJY","QJ","KXN"))#
ggplot(data = CM, aes(x = reference , y = prediction, fill = value)) +
  geom_tile() +
  geom_text(aes(label =value), vjust = 1) +
  labs(fill = "Count") +
  scale_fill_gradient(low = "white", high = "lightblue3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################################################Binary classification model

data_filtered4<- data_filtered2
data_filtered4$culture <- ifelse(data_filtered4$culture == "Non-sites", 0, 1)
table(data_filtered4$culture)
data_filtered4$culture<- factor(data_filtered4$culture, levels = c(0,1), labels=c("none","other"))

data<- na.omit(data_filtered4)
Task<- makeClassifTask(data = data, target = "culture")

forest8 <- makeLearner("classif.randomForest")
getParamSet(forest8)

forestParamSpace <- makeParamSet(
  makeIntegerParam("ntree", lower = 100, upper = 1500),
  makeIntegerParam("mtry", lower = 2, upper = 14),
  makeIntegerParam("nodesize", lower = 1, upper = 20),
  makeLogicalParam("importance", default = FALSE)
) #makeIntegerParam("maxnodes", lower = 1, upper = 400),
randSearch <- makeTuneControlRandom(maxit=100)
cvForTuning<-makeResampleDesc("CV", iters = 5)

library(parallel)
library(parallelMap)
parallelStartSocket(cpus=detectCores())

set.seed(123)
tunedForestPars<- tuneParams(forest8, task=Task,
                             resampling = cvForTuning,
                             par.set = forestParamSpace,
                             control = randSearch)

parallelStop()
tunedForestPars
tunedForest <- setHyperPars(forest8, par.vals = tunedForestPars$x)

ForestModel <- train(tunedForest, Task)

forest9<- getLearnerModel(ForestModel)
forest9
plot(forest9)

###########importance ranking

varImpPlot(forest9, scale = TRUE)
b<-importance(forest9)
b
par(mar=c(4,10,2,1))
par(las=2)
barplot(sort(b[,4]),horiz = TRUE)

########## OOB confusion Matrix

c<- as.data.frame(forest9$confusion)
c<- c[,-3]
col_totals <- rowSums(c)
confusion_matrix_percent <- c / col_totals
library(reshape2)
confusion_matrix_percent$prediction<- names(confusion_matrix_percent)
CM<- melt(confusion_matrix_percent, id="prediction")
colnames(CM)[2]<- "reference"
CM$value<- round(CM$value,2)
CM$reference <- factor(CM$reference, levels = c("none","other"))#
CM$prediction <- factor(CM$prediction, levels = c("none","other"))#
ggplot(data = CM, aes(x = reference , y = prediction, fill = value)) +
  geom_tile() +
  geom_text(aes(label =value), vjust = 1) +
  labs(fill = "Count") +
  scale_fill_gradient(low = "white", high = "lightblue3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########### SOM modeling, data preprocessing

allsite_num <-  na.omit(data_filtered)[, -c(1, 4, 7, 8, 9, 11, 12, 13)] 
allsite_factor <- na.omit(data_filtered)[, c(4, 7, 8, 9, 11, 12, 13)] 
str(allsite_num)
str(allsite_factor)

############ dummy variable process
library(caret)
dummies<- dummyVars(~ aspect + permanent-1 + intermitte-1 + lake-1 + veg_type + soil_type + soil_erosi, allsite_factor) 
#dummies<- dummyVars(~ veg_type, allsite_factor) 
dummies
allsite_factor2 <- data.frame(predict(dummies, newdata= allsite_factor))
sum(is.na(allsite_factor2))
alldata2 <- cbind(allsite_num, allsite_factor2)
sum(is.na(alldata2))

culture<- na.omit(data_filtered)[1]
library(kohonen)
alldata2<-as.matrix(alldata2)

################################################################################### som modeling
set.seed(123)
somGrid <- somgrid(xdim=20, ydim=20, topo = "rectangular", neighbourhood.fct = "gaussian", toroidal = FALSE)
fleaSom <- som(alldata2, grid= somGrid, rlen =5000, alpha= c(0.05, 0.01))

som.hc <- cutree(hclust(object.distances(
  fleaSom, "codes")),8)
plot(fleaSom)
add.cluster.boundaries(fleaSom, som.hc)

par(mfrow = c(1, 1))

plot(fleaSom, type="changes", shape="straight")
plot(fleaSom, type="mapping", shape="straight")
plot(fleaSom, type="codes")
plot(fleaSom, type="counts", shape="straight")
plot(fleaSom, type = 'dist.neighbours')

fill_colors <- c("#E74C3C", "green3", "#3498DB", "#CCCCCC"  )
plot(fleaSom,type="mapping", pch=21, bg = fill_colors[as.numeric(culture$culture)], shape= "straight", bgcol="white")
add.cluster.boundaries(fleaSom, som.hc)

plot(fleaSom,type="property", property = alldata2[,1], shape="straight")
plot(fleaSom,type="property", property = alldata2[,2], shape="straight")

fleaSom$unit.classif
data_som <- cbind(culture, fleaSom$unit.classif)
library(reshape2)
data_som2<- dcast(data_som, fleaSom$unit.classif~ culture)
colnames(data_som2)[1]<- "counts"

###############################Replace Na data
find_missing_numbers <- function(numbers) {  
  missing_numbers <- c()  
  for (i in 2:length(numbers)) {  
    diff <- numbers[i] - numbers[i-1]  
    if (diff > 1) {  
      missing_numbers <- c(missing_numbers, numbers[i]-1)  
    }  
  }  
  return(missing_numbers)  
} 

missing_numbers <- find_missing_numbers(data_som2$counts)  
missing_numbers

data_som4<- data_som2
for (i in 1:length(missing_numbers)) { 
  index <- missing_numbers[i]-1
  new_row1 <- c(missing_numbers[i], 0, 0, 0, 1)
  data_som4 <- rbind(data_som4[1:index,], new_row1, data_som4[index+1:nrow(data_som4),])
}

data_som4 <- data_som4[1:400,]

par(mar = c(0.1, 0.1, 0.1,0.1))
par(mfrow = c(20, 20))

for (i in 1:nrow(data_som4)) {
  pie(as.numeric(data_som4[i,-1]), labels = NA, col=fill_colors)
}

################################################################ 4 important variables SOM

alldata3<- alldata2[,c(1,5,7,9)]

set.seed(123)
somGrid <- somgrid(xdim=20, ydim=20, topo = "rectangular", neighbourhood.fct = "gaussian", toroidal = FALSE)
fleaSom <- som(alldata3, grid= somGrid, rlen =5000, alpha= c(0.05, 0.01))

som.hc <- cutree(hclust(object.distances(
  fleaSom, "codes")),8)

som.hc <- cutree(hclust(object.distances(
  fleaSom, "codes")),8)

plot(fleaSom)
add.cluster.boundaries(fleaSom, som.hc)

par(mfrow = c(1, 1))

plot(fleaSom, type="changes", shape="straight")
plot(fleaSom, type="mapping", shape="straight")
plot(fleaSom, type="codes", shape="straight")
plot(fleaSom, type="counts", shape="straight")
plot(fleaSom, type = 'dist.neighbours')

fill_colors <- c("#E74C3C", "green3", "#3498DB", "#CCCCCC"  )
plot(fleaSom,type="mapping", pch=21, bg = fill_colors[as.numeric(culture$culture)], shape= "straight", bgcol="white")
add.cluster.boundaries(fleaSom, som.hc)

plot(fleaSom,type="property", property = getCodes(fleaSom, 1)[,1], shape="straight", ncolors = 8,palette.name = terrain.colors)

