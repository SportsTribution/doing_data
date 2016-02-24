### Random Forest
  ## We will use a few functions, which are outsourced into "car_data_00_classifier_functions.R"
  ## Let's call those functions
source("car_data/car_data_00_functions.R")

  ## So, function for cross validation and parallel computing wrapper for Random Forest are written
  ## What do we want to classify? How about getting car brand from (mostly) eco data!? Sounds good to me

  ## Let's first aggregate a little bit further
carInfo <- c("Model","Displ","Cyl","Fuel","Drive")
ecoInfo <- c("Air.Pollution.Score","City.MPG","Hwy.MPG","Cmb.MPG","Greenhouse.Gas.Score")
carDataRF <- carData[,c(carInfo,ecoInfo)]
carDataRF <- aggregate(carDataRF[,ecoInfo], carData[,carInfo],  FUN = mean )

  ## Let's get the brands. luckily, that's easy
carDataRF$Brand <- sub(' .*', '', carDataRF$Model)
print(table(carDataRF$Brand))
  ## Okay... I want to have all cars that have more than 44 cars (So that we have Volvo and Volkswagen)
tabBrand <- table(carDataRF$Brand)
tabBrand <- tabBrand[tabBrand>=45]

carDataRF <- carDataRF[carDataRF$Brand %in% names(tabBrand),]

  ## And it will be nicer if they have understandable row names (you'll later see why)
rownames(carDataRF) <- apply( carDataRF[ , carInfo ] , 1 , paste , collapse = "_" )

  ## Now we need to variables: Our classes and the classifier data
gIdF <- droplevels(as.factor(carDataRF$Brand))
gId <- as.integer(gIdF)
mapInt <- table(gIdF)
mapInt <- names(mapInt)

dataSet <- carDataRF[,!(colnames(carDataRF) %in% c("Model","Brand"))]
  ## We only have two Fuel Types and two drive types. Thus, we'll transform the two into Binary
dataSet$Fuel.isDiesel <- as.integer(dataSet$Fuel=="Diesel")
dataSet$Fuel <- NULL
dataSet$Drive.is4WD <- as.integer(dataSet$Drive=="4WD")
dataSet$Drive <- NULL

  ## 9 variables and 16 groups... I'm not expecting much. But we'll see

print(dataSet)
list[pId,topFilters] <- kCrossVal(kCross=5,dataSet=dataSet,gId=gId)
  ## printing the importance, we see that Displacement and Air.Pollution.Score are our most informative variables

  ## Let's get our predictors as factors
pIdF<-as.factor(mapInt[pId])

  ## Let's look at the confusion matrix
confMat <- confusionMatrix(pIdF,gIdF)
print(confMat)

  ## We melt the table for ggplo2t2 and then use geom_tile
confMatMelt<-melt(confMat[[2]])
c.scale <- scale_fill_gradient(low = "white", 
                               high = "steelblue3",
                               limits=c(min(confMatMelt$value),
                                        max(confMatMelt$value)),
                               trans = 'sqrt')
p<-ggplot(confMatMelt, aes(Prediction, Reference)) + 
  geom_tile(aes(fill = value), colour="white") +
  geom_text(aes(fill = value,label= value))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  ggtitle(paste("Confusion Matrix Cars"))
p<-p + c.scale
ggsave("car_data/car_output/rf_1_confusion_matrix.png")
plot(p)
  ## That looks actually pretty good. The reason is most likely that we have from each brand a lot of submodels
  ## Those submodels probably are technically more or less the same
  ## We have by far the biggest problems for Chevrolet vs GMC and Kia vs Hyundai. Both not surprising

## We'll look at a pruned decision tree - because we can
dataSetTree<-dataSet
dataSetTree$Brand <- as.factor(gId)
brand_ctree <- ctree(Brand ~ ., data=dataSetTree,controls=ctree_control( maxdepth = 5))
png("car_data/car_output/rf_2_decision_tree.png",width=2000)
plot(brand_ctree)
dev.off()
  ## We see that a lot of brands can get classified by having exactly the same information
  ##for displacement or air.pollution.score. So, probably not the perfect example


  ## ToDo: Fix heatmap function and add it to the output
# yId <- as.matrix(rbind(gId,pId))
# prepareHeatmap3(dataSet,yId,'test','test','Click-Menge') 