memory.limit()
memory.limit(56000)

#Importing libraries
library(raster)
library(rgdal)
library(caret)

#Import the shapefile containing training data
trainD <- readOGR('C:/march2018/TrainingData.shp')

#Importing images or the bands or any variables you are going to use as input data
s1 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/January/clip_B4Jan.tif')
s2 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/february/clip_B2Febr.tif')
s3 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/february/clip_B4feb.tif')
s4 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/March/clip_B2March.tif')
s5 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/March/clip_B4march.tif')
s6 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/March/clip_B8AMarch.tif')
s7 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/March/clip_B11march.tif')
s8 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/April/clip_B2April.tif')
s9 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/April/clip_B4April.tif')
s10 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/April/clip_B8April.tif')

SAVI <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/INDICES/Jan_SAVI.tif')
NDVI <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/INDICES/Feb_NDVI.tif')
EVI2M <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/INDICES/March_EVI2.tif')
EVI2A <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/INDICES/April_EVI2.tif')
NDBI <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/INDICES/NDBI.tif')

h1 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/SAR/VH/clip_VHJan.tif')
h2 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/SAR/VH/clip_VHFebr.tif')
h3 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/SAR/VH/clip_VHMar.tif')
h4 <- raster('C:/Users/masizaw/Desktop/Sentinel 2 Images/All Clipped/Experimentation/SAR/VH/clip_VHApr.tif')

#Stack the data
MyStack <- stack(s1, s2, s3, s4, s5, s6, s7, s8, SAVI, NDVI, EVI2M, EVI2A, NDBI, s9, s10, h1, h2, h3, h4)

#Rename your bands to b1, B2, B3...Bn. Unless you want to refer to them later (e.g. variable Importance)
names(MyStack) <- paste0("B", c(1:19))

#extract the reflectance data
BandValues <- extract(MyStack, trainD, df=TRUE)

#attach the labels to this DataFrame
BandValues$lc <- as.factor(trainD$class[BandValues$ID])
BandValues$desc <- as.factor(trainD$class[BandValues$ID])

#Create Training and Testing data
Data_Split <- createDataPartition(y = BandValues$lc, p = 0.7, list = FALSE)
Testing <- BandValues[-Data_Split,]
Modelbuilder <- bandsvalues[Data_Split,]

# Create two subsets from the modelbulding data
Level1Data <- createDataPartition(y = Modelbuilder$lc, p = 0.7, list = FALSE)
Training <- Modelbuilder[Level1Data,]
Predicting <- Modelbuilder[-Level1Data,]


#5-fold cross-validation
trainctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

#Train your first model, see how well it fits, predict on new data, and then compute classification accuracy
set.seed(42)
my_rf <- train(factor(lc) ~ B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13 + B14 + B15 + B16 + B17 + B18 + B19, data=Training, method = "rf",
              preProcess = c("center", "scale"),
              trControl = trainctrl,
              metric="Kappa")
pred_rf <- predict(my_rf, Predicting)
confusionMatrix(pred_rf, Predicting$lc)

set.seed(42)
my_svm <- train(factor(lc) ~ B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13 + B14 + B15 + B16 + B17 + B18 + B19, data=Training, method = "svmPoly",
               preProcess = c("center", "scale"),
               trControl = trainctrl,
               metric="Kappa")
pred_svm <- predict(my_svm, Predicting)
confusionMatrix(pred_svm , Predicting$lc)

set.seed(42)
my_nn <- train(factor(lc) ~B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13 + B14 + B15 + B16 + B17 + B18 + B19, data=SSRTrain, method = "nnet",
              preProcess = c("center", "scale"),
              trControl = trainctrl,
              metric="Kappa")
pred_nn<- predict(my_nn, Predicting)
confusionMatrix(pred_nn, Predicting$lc)

#Model comparisons, mean accuracy, check model correlations
compared <- resamples(list(mod1 = my_rf,mod2 = my_svm, mod3 = my_nn)) 
modelCor(compared) 
summary(compared)

#Are the model differences significant?
difValues <- diff(compared)
summary(difValues)

#Create a new dataframe in which you combine the predictions of the submodels
predDi <- data.frame(pred_rf, pred_svm, pred_nn, class = Testing$lc)

#Fit your meta-classifier, in this case Extreme Gradient Boosting, with the predictions from the submodels
set.seed(42)
modelstack1 <- train(factor(class) ~., data = predDi, method = "xgbTree",
                     preProcess = c("center", "scale"),
                     trControl = trainctrl,
                     metric="Kappa")

#Do you have to do this?
predmstack<- predict(modelstack1, predDF)
confusionMatrix(predmstack, SSRTester$lc)

#Generate new predictions using the testing set
pred1V <- predict(my_rf,Testing)
pred2V <- predict(my_svm, Testing)
pred3V <- predict(my_nn, Testing)
predVDF <- data.frame(pred_rf = pred1V, pred_svm = pred2V, pred_nn = pred3V)
StackPred <- predict(modelstack1, predVDF)

#accuracy assessment
accuracy <- rbind(confusionMatrix(pred1V, Testing$lc)$overall[1], 
                  confusionMatrix(pred2V, Testing$lc)$overall[1],
                  confusionMatrix(pred3V, Testing$lc)$overall[1],
                  confusionMatrix(StackPred, Testing$lc)$overall[1])
row.names(accuracy) <- c("RF", "SVM", "NNET", "Stack")
accuracy

############################################################################################
#Write your image
beginCluster()
preds_rf <- clusterR(MyStack, raster::predict, args = list(model = my_rf))
preds_svm <- clusterR(MyStack, raster::predict, args = list(model = my_svm))
preds_nn <- clusterR(MyStack, raster::predict, args = list(model = my_nn))

preds <- stack(preds_rf, preds_svm, preds_nn)
names(preds) <- c("predRFss", "predsvmss", "predNNss")

pred_comb <- clusterR(preds, raster::predict, args = list(model = modelstack1))
endCluster()
