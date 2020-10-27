# Name: Mick Shaw
# Last updated: 7-3-2020


###############
# Project notes
###############

#WiFi location predictions


###############
# Housekeeping
###############

# Clear all variables from R
rm(list = ls())

# Set working directory
getwd()
setwd('F:/UT Data Analytics/Course 3 - Deep Analytics and Visualization/Task3 - WiFi/WiFi Locationing')
dir()


################################
## Install and load packages
################################

install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("forecast")
install.packages("TTR")
install.packages("RMySQL")
install.packages("plotly")
install.packages("ggfortify")
install.packages("ggplot2")
install.packages("reshape")
install.packages("naniar")
install.packages("plot3D")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("devtools")
install.packages("GGally")
install.packages("pacman")

library(pacman)

p_load(shiny, shinydashboard, dplyr, ggplot2, plotly, lubridate, naniar, devtools,
       corrplot, GGally, caret, reshape, doParallel, readr, mlbench, tidyverse, e1071, 
       kernlab, randomForest, gridExtra, caTools)


###############################
# --- Parallel processing --- #
###############################

detectCores()  # detect number of cores
cl <- makeCluster(2)  # select number of cores; 2 in this example
registerDoParallel(cl) # register cluster
getDoParWorkers()  # confirm number of cores being used by RStudio
# Stop Cluster. After performing your tasks, make sure to stop your cluster. 
stopCluster(cl)

######################## 
# --- Load dataset --- #  
########################

# --- Load Train/Existing data (Dataset 1) --- #
trainingData <- read.csv("trainingData.csv", stringsAsFactors = FALSE)
class(trainingData)  # "data.frame"
str(trainingData)

# --- Load Train/Existing data (Dataset 2) --- #
validationData <- read.csv("validationData.csv", stringsAsFactors = FALSE)
class(validationData)  # "data.frame"
str(validationData)
validationData

# --- Combined Data --- #
combinedData <- rbind(trainingData, validationData)


# ---Data Exploration--- #

## Inspect the Data Types
summary(trainingData[,520:529])
str(trainingData[,520:529])

summary(validationData[,520:529])
str(validationData[,520:529])

groupedData <- trainingData %>% 
  group_by(BUILDINGID, FLOOR, USERID, PHONEID) %>% 
  count()

write.csv(groupedData, file = "filteredTrainingData.csv")

groupedValidationData <-  validationData %>%
  group_by(BUILDINGID, FLOOR, USERID, PHONEID) %>% 
  count()

write.csv(groupedValidationData, file = "filteredValidationData.csv")

## Total Number of Observations in each Floor
BuildingFloorObs <- groupedData %>% 
  group_by((BUILDINGID)) %>% 
  ggplot(aes(FLOOR,n)) +
  geom_col(fill = "brown1") +
  labs(title = "Total Number of Observations in Each Building & Floor") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  ylab("Number of Observations") + 
  xlab("FLOOR") + 
  facet_wrap(~BUILDINGID)

BuildingFloorObs

## Building 0 Analysis
trainingData %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

trainingData %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)

## Building 1 Analysis
trainingData %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

trainingData %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)

## Building 2 Analysis
trainingData %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

trainingData %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)


### ---- Data Visualization ----
# What happens if we plot longitude to latitude?
plot(trainingData$LONGITUDE, trainingData$LATITUDE)
plot(validationData$LONGITUDE, validationData$LATITUDE)

# Longitude vs Latitude - training data
trainingData %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(color = "brown1") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(title = "Training Data Longitude vs Latitude")

# Longitude vs Latitude - validation data
validationData %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(color = "brown1") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(title = "Training Data Longitude vs Latitude")

# Longitude vs Latitude - combined data
combinedData %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(color = "brown1") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(title = "Training Data Longitude vs Latitude")


# Building 0 Preview
combinedData %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 0 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Building 1 Preview
combinedData %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 1 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Building 2 Preview
combinedData %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 2 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


## Users' Behaviors Visualization
trainingData %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 0",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

trainingData %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 1",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

trainingData %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 2",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


## Phone ID Patterns
trainingData %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(PHONEID)) %>%
  add_markers() %>%
  layout(title = "Phone ID Pattern Building 0",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

trainingData %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(PHONEID)) %>%
  add_markers() %>%
  layout(title = "Phone ID Pattern Building 1",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

trainingData %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(PHONEID)) %>%
  add_markers() %>%
  layout(title = "Phone ID Pattern Building 2",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


## SPACEID Locations

trainingData %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(SPACEID)) %>%
  add_markers() %>%
  layout(title = "Space IDs - Building 1",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))


# --- Preprocessing Data --- #


## Change Data Types
trainingDataV2 <- trainingData %>%
  mutate_at(c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID"), as.factor) %>%
  mutate_at(c("TIMESTAMP"), as.character) %>%
  mutate_at(c("TIMESTAMP"), as.numeric)
trainingDataV2    
summary(trainingDataV2)  
str(tail(trainingDataV2))
class(trainingDataV2)


validationDataV2 <- validationData %>%
  mutate_at(c("FLOOR", "BUILDINGID", "SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID"), as.factor) %>%
  mutate_at(c("TIMESTAMP"), as.character) %>%
  mutate_at(c("TIMESTAMP"), as.numeric)
validationDataV2    
summary(validationDataV2)  
str(tail(validationDataV2))

# - Seperate WAPS from remaining variables - #

WAPStrainingData <- trainingDataV2[,1:520]

varTrainingData <- trainingDataV2[, 521:529]

WAPSvalidationData <- validationDataV2[,1:520]

varValidationData <- validationDataV2[,521:529]


## Correlation Analysis for remaining variables
varTrainingDataCorr <- cor(trainingData[,521:529])
varTrainingDataCorr
corrplot(varTrainingDataCorr)

WAPStrainingDataCorr <- cor(trainingData[,1:520])
WAPStrainingDataCorr
corrplot(WAPStrainingDataCorr)

## Check interval of variance, mean and median of columns to eliminate 
wapsdata <- data.frame(variance = apply(WAPStrainingData, 2, var),
                       mean = apply(WAPStrainingData, 2, mean),
                       median = apply(WAPStrainingData, 2, median))
summary(wapsdata)

## Remove unregistered waps signals
weakWAPStrainCol <- apply(WAPStrainingData, 2, mean) >= 99.8
weakWAPStrainRow <- apply(WAPStrainingData, 1, mean) >= 99.8
#Decrease mean by decimal to increase signal quality
class(weakWAPStrainCol)

goodWAPStrain <- WAPStrainingData[!weakWAPStrainRow,!weakWAPStrainCol]
goodWAPStrain

weakWAPSvalidCol <- apply(WAPSvalidationData, 2, mean) >= 99.8
weakWAPSvalidRow <- apply(WAPSvalidationData, 1, mean) >= 99.8
#Decrease mean by decimal to increase signal quality

goodWAPSvalid <- WAPSvalidationData[!weakWAPSvalidRow,!weakWAPSvalidCol]
goodWAPSvalid


## Equalize row amount in Variable data sets
varTrainingData2 <- varTrainingData[!weakWAPStrainRow, ]
varValidationData2 <- varValidationData[!weakWAPSvalidRow, ]


## Combine WAPS and Variables Data
adjTrainingData <- cbind(goodWAPStrain, varTrainingData2)
adjValidationData <- cbind(goodWAPSvalid, varValidationData2)

## Feature Engineering ##
# Remove Variables
readyTraining <- subset(adjTrainingData, select = -c(USERID:TIMESTAMP))
readyValidation <- subset(adjValidationData, select = -c(USERID:TIMESTAMP))
summary(readyTraining)

## Save preprocessed data sets
write.csv(readyTraining, file = "readyTrainingData.csv")
write.csv(readyValidation, file = "readyValidationData.csv")



### --- Subsetting and Sampling --- ###
## Building 0 Subset
B0trainingData <- readyTraining %>%
  filter(BUILDINGID == 0) 
B0trainingData

## Building 1 Subset
B1trainingData <- readyTraining %>%
  filter(BUILDINGID == 1)
B1trainingData

## Building 2 Subset
B2trainingData <- readyTraining %>%
  filter(BUILDINGID == 2)
B2trainingData

## Building 0 Grouped Location Subset
groupB0trainingData <- B0trainingData %>%
  unite("LOCATION",367:370,sep="", remove=FALSE)
groupB0trainingData$LOCATION <- as.factor(groupB0trainingData$LOCATION)
groupB0trainingData$FLOOR <- NULL
groupB0trainingData$BUILDINGID <- NULL
groupB0trainingData$SPACEID <- NULL
groupB0trainingData$RELATIVEPOSITION <- NULL
groupB0trainingData$LATITUDE <- NULL
groupB0trainingData$LONGITUDE <- NULL
sapply(groupB0trainingData, class)

# - Seperate WAPS from remaining variables for Building 0 subset - #

WAPSgroupB0trainingData <- groupB0trainingData[,1:364]
WAPSgroupB0trainingData
class(WAPSgroupB0trainingData)
vargroupB0trainingData <- groupB0trainingData[,365]
vargroupB0trainingData <- as.data.frame(vargroupB0trainingData)
class(vargroupB0trainingData)

## Remove unregistered waps signals
weakWAPStrainColgroupB0 <- apply(WAPSgroupB0trainingData, 2, mean) >= 99.9
weakWAPStrainRowgroupB0 <- apply(WAPSgroupB0trainingData, 1, mean) >= 99.9
#Decrease mean by decimal to increase signal quality

goodWAPStrainB0 <- WAPSgroupB0trainingData[!weakWAPStrainRowgroupB0,!weakWAPStrainColgroupB0]
goodWAPStrainB0

## Equalize row amount in Variable data sets
vargroupB0trainingData2 <- vargroupB0trainingData[!weakWAPStrainRowgroupB0, ]

## Combine WAPS and Variables Data
adjTrainingDataB0 <- cbind(goodWAPStrainB0, vargroupB0trainingData2)
names(adjTrainingDataB0)[151]<-paste("LOCATION")
adjTrainingDataB0



## Building 1 Grouped Location Subset
groupB1trainingData <- B1trainingData %>%
  unite("LOCATION",367:370,sep="", remove=FALSE)
groupB1trainingData
groupB1trainingData$LOCATION <- as.factor(groupB1trainingData$LOCATION)
groupB1trainingData$FLOOR <- NULL
groupB1trainingData$BUILDINGID <- NULL
groupB1trainingData$SPACEID <- NULL
groupB1trainingData$RELATIVEPOSITION <- NULL
groupB1trainingData$LATITUDE <- NULL
groupB1trainingData$LONGITUDE <- NULL
sapply(groupB1trainingData, class)

# - Seperate WAPS from remaining variables for Building 1 subset - #

WAPSgroupB1trainingData <- groupB1trainingData[,1:364]
WAPSgroupB1trainingData
class(WAPSgroupB1trainingData)
vargroupB1trainingData <- groupB1trainingData[,365]
vargroupB1trainingData <- as.data.frame(vargroupB1trainingData)
class(vargroupB1trainingData)

## Remove unregistered waps signals
weakWAPStrainColgroupB1 <- apply(WAPSgroupB1trainingData, 2, mean) >= 99.9
weakWAPStrainRowgroupB1 <- apply(WAPSgroupB1trainingData, 1, mean) >= 99.9
#Decrease mean by decimal to increase signal quality

goodWAPStrainB1 <- WAPSgroupB1trainingData[!weakWAPStrainRowgroupB1,!weakWAPStrainColgroupB1]
goodWAPStrainB1

## Equalize row amount in Variable data sets
vargroupB1trainingData2 <- vargroupB1trainingData[!weakWAPStrainRowgroupB1, ]

## Combine WAPS and Variables Data
adjTrainingDataB1 <- cbind(goodWAPStrainB1, vargroupB1trainingData2)
names(adjTrainingDataB1)[189]<-paste("LOCATION")
adjTrainingDataB1


## Building 2 Grouped Location Subset
groupB2trainingData <- B2trainingData %>%
  unite("LOCATION",367:370,sep="", remove=FALSE)
groupB2trainingData
groupB2trainingData$LOCATION <- as.factor(groupB2trainingData$LOCATION)
groupB2trainingData$FLOOR <- NULL
groupB2trainingData$BUILDINGID <- NULL
groupB2trainingData$SPACEID <- NULL
groupB2trainingData$RELATIVEPOSITION <- NULL
groupB2trainingData$LATITUDE <- NULL
groupB2trainingData$LONGITUDE <- NULL
sapply(groupB2trainingData, class)

# - Seperate WAPS from remaining variables for Building 2 subset - #

WAPSgroupB2trainingData <- groupB2trainingData[,1:364]
WAPSgroupB2trainingData
class(WAPSgroupB2trainingData)
vargroupB2trainingData <- groupB2trainingData[,365]
vargroupB2trainingData <- as.data.frame(vargroupB2trainingData)
class(vargroupB2trainingData)

## Remove unregistered waps signals
weakWAPStrainColgroupB2 <- apply(WAPSgroupB2trainingData, 2, mean) >= 99.9
weakWAPStrainRowgroupB2 <- apply(WAPSgroupB2trainingData, 1, mean) >= 99.9
#Decrease mean by decimal to increase signal quality

goodWAPStrainB2 <- WAPSgroupB2trainingData[!weakWAPStrainRowgroupB2,!weakWAPStrainColgroupB2]
goodWAPStrainB2

## Equalize row amount in Variable data sets
vargroupB2trainingData2 <- vargroupB2trainingData[!weakWAPStrainRowgroupB2, ]

## Combine WAPS and Variables Data
adjTrainingDataB2 <- cbind(goodWAPStrainB2, vargroupB2trainingData2)
names(adjTrainingDataB2)[162]<-paste("LOCATION")
adjTrainingDataB2


### --- Train/Test Sets --- ###


seed <- 123



## Building 0 Train/Test LOCATION
set.seed(seed)
inTrainingB0LOC <- createDataPartition(adjTrainingDataB0$LOCATION, p=0.75, list=FALSE)
trainSetB0LOC <- adjTrainingDataB0[inTrainingB0LOC,]   
testSetB0LOC <- adjTrainingDataB0[-inTrainingB0LOC,]  
trainSetB0LOC



## Building 1 Train/Test LOCATION
set.seed(seed)
inTrainingB1LOC <- createDataPartition(adjTrainingDataB1$LOCATION, p=0.75, list=FALSE)
trainSetB1LOC <- adjTrainingDataB1[inTrainingB1LOC,]   
testSetB1LOC <- adjTrainingDataB1[-inTrainingB1LOC,]  
trainSetB1LOC



## Building 2 Train/Test LOCATION
set.seed(seed)
inTrainingB2LOC <- createDataPartition(adjTrainingDataB2$LOCATION, p=0.75, list=FALSE)
trainSetB2LOC <- adjTrainingDataB2[inTrainingB2LOC,]   
testSetB2LOC <- adjTrainingDataB2[-inTrainingB2LOC,]  
trainSetB2LOC


### --- KNN Models --- ###

## Train control

# set 10 fold cross validation
knnControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)



### --- Classifier Models --- ###
## Building 0 Location Model
set.seed(seed)
KNNfitB0LOCr3 <- train(LOCATION~., data=trainSetB0LOC, method="knn", trControl=knnControl)
KNNfitB0LOCr3
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 3592, 3599, 3599, 3593, 3599, 3598, ... 
# Resampling results across tuning parameters:
#   
#   k  Accuracy   Kappa    
# 5  0.5469649  0.5450367
# 7  0.5368972  0.5349364
# 9  0.5041284  0.5020434
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 5.


saveRDS(KNNfitB0LOCr3, file = "KNNfitB0LOCr3.rds")
KNNfitB0LOCr3 <- readRDS("KNNfitB0LOCr3.rds")

knnPredB0LOCr3 <- predict(KNNfitB0LOCr3, testSetB0LOC)
knnPredB0LOCr3

postResample(knnPredB0LOCr3, testSetB0LOC$LOCATION)
# Accuracy     Kappa 
# 0.5578611 0.5560667 

confusionMatrix(data = knnPredB0LOCr3, reference = testSetB0LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.5579          
# 95% CI : (0.5299, 0.5856)
# No Information Rate : 0.008           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5561          
# 
# Mcnemar's Test P-Value : NA            

## Building 1 Location Model
set.seed(seed)
KNNfitB1LOCr3 <- train(LOCATION~., data=trainSetB1LOC, method="knn", trControl=knnControl)
KNNfitB1LOCr3
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 3548, 3551, 3537, 3536, 3555, 3544, ... 
# Resampling results across tuning parameters:
#   
#   k  Accuracy   Kappa    
# 5  0.6361240  0.6339186
# 7  0.6119696  0.6095994
# 9  0.6022779  0.5998275
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 5.



saveRDS(KNNfitB1LOCr3, file = "KNNfitB1LOCr3.rds")
KNNfitB1LOCr3 <- readRDS("KNNfitB1LOCr3.rds")

knnPredB1LOCr3 <- predict(KNNfitB1LOCr3, testSetB1LOC)
knnPredB1LOCr3

postResample(knnPredB1LOCr3, testSetB1LOC$LOCATION)
# Accuracy     Kappa 
# 0.6688525 0.6668007 



confusionMatrix(data = knnPredB1LOCr3, reference = testSetB1LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.6689          
# 95% CI : (0.6417, 0.6952)
# No Information Rate : 0.018           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6668          
# 
# Mcnemar's Test P-Value : NA            



## Building 2 Location Model
set.seed(seed)
KNNfitB2LOCr3 <- train(LOCATION~., data=trainSetB2LOC, method="knn", trControl=knnControl)
KNNfitB2LOCr3
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 6482, 6480, 6463, 6485, 6485, 6467, ... 
# Resampling results across tuning parameters:
#   
#   k  Accuracy   Kappa    
# 5  0.6148794  0.6136490
# 7  0.6034101  0.6021332
# 9  0.5784587  0.5770945
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was k = 5.





saveRDS(KNNfitB2LOCr3, file = "KNNfitB2LOCr3.rds")
KNNfitB2LOCr3<- readRDS("KNNfitB2LOCr3.rds")


knnPredB2LOCr3 <- predict(KNNfitB2LOCr3, testSetB2LOC)
knnPredB2LOCr3

postResample(knnPredB2LOCr3, testSetB2LOC$LOCATION)
# Accuracy     Kappa 
# 0.6241135 0.6229376 


confusionMatrix(data = knnPredB2LOCr3, reference = testSetB2LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.6241          
# 95% CI : (0.6038, 0.6441)
# No Information Rate : 0.0075          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6229          
# 
# Mcnemar's Test P-Value : NA    





### --- RF Models --- ###

## Train control

# set 10 fold cross validation
rfControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)




### --- Classifier Models --- ###
## Building 0 Location Model RF
set.seed(seed)
RFfitB0LOCr3 <- train(LOCATION~., data=trainSetB0LOC, method="rf", trControl=rfControl)
RFfitB0LOCr3

# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 3592, 3599, 3599, 3593, 3599, 3598, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2   0.4315290  0.4286191
# 76   0.7572192  0.7561663
# 150   0.7479813  0.7468919
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 76.







saveRDS(RFfitB0LOCr3, file = "RFfitB0LOCr3.rds")
RFfitB0LOCr3 <- readRDS("RFfitB0LOCr3.rds")

rfPredB0LOCr3 <- predict(RFfitB0LOCr3, testSetB0LOC)
rfPredB0LOCr3

postResample(rfPredB0LOCr3, testSetB0LOC$LOCATION)
# Accuracy     Kappa 
# 0.7733440 0.7724199 


rfB0cm <- confusionMatrix(data = rfPredB0LOCr3, reference = testSetB0LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.7733          
# 95% CI : (0.7491, 0.7963)
# No Information Rate : 0.008           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7724          
# 
# Mcnemar's Test P-Value : NA         

summary(rfB0cm)
rfB0cm$overall
rfB0cm$table
rfB0cm$byClass
rfB0cmOVER <- rfB0cm$overall
rfB0cmOVER
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue 
# 0.773343974    0.772419889    0.749136633    0.796261442    0.007980846    0.000000000 
rfB0cmCLASS <- rfB0cm$byClass
class(rfB0cmCLASS)
head(rfB0cmCLASS)
# Sensitivity Specificity Pos Pred Value Neg Pred Value Precision Recall   F1
# Class: 001022         0.6   1.0000000      1.0000000      0.9984000 1.0000000    0.6 0.75
# Class: 001062         1.0   0.9992006      0.6666667      1.0000000 0.6666667    1.0 0.80
# Class: 001072         0.0   1.0000000            NaN      0.9984038        NA    0.0   NA
# Class: 001102         0.5   0.9992006      0.5000000      0.9992006 0.5000000    0.5 0.50
# Class: 001112         0.6   0.9983974      0.6000000      0.9983974 0.6000000    0.6 0.60
# Class: 001122         1.0   1.0000000      1.0000000      1.0000000 1.0000000    1.0 1.00
# Prevalence Detection Rate Detection Prevalence Balanced Accuracy
# Class: 001022 0.003990423   0.0023942538          0.002394254         0.8000000
# Class: 001062 0.001596169   0.0015961692          0.002394254         0.9996003
# Class: 001072 0.001596169   0.0000000000          0.000000000         0.5000000
# Class: 001102 0.001596169   0.0007980846          0.001596169         0.7496003
# Class: 001112 0.003990423   0.0023942538          0.003990423         0.7991987
# Class: 001122 0.003990423   0.0039904230          0.003990423         1.0000000
rfB0cmCLASSdf <-  as.data.frame(rfB0cmCLASS)
plot(rfB0cmCLASSdf$Sensitivity,rfB0cmCLASSdf$Specificity)

rfB0cmDF <- as.data.frame(rfB0cm$table)
rfB0cmDF

matplot(rfB0cm$byClass)

rfVarfitB0LOCr3 <- varImp(RFfitB0LOCr3)
rfVarfitB0LOCr3
# Overall
# WAP051  100.00
# WAP052   90.42
# WAP156   80.64
# WAP161   76.50
# WAP155   74.14
# WAP162   72.42
# WAP013   71.24
# WAP014   69.97
# WAP036   68.42
# WAP224   65.42
# WAP030   65.04
# WAP035   63.83
# WAP042   62.45
# WAP143   59.30
# WAP041   59.10
# WAP039   58.82
# WAP029   58.41
# WAP142   56.88
# WAP225   55.27
# WAP034   55.21





set.seed(seed)
RFfitB1LOCr3 <- train(LOCATION~., data=trainSetB1LOC, method="rf", trControl=rfControl)
RFfitB1LOCr3
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 3548, 3551, 3537, 3536, 3555, 3544, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2   0.2495971  0.2412847
# 95   0.8481456  0.8472069
# 188   0.8354868  0.8344707
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 95.




saveRDS(RFfitB1LOCr3, file = "RFfitB1LOCr3.rds")
RFfitB1LOCr3 <- readRDS("RFfitB1LOCr3.rds")

rfPredB1LOCr3 <- predict(RFfitB1LOCr3, testSetB1LOC)
rfPredB1LOCr3

postResample(rfPredB1LOCr3, testSetB1LOC$LOCATION)
# Accuracy     Kappa 
# 0.8524590 0.8515155



rfB1cm <- confusionMatrix(data = rfPredB1LOCr3, reference = testSetB1LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.8525          
# 95% CI : (0.8313, 0.8719)
# No Information Rate : 0.018           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8515          
# 
# Mcnemar's Test P-Value : NA    

summary(rfB1cm)
rfB1cm$overall
rfB1cm$table
rfB1cm$byClass
rfB1cmOVER <- rfB1cm$overall
rfB1cmOVER
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 0.85245902     0.85151548     0.83130134     0.87190123     0.01803279     0.00000000            NaN 
rfB1cmCLASS <- rfB1cm$byClass
class(rfB1cmCLASS)
rfB1cmCLASS
head(rfB1cmCLASS)
# Sensitivity Specificity Pos Pred Value Neg Pred Value Precision Recall        F1  Prevalence
# Class: 01101          0.4   1.0000000           1.00      0.9975369      1.00    0.4 0.5714286 0.004098361
# Class: 01102          1.0   1.0000000           1.00      1.0000000      1.00    1.0 1.0000000 0.003278689
# Class: 011072         1.0   1.0000000           1.00      1.0000000      1.00    1.0 1.0000000 0.004098361
# Class: 011082         1.0   0.9991776           0.80      1.0000000      0.80    1.0 0.8888889 0.003278689
# Class: 0111           1.0   1.0000000           1.00      1.0000000      1.00    1.0 1.0000000 0.001639344
# Class: 011102         0.6   0.9991770           0.75      0.9983553      0.75    0.6 0.6666667 0.004098361
# Detection Rate Detection Prevalence Balanced Accuracy
# Class: 01101     0.001639344          0.001639344         0.7000000
# Class: 01102     0.003278689          0.003278689         1.0000000
# Class: 011072    0.004098361          0.004098361         1.0000000
# Class: 011082    0.003278689          0.004098361         0.9995888
# Class: 0111      0.001639344          0.001639344         1.0000000
# Class: 011102    0.002459016          0.003278689         0.7995885
rfB1cmCLASSdf <-  as.data.frame(rfB1cmCLASS)
plot(rfB1cmCLASSdf$Sensitivity,rfB1cmCLASSdf$Specificity)

rfB1cmDF <- as.data.frame(rfB1cm$table)
rfB1cmDF

rfVarfitB1LOCr3 <- varImp(RFfitB1LOCr3)
rfVarfitB1LOCr3
# Overall
# WAP108  100.00
# WAP140   98.10
# WAP222   96.77
# WAP107   92.74
# WAP223   85.22
# WAP173   82.74
# WAP114   80.71
# WAP248   80.12
# WAP141   78.05
# WAP167   77.61
# WAP166   77.41
# WAP008   76.97
# WAP015   75.00
# WAP478   69.72
# WAP176   67.21
# WAP147   67.05
# WAP146   66.81
# WAP102   66.77
# WAP119   65.11
# WAP016   64.04




## Building 2 Location Model RF
set.seed(seed)
RFfitB2LOCr3 <- train(LOCATION~., data=trainSetB2LOC, method="rf", trControl=rfControl)
RFfitB2LOCr3
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 6482, 6480, 6463, 6485, 6485, 6467, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 2   0.3656132  0.3626377
# 81   0.8078390  0.8072229
# 161   0.7765020  0.7757871
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 81.






saveRDS(RFfitB2LOCr3, file = "RFfitB2LOCr3.rds")
RFfitB2LOCr3 <- readRDS("RFfitB2LOCr3.rds")

rfPredB2LOCr3 <- predict(RFfitB2LOCr3, testSetB2LOC)
rfPredB2LOCr3

postResample(rfPredB2LOCr3, testSetB2LOC$LOCATION)
# Accuracy     Kappa 
# 0.8071809 0.8065729 


rfB2cm <- confusionMatrix(data = rfPredB2LOCr3, reference = testSetB2LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.8072          
# 95% CI : (0.7903, 0.8233)
# No Information Rate : 0.0075          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8066          
# 
# Mcnemar's Test P-Value : NA         

## --- Confusion Matrix Visual -- ##
summary(rfB2cm)
rfB2cm$overall
rfB2cm$table
rfB2cm$byClass
rfB2cmOVER <- rfB2cm$overall
rfB2cmOVER
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue 
# 0.807180851    0.806572887    0.790285071    0.823274824    0.007535461    0.000000000            NaN 
rfB2cmCLASS <- rfB2cm$byClass
class(rfB2cmCLASS)
rfB2cmCLASS
head(rfB2cmCLASS)
# Sensitivity Specificity Pos Pred Value Neg Pred Value Precision    Recall        F1  Prevalence
# Class: 021012   0.4285714   0.9991107      0.6000000      0.9982230 0.6000000 0.4285714 0.5000000 0.003102837
# Class: 021031   1.0000000   0.9995556      0.8571429      1.0000000 0.8571429 1.0000000 0.9230769 0.002659574
# Class: 021032   0.7142857   0.9991107      0.7142857      0.9991107 0.7142857 0.7142857 0.7142857 0.003102837
# Class: 021041   0.4000000   0.9995558      0.6666667      0.9986684 0.6666667 0.4000000 0.5000000 0.002216312
# Class: 021042   0.8000000   0.9982151      0.7500000      0.9986607 0.7500000 0.8000000 0.7741935 0.006648936
# Class: 021062   1.0000000   1.0000000      1.0000000      1.0000000 1.0000000 1.0000000 1.0000000 0.003102837
# Detection Rate Detection Prevalence Balanced Accuracy
# Class: 021012   0.0013297872          0.002216312         0.7138411
# Class: 021031   0.0026595745          0.003102837         0.9997778
# Class: 021032   0.0022163121          0.003102837         0.8566982
# Class: 021041   0.0008865248          0.001329787         0.6997779
# Class: 021042   0.0053191489          0.007092199         0.8991075
# Class: 021062   0.0031028369          0.003102837         1.0000000
rfB2cmCLASSdf <-  as.data.frame(rfB2cmCLASS)
plot(rfB2cmCLASSdf$Sensitivity,rfB2cmCLASSdf$Specificity)
rfB2cmCLASSdf
rfB2cmDF <- as.data.frame(rfB2cm$table)
rfB2cmDF

# draw_confusion_matrixB2 <- function(rfB2cm) {
# 
#   layout(matrix(c(1,1,2)))
#   par(mar=c(2,2,2,2))
#   plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
#   title('CONFUSION MATRIX', cex.main=2)
# 
#   # create the matrix
#   rect(150, 430, 240, 370, col='#3F97D0')
#   text(195, 435, '0', cex=1.2)
#   rect(250, 430, 340, 370, col='#F7AD50')
#   text(295, 435, '1', cex=1.2)
#   text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
#   text(245, 450, 'Actual', cex=1.3, font=2)
#   rect(150, 305, 240, 365, col='#F7AD50')
#   rect(250, 305, 340, 365, col='#3F97D0')
#   text(140, 400, '0', cex=1.2, srt=90)
#   text(140, 335, '1', cex=1.2, srt=90)
# 
#   # add in the cm results
#   #res <- as.numeric(rfB2cm$table)
#   res <- rfB2cm$table
#   text(195, 400, res[1], cex=1.6, font=2, col='white')
#   text(195, 335, res[2], cex=1.6, font=2, col='white')
#   text(295, 400, res[3], cex=1.6, font=2, col='white')
#   text(295, 335, res[4], cex=1.6, font=2, col='white')
# 
#   # add in the specifics
#   plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
#   text(5, 85, names(rfB2cm$byClass[1]), cex=1.4, font=2)
#   text(5, 70, round(as.numeric(rfB2cm$byClass[1]), 5), cex=1.4)
#   text(15, 85, names(rfB2cm$byClass[2]), cex=1.4, font=2)
#   text(15, 70, round(as.numeric(rfB2cm$byClass[2]), 5), cex=1.4)
#   text(25, 85, names(rfB2cm$byClass[3]), cex=1.4, font=2)
#   text(25, 70, round(as.numeric(rfB2cm$byClass[3]), 5), cex=1.4)
#   text(35, 85, names(rfB2cm$byClass[4]), cex=1.4, font=2)
#   text(35, 70, round(as.numeric(rfB2cm$byClass[4]), 5), cex=1.4)
#   text(45, 85, names(rfB2cm$byClass[5]), cex=1.4, font=2)
#   text(45, 70, round(as.numeric(rfB2cm$byClass[5]), 5), cex=1.4)
#   text(55, 85, names(rfB2cm$byClass[6]), cex=1.4, font=2)
#   text(55, 70, round(as.numeric(rfB2cm$byClass[6]), 5), cex=1.4)
#   text(65, 85, names(rfB2cm$byClass[7]), cex=1.4, font=2)
#   text(65, 70, round(as.numeric(rfB2cm$byClass[7]), 5), cex=1.4)
#   text(75, 85, names(rfB2cm$byClass[8]), cex=1.4, font=2)
#   text(75, 70, round(as.numeric(rfB2cm$byClass[8]), 5), cex=1.4)
#   text(85, 85, names(rfB2cm$byClass[9]), cex=1.4, font=2)
#   text(85, 70, round(as.numeric(rfB2cm$byClass[9]), 5), cex=1.4)
#   text(95, 85, names(rfB2cm$byClass[10]), cex=1.4, font=2)
#   text(95, 70, round(as.numeric(rfB2cm$byClass[10]), 5), cex=1.4)
# 
#   # add in the accuracy information
#   text(30, 35, names(rfB2cm$overall[1]), cex=1.5, font=2)
#   text(30, 20, round(as.numeric(rfB2cm$overall[1]), 3), cex=1.4)
#   text(70, 35, names(rfB2cm$overall[2]), cex=1.5, font=2)
#   text(70, 20, round(as.numeric(rfB2cm$overall[2]), 3), cex=1.4)
# }
# 
# draw_confusion_matrixB2(rfB2cm)

##

# draw_confusion_matrix <- function(rfB2cm) {
#   
#   total <- sum(rfB2cm$table)
#   res <- as.numeric(rfB2cm$table)
#   
#   # Generate color gradients. Palettes come from RColorBrewer.
#   greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
#   redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
#   getColor <- function (greenOrRed = "green", amount = 0) {
#     if (amount == 0)
#       return("#FFFFFF")
#     palette <- greenPalette
#     if (greenOrRed == "red")
#       palette <- redPalette
#     colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
#   }
#   
#   # set the basic layout
#   layout(matrix(c(1,1,2)))
#   par(mar=c(2,2,2,2))
#   plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
#   title('CONFUSION MATRIX', cex.main=2)
#   
#   # create the matrix 
#   classes = colnames(rfB2cm$table)
#   rect(150, 430, 240, 370, col=getColor("green", res[1]))
#   text(195, 435, classes[1], cex=1.2)
#   rect(250, 430, 340, 370, col=getColor("red", res[3]))
#   text(295, 435, classes[2], cex=1.2)
#   text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
#   text(245, 450, 'Actual', cex=1.3, font=2)
#   rect(150, 305, 240, 365, col=getColor("red", res[2]))
#   rect(250, 305, 340, 365, col=getColor("green", res[4]))
#   text(140, 400, classes[1], cex=1.2, srt=90)
#   text(140, 335, classes[2], cex=1.2, srt=90)
#   
#   # add in the cm results
#   text(195, 400, res[1], cex=1.6, font=2, col='white')
#   text(195, 335, res[2], cex=1.6, font=2, col='white')
#   text(295, 400, res[3], cex=1.6, font=2, col='white')
#   text(295, 335, res[4], cex=1.6, font=2, col='white')
#   
#   # add in the specifics 
#   plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
#   text(10, 85, names(rfB2cm$byClass[1]), cex=1.2, font=2)
#   text(10, 70, round(as.numeric(rfB2cm$byClass[1]), 3), cex=1.2)
#   text(30, 85, names(rfB2cm$byClass[2]), cex=1.2, font=2)
#   text(30, 70, round(as.numeric(rfB2cm$byClass[2]), 3), cex=1.2)
#   text(50, 85, names(rfB2cm$byClass[5]), cex=1.2, font=2)
#   text(50, 70, round(as.numeric(rfB2cm$byClass[5]), 3), cex=1.2)
#   text(70, 85, names(rfB2cm$byClass[6]), cex=1.2, font=2)
#   text(70, 70, round(as.numeric(rfB2cm$byClass[6]), 3), cex=1.2)
#   text(90, 85, names(rfB2cm$byClass[7]), cex=1.2, font=2)
#   text(90, 70, round(as.numeric(rfB2cm$byClass[7]), 3), cex=1.2)
#   
#   # add in the accuracy information 
#   text(30, 35, names(rfB2cm$overall[1]), cex=1.5, font=2)
#   text(30, 20, round(as.numeric(rfB2cm$overall[1]), 3), cex=1.4)
#   text(70, 35, names(rfB2cm$overall[2]), cex=1.5, font=2)
#   text(70, 20, round(as.numeric(rfB2cm$overall[2]), 3), cex=1.4)
# }


rfVarfitB2LOCr3 <- varImp(RFfitB2LOCr3)
rfVarfitB2LOCr3
# Overall
# WAP501  100.00
# WAP496   91.14
# WAP502   84.89
# WAP012   80.66
# WAP065   80.45
# WAP099   80.34
# WAP122   79.05
# WAP066   78.57
# WAP131   77.04
# WAP121   76.22
# WAP118   74.01
# WAP087   72.29
# WAP062   71.64
# WAP132   71.42
# WAP098   70.15
# WAP495   69.51
# WAP070   69.32
# WAP061   68.03
# WAP096   68.01
# WAP097   68.00



## Building 0 Location Model C5.0
c5.0Control <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


## Building 0 Location Model
set.seed(seed)
c5.0fitB0LOCr3 <- train(LOCATION~., data=trainSetB0LOC, method="C5.0", trControl=c5.0Control,importancetrControl=c5.0Control)
c5.0fitB0LOCr3 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 3592, 3599, 3599, 3593, 3599, 3598, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.5704029  0.5685821
# rules  FALSE   10      0.6812673  0.6798975
# rules  FALSE   20      0.6981067  0.6968076
# rules   TRUE    1      0.5673956  0.5655597
# rules   TRUE   10      0.6686139  0.6671945
# rules   TRUE   20      0.6943825  0.6930720
# tree   FALSE    1      0.5801755  0.5783872
# tree   FALSE   10      0.6755546  0.6741642
# tree   FALSE   20      0.6878574  0.6865150
# tree    TRUE    1      0.5821945  0.5804155
# tree    TRUE   10      0.6758747  0.6744851
# tree    TRUE   20      0.6984084  0.6971140
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 20, model = tree and winnow = TRUE.


saveRDS(c5.0fitB0LOCr3, file = "c5.0fitB0LOCr3.rds")
c5.0fitB0LOCr3 <- readRDS("c5.0fitB0LOCr3.rds")

c5.0PredB0LOCr3 <- predict(c5.0fitB0LOCr3, testSetB0LOC)
c5.0PredB0LOCr3




postResample(c5.0PredB0LOCr3, testSetB0LOC$LOCATION)
# Accuracy     Kappa 
# 0.7150838 0.7139204


confusionMatrix(data = c5.0PredB0LOCr3, reference =  testSetB0LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.7151          
# 95% CI : (0.6892, 0.7399)
# No Information Rate : 0.008           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7139          
# 
# Mcnemar's Test P-Value : NA         






## Building 1 Location Model
set.seed(seed)
c5.0fitB1LOCr3 <- train(LOCATION~., data=trainSetB1LOC, method="C5.0",trControl=c5.0Control, importancetrControl=c5.0Control)
c5.0fitB1LOCr3 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 3548, 3551, 3537, 3536, 3555, 3544, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.6893285  0.6874302
# rules  FALSE   10      0.7866239  0.7853156
# rules  FALSE   20      0.7977708  0.7965323
# rules   TRUE    1      0.6854511  0.6835221
# rules   TRUE   10      0.7922808  0.7910045
# rules   TRUE   20      0.7912364  0.7899523
# tree   FALSE    1      0.6889950  0.6870995
# tree   FALSE   10      0.7800496  0.7786795
# tree   FALSE   20      0.7904896  0.7891877
# tree    TRUE    1      0.6889112  0.6870183
# tree    TRUE   10      0.7774834  0.7760962
# tree    TRUE   20      0.7934668  0.7921761
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 20, model = rules and winnow = FALSE.





saveRDS(c5.0fitB1LOCr3, file = "c5.0fitB1LOCr3.rds")
c5.0fitB1LOCr3 <- readRDS("c5.0fitB1LOCr3.rds")

c5.0PredB1LOCr3 <- predict(c5.0fitB1LOCr3, testSetB1LOC)
c5.0PredB1LOCr3

postResample(c5.0PredB1LOCr3, testSetB1LOC$LOCATION)
# Accuracy     Kappa 
# 0.8032787 0.8020643 

confusionMatrix(data = c5.0PredB1LOCr3, reference =  testSetB1LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.8033          
# 95% CI : (0.7798, 0.8252)
# No Information Rate : 0.018           
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8021          
# 
# Mcnemar's Test P-Value : NA              



## Building 2 Location Model
set.seed(seed)
c5.0fitB2LOCr3 <- train(LOCATION~., data=trainSetB2LOC, method="C5.0", trControl=c5.0Control, importancetrControl=c5.0Control)
c5.0fitB2LOCr3 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 1 times) 
# Summary of sample sizes: 6482, 6480, 6463, 6485, 6485, 6467, ... 
# Resampling results across tuning parameters:
#   
#   model  winnow  trials  Accuracy   Kappa    
# rules  FALSE    1      0.5693060  0.5679310
# rules  FALSE   10      0.7007312  0.6997771
# rules  FALSE   20      0.7196209  0.7187317
# rules   TRUE    1      0.5666329  0.5652566
# rules   TRUE   10      0.7014232  0.7004783
# rules   TRUE   20      0.7176396  0.7167498
# tree   FALSE    1      0.5817150  0.5804108
# tree   FALSE   10      0.6947786  0.6938066
# tree   FALSE   20      0.7119889  0.7110741
# tree    TRUE    1      0.5770759  0.5757564
# tree    TRUE   10      0.6977816  0.6968247
# tree    TRUE   20      0.7089041  0.7079833
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were trials = 20, model = rules and winnow = FALSE.






c5.0PredB2LOCr3 <- predict(c5.0fitB2LOCr3, testSetB2LOC)
c5.0PredB2LOCr3

saveRDS(c5.0fitB2LOCr3, file = "c5.0fitB2LOCr3.rds")
c5.0fitB2LOCr3 <- readRDS("c5.0fitB2LOCr3.rds")

postResample(c5.0PredB2LOCr3, testSetB2LOC$LOCATION)
# Accuracy     Kappa 
# 0.7402482 0.7394342 

confusionMatrix(data = c5.0PredB2LOCr3, reference =  testSetB2LOC$LOCATION)
# Overall Statistics
# 
# Accuracy : 0.7402          
# 95% CI : (0.7216, 0.7582)
# No Information Rate : 0.0075          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.7394          
# 
# Mcnemar's Test P-Value : NA           




### --- Model Data --- ###
ModelDataB0r3 <- resamples(list(knnB0LOCr3 = KNNfitB0LOCr3, rfB0LOCr3 = RFfitB0LOCr3, c5.0B0LOCr3 = c5.0fitB0LOCr3))
ModelDataB0r3
class(ModelDataB0r3)
summary(ModelDataB0r3)

# Models: knnB0LOCr3, rfB0LOCr3, c5.0B0LOCr3 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# knnB0LOCr3  0.4938272 0.5340909 0.5469208 0.5469649 0.5682487 0.5894207    0
# rfB0LOCr3   0.7078086 0.7472977 0.7617928 0.7572192 0.7715171 0.7934509    0
# c5.0B0LOCr3 0.6750630 0.6884259 0.6967436 0.6984084 0.7114024 0.7171216    0
# 
# Kappa 
#                  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# knnB0LOCr3  0.4916920 0.5320925 0.5449707 0.5450367 0.5664360 0.5876521    0
# rfB0LOCr3   0.7065630 0.7461954 0.7607611 0.7561663 0.7705318 0.7925479    0
# c5.0B0LOCr3 0.6736716 0.6871148 0.6954281 0.6971140 0.7101559 0.7158953    0





ModelDataB1r3 <- resamples(list(knnB1LOCr3 = KNNfitB1LOCr3, rfB1LOCr3 = RFfitB1LOCr3, c5.0B1LOCr3 = c5.0fitB1LOCr3))
ModelDataB1r3
summary(ModelDataB1r3)

# Models: knnB1LOCr3, rfB1LOCr3, c5.0B1LOCr3 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# knnB1LOCr3  0.5930521 0.6243308 0.6381074 0.6361240 0.6527441 0.6588542    0
# rfB1LOCr3   0.8209719 0.8405127 0.8464507 0.8481456 0.8621462 0.8756345    0
# c5.0B1LOCr3 0.7578125 0.7783670 0.7931856 0.7977708 0.8121791 0.8500000    0
# 
# Kappa 
#                  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# knnB1LOCr3  0.5906247 0.6220063 0.6358517 0.6339186 0.6506679 0.6567475    0
# rfB1LOCr3   0.8198690 0.8395185 0.8454780 0.8472069 0.8613155 0.8748444    0
# c5.0B1LOCr3 0.7562986 0.7770297 0.7919357 0.7965323 0.8110176 0.8490832    0





ModelDataB2r3 <- resamples(list(knnB2LOCr3 = KNNfitB2LOCr3, rfB2LOCr3 = RFfitB2LOCr3, c5.0B2LOCr3 = c5.0fitB2LOCr3))
ModelDataB2r3
summary(ModelDataB2r3)

# Models: knnB2LOCr3, rfB2LOCr3, c5.0B2LOCr3 
# Number of resamples: 10 
# 
# Accuracy 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# knnB2LOCr3  0.5842541 0.5997698 0.6150888 0.6148794 0.6242078 0.6477273    0
# rfB2LOCr3   0.7809524 0.8039972 0.8102371 0.8078390 0.8162079 0.8338068    0
# c5.0B2LOCr3 0.6878453 0.7174935 0.7215979 0.7196209 0.7274951 0.7400568    0
# 
# Kappa 
#                  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
# knnB2LOCr3  0.5829485 0.5985049 0.6138784 0.6136490 0.6230041 0.6465821    0
# rfB2LOCr3   0.7802474 0.8033689 0.8096296 0.8072229 0.8156344 0.8332659    0
# c5.0B2LOCr3 0.6868530 0.7166053 0.7207188 0.7187317 0.7266235 0.7392297    0
