#Loading the required packages
library(tidyverse) # metapackage with lots of helpful functions
library(data.table)
library(dplyr)
# Load packages required for random forest:
library(randomForest)
train <- read.csv("train.csv")
#Join State, Breed and Colors
color <- read.csv('color_labels.csv')
state <- read.csv('state_labels.csv')
breed <- read.csv('breed_labels.csv')

#Classifying Breeds
train <- mutate(train, PureBreed = ifelse(Breed1 == 0 | Breed2 == 0, "Pure Breed", "Not Pure"))

# #Joining Breed to train data
# colnames(breed)[1] <- 'Breed1'
# train <- left_join(train,breed, by = c('Breed1'))
# train <- setDT(train)[,-c('Breed1','Type.y')]
# colnames(train)[ncol(train)] <- 'BreedName1'
# train$BreedName1[is.na(train$BreedName1)] <- 'None'
# 
# colnames(breed)[1] <- 'Breed2'
# train <- left_join(train,breed, by = c('Breed2'))
# train <- setDT(train)[,-c('Breed2','Type')]
# colnames(train)[ncol(train)] <- 'BreedName2'
# train$BreedName2[is.na(train$BreedName2)] <- 'None'
# 
# names(train)[1]<-"Type"
# 
# #Color
# colnames(color)[1] <- 'Color1'
# train <- left_join(train,color, by = c('Color1'))
# train <- setDT(train)[,-c('Color1')]
# colnames(train)[ncol(train)] <- 'ColorType1'
# train$ColorType1[is.na(train$ColorType1)] <- 'None'
# 
# colnames(color)[1] <- 'Color2'
# train <- left_join(train,color, by = c('Color2'))
# train <- setDT(train)[,-c('Color2')]
# colnames(train)[ncol(train)] <- 'ColorType2'
# train$ColorType2[is.na(train$ColorType2)] <- 'None'
# 
# colnames(color)[1] <- 'Color3'
# train <- left_join(train,color, by = c('Color3'))
# train <- setDT(train)[,-c('Color3')]
# colnames(train)[ncol(train)] <- 'ColorType3'
# train$ColorType3[is.na(train$ColorType3)] <- 'None'
# 
train <- train %>% left_join(state, by = c("State" = "StateID"))

head(train)

#Extract the length of Description
library(stringr)
train$nlen_Desc<-str_length(train$Description)

#Model Building
#Build a random forest model
train$AdoptionSpeed <- as.factor(train$AdoptionSpeed)
train$PureBreed <- as.factor(train$PureBreed)
train$StateName <- as.factor(train$StateName)
summary(train)
rf<- randomForest(AdoptionSpeed~.-Name-RescuerID-PetID-Description, data=train, ntree=50)
print(rf)
plot(rf)
importance(rf)
varImpPlot(rf)

#Try model with selective variables

rf_model <- randomForest(AdoptionSpeed~nlen_Desc+Age+PhotoAmt+Breed1+Color2+Color1+StateName, data=train, ntree=5)
plot(rf_model)
print(rf_model)
varImpPlot(rf_model)
#Modifying test data
test <- read.csv('test.csv')
head(test)
#Classifying Breeds
test <- mutate(test, PureBreed = ifelse(Breed1 == 0 | Breed2 == 0, "Pure Breed", "Not Pure"))

test <- test %>% left_join(state, by = c("State" = "StateID"))
test$nlen_Desc<-str_length(test$Description)


test$PureBreed <- as.factor(test$PureBreed)
test$StateName <- as.factor(test$StateName)

#Predicting output
pred <- predict(rf_model, newdata=test)
head(pred)

submission <- data.frame(cbind(test$PetID,pred))
names(submission) <- c("PetID", "AdoptionSpeed")
head(submission)

write.table(submission, file="RF_predictions.csv")





















