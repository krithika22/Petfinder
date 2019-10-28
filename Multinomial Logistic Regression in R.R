install.packages("data.table", repos = "https://cran.r-project.org")
install.packages("tidyverse", repos = "https://cran.r-project.org")
library(tidyverse) # metapackage with lots of helpful functions
library(data.table)
library(dplyr)


train= fread("C:/Users/kvellore/Desktop/Petfinder/Data/train_data_with_description.csv")

#Join State, Breed and Colors
color = fread("C:/Users/kvellore/Desktop/Petfinder/Data/color_labels.csv")
state = fread("C:/Users/kvellore/Desktop/Petfinder/Data/state_labels.csv")
breed = fread("C:/Users/kvellore/Desktop/Petfinder/Data/breed_labels.csv")

summary(train)

#Classifying Breeds
train <- mutate(train, PureBreed = ifelse(Breed1 == 0 | Breed2 == 0, "Pure Breed", "Mixed Breed"))
#Joining Breed to train data
colnames(breed)[1] <- 'Breed1'
train <- left_join(train,breed, by = c('Breed1'))
train <- setDT(train)[,-c('Breed1','Type.y')]
colnames(train)[ncol(train)] <- 'BreedName1'
train$BreedName1[is.na(train$BreedName1)] <- 'None'

colnames(breed)[1] <- 'Breed2'
train <- left_join(train,breed, by = c('Breed2'))
train <- setDT(train)[,-c('Breed2','Type')]
colnames(train)[ncol(train)] <- 'BreedName2'
train$BreedName2[is.na(train$BreedName2)] <- 'None'

names(train)[1]<-"Type"

#Color

colnames(color)[1] <- 'Color1'
train <- left_join(train,color, by = c('Color1'))
train <- setDT(train)[,-c('Color1')]
colnames(train)[ncol(train)] <- 'ColorType1'
train$ColorType1[is.na(train$ColorType1)] <- 'None'

colnames(color)[1] <- 'Color2'
train <- left_join(train,color, by = c('Color2'))
train <- setDT(train)[,-c('Color2')]
colnames(train)[ncol(train)] <- 'ColorType2'
train$ColorType2[is.na(train$ColorType2)] <- 'None'

colnames(color)[1] <- 'Color3'
train <- left_join(train,color, by = c('Color3'))
train <- setDT(train)[,-c('Color3')]
colnames(train)[ncol(train)] <- 'ColorType3'
train$ColorType3[is.na(train$ColorType3)] <- 'None'
# 
train <- train %>% left_join(state, by = c("State" = "StateID"))

head(train)

#Checking for missing and unque values
sapply(train,function(x) sum(is.na(x)))#Checking for missing values
sapply(train, function(x) length(unique(x)))#Checking for unique values

summary(train)

train$AdoptionSpeed <- as.factor(train$AdoptionSpeed)
train$PureBreed <- as.factor(train$PureBreed)
train$StateName <- as.factor(train$StateName)
train$BreedName1 <- as.factor(train$BreedName1)
train$BreedName2 <- as.factor(train$BreedName2)
train$ColorType1 <- as.factor(train$ColorType1)
train$ColorType2 <- as.factor(train$ColorType2)
train$ColorType3 <- as.factor(train$ColorType3)

#Splitting the data into train and test

# n is the number of rows in the data
n = nrow(train) 
trainIndex = sample(1:n,size = round(0.8*n),replace=FALSE)# Train_data
train_data = train[trainIndex,] # We use the index to create training data
test_data = train[-trainIndex,] # Test_Data

#Recode AdoptionSpeed from 0,1,2,3,4 to 1,2,3,4,5


#Creating a baseline model

table(train_data$AdoptionSpeed)

#Removing the variables which are not important for Analysis 
train_data$Name <- NULL
train_data$PetID <- NULL
train_data$RescuerID <- NULL

summary(train_data)

#Setting a reference point for the AdoptionSpeed. All the other values will be compared to the reference point

train_data$AdoptionSpeed <- relevel(train_data$AdoptionSpeed, ref = "0")

#library(car)
#train_data$AdoptionSpeed<-recode(train_data$AdoptionSpeed,"0=1; 1=2;2=3;3=4;4=5")
#head(train_data)

install.packages("nnet", repos = "https://cran.r-project.org")
library(nnet)

colnames(train_data)
model <- multinom(AdoptionSpeed ~ Type+Age+Dewormed+FurLength+Gender+Health+MaturitySize                                 
                                     +PhotoAmt+VideoAmt+BreedName2+Lines_of_description+
                                      StateName+ColorType1+PureBreed+ColorType2+ColorType3
                                      +BreedName1+description_magnitude+
                                      Sterilized+Vaccinated+Quantity+Fee, 
                                     family=multinom,data=train_data,maxit=1000) 
mlogit_output <- summary(model)

summary(model)

#Calculating the coefficients and standard errors

#Coefficients
mlogit_output$coefficients

#Standard Errors
mlogit_output$standard.errors


#Calculating the z score and p-value for the variables in the model

z_score <- mlogit_output$coefficients/mlogit_output$standard.errors
print(z_score,digits=3)

p_value <- (1-pnorm(abs(z_score),0,1))*2
print(p_value,digits=3)

#Calculating the odds ratio

OddML <- exp(coef(mlogit_output))
print(OddML,digits=2)

#Predicting on the test data
predictedML <-predict(model,test_data,na.action=na.pass,type="probs")
predicted_classML <- predict(model,test_data)

install.packages("caret", repos = "https://cran.r-project.org")
library(caret)

caret :: confusionMatrix(as.factor(predicted_classML),as.factor(test_data$AdoptionSpeed))

mean(as.character(predicted_classML)!=
    as.character(test_data$AdoptionSpeed))
#0.6724377 : The overall accuracy of the model

#Calculating the important variables with the caret package 

mostImportantVariables <- varImp(model)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))

#Building the final multinomial model based on the p-value,Accuracy 

model1 <- multinom(AdoptionSpeed~Age+Dewormed+FurLength+Health+MaturitySize+                                
                     PhotoAmt +Sterilized+Vaccinated, 
                   family=multinom,data=train_data,maxit=1000)

mlogit_output1 <- summary(model1)

predictedML <-predict(model,test_data,na.action=na.pass,type="probs")
predicted_classML <- predict(model1,test_data)

caret :: confusionMatrix(as.factor(predicted_classML),as.factor(test_data$AdoptionSpeed))



