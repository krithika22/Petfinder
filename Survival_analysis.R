#Survival Analysis

train <- read.csv("train.csv")
head(train)
colnames(train)
train <- train[complete.cases(train),]
install.packages("survival", repos = "https://cran.r-project.org")
devtools::install_github("sachsmc/ggkm") # We need this for plotting
#Survival Analysis
train$Adopted <- ifelse(train$AdoptionSpeed == 4,0,1)
library(survival)
library(ggplot2)
library(ggkm)
train1 <- subset(train, Type==2)
attach(train)

x<-cbind(Age, Breed1, Breed2, Color1, Color2, Color3,  Dewormed, Fee,
      FurLength, Gender, Health, MaturitySize,PhotoAmt, Quantity,State,
      Sterilized, Type, Vaccinated, VideoAmt)
#Model1
coxph <- coxph(Surv(AdoptionSpeed,Adopted)~Age+PhotoAmt, method="breslow")
summary(coxph)
#Model2
km1 <- survfit(Surv(AdoptionSpeed,Adopted)~1)
summary(km1)
plot(km1, xlab = "Time", ylab = "Survival Probability")
#Model3
km2 <- survfit(Surv(AdoptionSpeed,Adopted)~Gender)
summary(km2)
plot(km2, xlab = "Time", ylab = "Survival Probability")
ggplot(data, aes(time = lenfol, status = fstat, color = factor(Gender))) + geom_km()
