library(randomForest)
library(neuralnet)
library(dummies)
library(dplyr)

#Titanic model for Kaggle
setwd("/Users/ruggerocazzato/Desktop/titanic")

titanic.train <- read.csv("/Users/ruggerocazzato/Desktop/titanic/train.csv",
                          stringsAsFactors = FALSE,
                          header = TRUE)

titanic.test <- read.csv("/Users/ruggerocazzato/Desktop/titanic/test.csv",
                         stringsAsFactors = FALSE,
                         header = TRUE)

titanic.train$isTrainSet <- TRUE
titanic.test$isTrainSet <- FALSE

titanic.test$Survived <- NA 

titanic.full <- rbind(titanic.train, titanic.test)

titanic.full[titanic.full$Cabin== '', "Cabin"] <- NA
#let's look the missing value in the features
nbNA <- function(x){sum(is.na(x))}
apply(titanic.full,2,nbNA)

#cleaning missing value
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked== '',] #we found two people we don't know where they  are embarked from
table(titanic.full[titanic.full$Sex == "female" & titanic.full$Pclass == "1" & titanic.full$Age > 30,"Embarked"])
titanic.full[titanic.full$Embarked== '', "Embarked"] <- "S" #hanno un nome inglese per cui le aggiungiamo al porto S

#clean missing value of age
table(is.na(titanic.full$Age))
boxplot(titanic.full$Age)

age.equation <- "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
age.formula <- as.formula(age.equation)

age.model <- lm(formula = age.formula, data = titanic.full)

age.row <- titanic.full[
  is.na(titanic.full$Age), 
  c("Pclass", "Sex", "Fare",  "SibSp",  "Parch",  "Embarked")
]

age.prediction <- predict(age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age), "Age"] <-  age.prediction

#clean  missing value of fare
#we'll use regression model to predict na value of Fare
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker
titanic.full[outlier.filter,]

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.formula <- as.formula(fare.equation)
fare.model <- lm(formula = fare.formula,  data = titanic.full[outlier.filter,])

#in the next case, the data will be any raws with missing value
fare.row <- titanic.full[
  is.na(titanic.full$Fare), 
  c("Pclass", "Sex", "Age",  "SibSp",  "Parch",  "Embarked")
]

fare.prediction <- predict(fare.model, newdata =  fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <-  fare.prediction

#create child & young from Age
titanic.full$Child <- factor(titanic.full$Age <= 5)
titanic.full$Young <- factor(titanic.full$Age > 15 & titanic.full$Age <= 30)

#create family size feature
#titanic.full$FamilySize <- titanic.full$SibSp + titanic.full$Parch + 1

#categorical casting
for (i in c("Survived", "Pclass","Sex","Embarked")){
  titanic.full[,i] = as.factor(titanic.full[,i])
}

str(titanic.full)

titanic.full <- dummy.data.frame(titanic.full, names = c("Pclass", "Sex", "Embarked"), sep = "_")

#split dataset back out into train and test
titanic.train <- titanic.full[titanic.full$isTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$isTrainSet==FALSE,]

#costruzione equazione e modello per Random Forest 
#survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child + Young"
survived.equation <- "Survived ~ Pclass_1 + Pclass_2 + Pclass_3 + Sex_female + Sex_male + Age + SibSp + Parch + Fare + Embarked_C + Embarked_Q + Embarked_S  + Child + Young"
survived.formula <- as.formula(survived.equation)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, 
                              ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.full))

Survived <- predict(titanic.model, newdata = titanic.test)


PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

#write.csv(output.df, file = "kaggle_submission.csv" ,row.names = FALSE)


