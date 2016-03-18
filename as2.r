#Question 4, of Assignment 2 

#Install required packages
#install.packages("RWeka")
library("RWeka")

cat("\014")  
#Load Datasets
train <- read.csv("~/Desktop/school/datamining/train.csv")
test <- read.csv("~/Desktop/school/datamining/test.csv")

#Factorize the data for J48 algorithm
train <- data.frame(sapply(train, function(x) factor(x) ))
test  <- data.frame(sapply(test, function(x) factor(x) ))

#Model 1: Start with something Simple and choose all the data to see an output
temp <- train[complete.cases(train),]
ml1 <- J48( factor(Survived) ~., data = temp )
eval_j48 <- evaluate_Weka_classifier(ml1, numFolds = 10)
prediction1 <- test
prediction1$Survived <- c(predict(ml1, test))
eval_j48 

# Model# 2: Choose data that seems the most helpful. Selecte: Age, Sex, PcLass, Fare, Parch 
ml2 <- J48(factor(Survived) ~ Age + Sex + Pclass + Fare + Parch, data = train)
eval_j48 <- evaluate_Weka_classifier(ml2, numFolds = 10) # an Improvement of two percent!!!
prediction2 <- test
prediction2$Survived <- c(predict(ml2, test))
eval_j48 

#Model #3: Combine attribute and see if a bigger family lead to higher rate of survival. 
#Exploration of feature engineering
temp <- train
temp$family_size <- factor(as.numeric(train$SibSp) + as.numeric(train$Parch))

test_with_family <- test
test_with_family$family_size <- factor(as.numeric(test$SibSp) + as.numeric(test$Parch) + 1)
ml3 <- J48(factor(Survived) ~ Age + Sex + Pclass + Fare + Parch + Embarked, data = temp)
eval_j48 <- evaluate_Weka_classifier(ml3, numFolds = 10)
prediction3 <- test 
prediction3$Survived <- c(predict(ml3, test_with_family))
eval_j48 

#Model #4: Inquire about J48 Paramters and see if we can improve our classification rate that way. + Pclass + Fare + Parch + Embarked
ml4 <- J48(factor(Survived) ~ Age + Sex + Pclass + Fare + Parch + Embarked, data = train, control = Weka_control(R = TRUE, B= TRUE))
eval_j48 <- evaluate_Weka_classifier(ml4, numFolds = 10) # an Improvement of two percent!!!
prediction4 <- test
prediction4$Survived <- c(predict(ml4, test))
eval_j48 


#Model #5:  
ml5 <- J48(factor(Survived) ~ Age + Sex + Pclass + Parch, data = train, control = Weka_control(A=TRUE, B= FALSE))
eval_j48 <- evaluate_Weka_classifier(ml1, numFolds = 10, seed = 820) 
prediction5 <- test
prediction5$Survived <- c(predict(ml5, test))
eval_j48 

prediction1$Survived <- sapply(prediction1$Survived, function(x) x-1)
write.table(prediction1[c("PassengerId", "Survived")], file = "prediction1.csv", sep = ",")

prediction2$Survived <- sapply(prediction1$Survived, function(x) x-1)
write.table(prediction1[c("PassengerId", "Survived")], file = "prediction2.csv", sep = ",")

prediction3$Survived <- sapply(prediction3$Survived, function(x) x-1)
write.table(prediction1[c("PassengerId", "Survived")], file = "prediction3.csv", sep = ",")

prediction4$Survived <- sapply(prediction1$Survived, function(x) x-1)
write.table(prediction1[c("PassengerId", "Survived")], file = "prediction4.csv", sep = ",")

prediction5$Survived <- sapply(prediction1$Survived, function(x) x-1)
write.table(prediction1[c("PassengerId", "Survived")], file = "prediction5.csv", sep = ",")

