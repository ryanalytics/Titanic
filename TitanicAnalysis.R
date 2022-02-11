library('caret')


#Get data for both model building and predictions and turn them into data frames
train <- read.csv(TrainLoc)
test <- read.csv(TestLoc)
train <- as.data.frame(train)
test <- as.data.frame(testl)

#replace missing age values with the mean of ages
AgeMean <- mean(train[-which(is.na(train$Age)),7])

train[which(is.na(train$Age)),7] <- AgeMean
test[which(is.na(test$Age)),6] <- AgeMean
test[which(is.na(test$Fare)),10] <- 0

#Remove rows where embarked location is unknown
train <- subset(train, Embarked != 'Unknown')

#First model with be multiple logistic regression 

#Model with all variables included, Fare is normalized with log(Fare + 1) 
Model1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + log(Fare+1) + Embarked + Ticket.Letters + Cabin.Letters, data = train, family = binomial)
summary(Model1)

#Do likelihood Ratio Test(LRT) to see if we should remove Ticket.Letters
ReducedModel1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + log(Fare+1) + Embarked + Cabin.Letters, data = train, family = binomial)
anova(ReducedModel1, Model1, test ="LRT")

#Do likelihood Ratio Test(LRT) to see if we should remove Cabin.Letters
ReducedModel1b <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + log(Fare+1) + Embarked, data = train, family = binomial)
anova(ReducedModel1b, Model1, test ="LRT")

#Remove Parch because it appears to be insignificant
Model2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + log(Fare+1) + Embarked + Ticket.Letters + Cabin.Letters, data = train, family = binomial)
summary(Model2)

#Model2 appears to have all significant values
#Next section will be using cross validation to test model

set.seed(123)

#Get sample from Data set
Sample1  <- createDataPartition(train$Pclass, p = .75, list = FALSE)

#Data for using in the model
Training <- train[Sample1,]

#Data used for prediction
Testing<- train[-Sample1,]

#Create model with 75 percent of data
ModelTrain<- glm(Survived ~ Pclass + Sex + Age + SibSp + log(Fare+1) + Embarked + Ticket.Letters + Cabin.Letters, data=Training, family = binomial)
ModelTrain
summary(ModelTrain)

#Removes ticket letters that may not appear in the test sample we took so we can run the test model
Testing <- subset(Testing, !Ticket.Letters %in% c('sc', 'sca4', 'sp')) 

#x <- Testing[Testing$Ticket %in% Levels,]

#Run model with rest of data
Results <- predict(ModelTrain, type = 'response', newdata = Testing)

#Testing accuracy, checks Results vs actual results from train data
temp = c() 
for(i in 1:length(Results)){
	if(Results[i] < .5){
		temp <- append(temp, 0)
	}
	else{
		temp <- append(temp, 1)
	}
}

#Add one for each correct result
Correct <- 0
for(j in 1:length(Results)){
	if(temp[j] == Testing$Survived[j]){
		Correct <- Correct + 1
	}
}
#Percent of correct results
CorrectPerc <- Correct/length(Results)

#Predicting results for test.csv
#Create list of passenger ids to be used in final csv file 
PassengerId = test$PassengerId
#Create blank vector to be used for prediction of each passenger, 1 = survived, 0 = died
Survived = c()
#Get logistic prediction for each passenger in test
FinalModel <- glm(Survived ~ Pclass + Sex + Age + SibSp + log(Fare+1) + Embarked + Cabin.Letters, data=train, family = binomial) 
FinalResults <- predict(FinalModel ,type = 'response', newdata= test)

#If percentage is less than .5 change to 0, if equal or greater change to 1
for(k in 1:length(PassengerId)){
	if(FinalResults[k] < .5){
		Survived = append(Survived, 0)
	}
	else{
		Survived = append(Survived, 1)
	}
}

#Create data frame to be turned into .csv file
Submission <- data.frame(PassengerId, Survived)

#Create final csv file to be submitted to kaggle
write.csv(Submission, ResultsLoc, row.names=FALSE)
