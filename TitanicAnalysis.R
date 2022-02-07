library('readxl')
library('openxlsx')
library('writexl')
library('caret')

TrainLoc <- 'C:/Users/Owner/Desktop/Projects/Titanic/TrainClean.csv'
TestLoc <- 'C:/Users/Owner/Desktop/Projects/Titanic/TestClean.csv'

#Get data for both model building and predictions and turn them into data frames
train <- read.csv(TrainLoc)
test <- read.csv(TestLoc)
train <- as.data.frame(train)
test <- as.data.frame(test)

#replace missing age values with the mean of ages
train[which(is.na(train$Age)),7] <- mean(train[-which(is.na(train$Age)),7])

#Remove rows where embarked location is unknown
train <- subset(train, Embarked != 'Unknown')

#First model with be multiple logistic regression 

#Model with all variable included 
Model1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Ticket.Letters + Cabin.Letters, data = train, family = binomial)
summary(Model1)

#Model after removing Ticket Letter because it was the least significant and p value was greater than .05
Model2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Cabin.Letters, data = train, family = binomial)
summary(Model2)

#Model after removing Cabin Letter because it was the least significant and p value was greater than .05
Model3 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, family = binomial)
summary(Model3)

#Model after removing embarked location because it was the least significant and p value was greater than .05
Model4 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, data = train, family = binomial)
summary(Model4)

#Model after removing parch location because it was the least significant and p value was greater than .05
Model5 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare, data = train, family = binomial)
summary(Model5)

#Model after removing parch location because it was the least significant and p value was greater than .05
Model6 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data = train, family = binomial)
summary(Model6)

#Model6 appears to have all significant variables
#Next section will be using cross validation to test model 

set.seed(123)

#Get sample from Data set
Sample1  <- createDataPartition(train$Pclass, p = .75, list = FALSE)

#Data for using in the model
Training <- train[sample1,]

#Data used for prediction
Testing<- train[-sample1,]

#Create model with 75 percent of data
ModelTrain<- glm(Survived ~ Pclass + Sex + Age + SibSp, data=Training, family = binomial)
ModelTrain
summary(ModelTrain)
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