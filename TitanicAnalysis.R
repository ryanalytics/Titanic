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
train$Age <- trunc(train$Age) 
test$Age <- trunc(test$Age) 


#Create train dataset for bayesian model
TrainBayes <- train

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
FinalResults = c()
#Get logistic prediction for each passenger in test, full model for those with ticket.letters that exist in train, partial model for the rest
for(a in 1:nrow(test)){
	if(test$Ticket.Letters[a] %in% c('a','aq3','aq4','lp','sca3','stonoq')){
		FinalModel <- glm(Survived ~ Pclass + Sex + Age + SibSp + log(Fare+1) + Embarked + Cabin.Letters, data=train, family = binomial) 
		FinalResults <- append(FinalResults, predict(FinalModel ,type = 'response', newdata= test[a,]))
	}
	else{
		FinalModel <- glm(Survived ~ Pclass + Sex + Age + SibSp + log(Fare+1) + Embarked + Ticket.Letters + Cabin.Letters, data=train, family = binomial) 
		FinalResults <- append(FinalResults, predict(FinalModel ,type = 'response', newdata= test[a,]))
	}
}
#If percentage is less than .5 change to 0, if equal or greater change to 1
for(k in 1:length(PassengerId)){
	if(FinalResults[k] < .5){
		Survived <- append(Survived, 0)
	}
	else{
		Survived <- append(Survived, 1)
	}
}

#Create data frame to be turned into .csv file
Submission <- data.frame(PassengerId, Survived)

#Create final csv file to be submitted to kaggle
write.csv(Submission, ResultsLoc, row.names=FALSE)





#The next section will be modeling using naive bayes method
#Create priors for survive and died. Priors will be percentage survivors in train data and percentage died in train data 
SurvPrior <- sum(TrainBayes$Survived)/nrow(TrainBayes)
DiedPrior <- 1 - SurvPrior

#Create data sets for passengers who survived and those who died 
TrainSurv <- TrainBayes[which(TrainBayes$Survived == 1),]
TrainDied <- TrainBayes[which(TrainBayes$Survived == 0),]


#Create a data frame for each of the 9 predictor variables for both TrainSurv and TrainDied. They will contain the probability of that variable value given survive or died. 
#Add one to length of each value prevent any zero values from accuring. Also add all values from test and train incase there is not one of those values in train 
TempUniPclass <- append(TrainBayes$Pclass, test$Pclass)
UniPclass <- unique(TempUniPclass)
TempUniSex <- append(TrainBayes$Sex, test$Sex)
UniSex <- unique(TempUniSex)
TempUniAge <- append(TrainBayes$Age, test$Age)
UniAge <- unique(TempUniAge)
TempUniSibSp <- append(TrainBayes$SibSp, test$SibSp)
UniSibSp <- unique(TempUniSibSp)
TempUniParch <- append(TrainBayes$Parch, test$Parch)
UniParch <- unique(TempUniParch)
TempUniFare <- append(TrainBayes$Fare, test$Fare)
UniFare <- unique(TempUniFare)
TempUniEmbarked <-append(TrainBayes$Embarked, test$Embarked)
UniEmbarked <- unique(TempUniEmbarked)
TempUniCabin <- append(TrainBayes$Cabin, test$Cabin.Letters)
UniCabin <- unique(TempUniCabin)
TempUniTicket <- append(TrainBayes$Ticket.Letters, test$Ticket.Letters)
UniTicket <- unique(TempUniTicket)
TempProbS <- c()
TempProbD <- c()
for(A in 1:length(UniPclass)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Pclass == UniPclass[A])) + 1)/(nrow(TrainSurv) + length(UniPclass)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Pclass == UniPclass[A])) + 1)/(nrow(TrainDied) + length(UniPclass)))	
} 
PclassSurv <- data.frame(UniPclass, TempProbS)
PclassDied <- data.frame(UniPclass, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(B in 1:length(UniSex)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Sex == UniSex[B])) + 1)/(nrow(TrainSurv) + length(UniSex)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Sex == UniSex[B])) + 1)/(nrow(TrainDied) + length(UniSex)))	
} 
SexSurv <- data.frame(UniSex, TempProbS)
SexDied <- data.frame(UniSex, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(C in 1:length(UniAge)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Age == UniAge[C])) + 1)/(nrow(TrainSurv) + length(UniAge)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Age == UniAge[C])) + 1)/(nrow(TrainDied) + length(UniAge)))	
} 
AgeSurv <- data.frame(UniAge, TempProbS)
AgeDied <- data.frame(UniAge, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(D in 1:length(UniSibSp)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$SibSp == UniSibSp[D])) + 1)/(nrow(TrainSurv) + length(UniSibSp)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$SibSp == UniSibSp[D])) + 1)/(nrow(TrainDied) + length(UniSibSp)))	
} 
SibSpSurv <- data.frame(UniSibSp, TempProbS)
SibSpDied <- data.frame(UniSibSp, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(E in 1:length(UniParch)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Parch == UniParch[E])) + 1)/(nrow(TrainSurv) + length(UniParch)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Parch == UniParch[E])) + 1)/(nrow(TrainDied) + length(UniParch)))	
} 
ParchSurv <- data.frame(UniParch, TempProbS)
ParchDied <- data.frame(UniParch, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(F in 1:length(UniFare)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Fare == UniFare[F])) + 1)/(nrow(TrainSurv) + length(UniFare)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Fare == UniFare[F])) + 1)/(nrow(TrainDied) + length(UniFare)))	
} 
FareSurv <- data.frame(UniFare, TempProbS)
FareDied <- data.frame(UniFare, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(G in 1:length(UniEmbarked)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Embarked == UniEmbarked[G])) + 1)/(nrow(TrainSurv) + length(UniEmbarked)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Embarked == UniEmbarked[G])) + 1)/(nrow(TrainDied) + length(UniEmbarked)))	
} 
EmbarkedSurv <- data.frame(UniEmbarked, TempProbS)
EmbarkedDied <- data.frame(UniEmbarked, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(H in 1:length(UniCabin)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Cabin.Letters == UniCabin[H])) + 1)/(nrow(TrainSurv) + length(UniCabin)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Cabin.Letters == UniCabin[H])) + 1)/(nrow(TrainDied) + length(UniCabin)))	
} 
CabinSurv <- data.frame(UniCabin, TempProbS)
CabinDied <- data.frame(UniCabin, TempProbD)
TempProbS <- c()
TempProbD <- c()

for(I in 1:length(UniTicket)){
	TempProbS <- append(TempProbS, (length(which(TrainSurv$Ticket.Letters == UniTicket[I])) + 1)/(nrow(TrainSurv) + length(UniTicket)))
	TempProbD <- append(TempProbD, (length(which(TrainDied$Ticket.Letters == UniTicket[I])) + 1)/(nrow(TrainDied) + length(UniTicket)))	
} 
TicketSurv <- data.frame(UniTicket, TempProbS)
TicketDied <- data.frame(UniTicket, TempProbD)
TempProbS <- c()
TempProbD <- c()

#Input data from test to come up with probabilities 
#Initialize vector for final predictions
BayesResults <- c()
#Create vector of the predictor variables in test


#Go through every row of train and get probability for surviving and dying. If probability of Surviving is less than dying, then dying is the prediction, otherwise it is survived
for(row in 1:nrow(test)){
	

	#Bayes theorem. Prob of A  given B is prob A times prob of B given A divided by probability of B(We can leave this part off because it will be the same for died and survive probabilities)
	TempSurv <- (SurvPrior * PclassSurv[which(PclassSurv$UniPclass == test[row, 'Pclass']), 2] * SexSurv[which(SexSurv$UniSex == test[row, 'Sex']), 2] * AgeSurv[which(AgeSurv$UniAge == test[row, 'Age']), 2] *
	SibSpSurv[which(SibSpSurv$UniSibSp == test[row, 'SibSp']), 2] * ParchSurv[which(ParchSurv$UniParch == test[row, 'Parch']), 2] * FareSurv[which(FareSurv$UniFare == test[row, 'Fare']), 2] * 
	EmbarkedSurv[which(EmbarkedSurv$UniEmbarked == test[row, 'Embarked']), 2] * CabinSurv[which(CabinSurv$UniCabin == test[row, 'Cabin.Letters']), 2] * TicketSurv[which(TicketSurv$UniTicket == test[row, 'Ticket.Letters']), 2]) 

	TempDied <- (DiedPrior * PclassDied[which(PclassDied$UniPclass == test[row, 'Pclass']), 2] * SexDied[which(SexDied$UniSex == test[row, 'Sex']), 2] * AgeDied[which(AgeDied$UniAge == test[row, 'Age']), 2] *
	SibSpDied[which(SibSpDied$UniSibSp == test[row, 'SibSp']), 2] * ParchDied[which(ParchDied$UniParch == test[row, 'Parch']), 2] * FareDied[which(FareDied$UniFare == test[row, 'Fare']), 2] * 
	EmbarkedDied[which(EmbarkedDied$UniEmbarked == test[row, 'Embarked']), 2] * CabinDied[which(CabinDied$UniCabin == test[row, 'Cabin.Letters']), 2] * TicketDied[which(TicketDied$UniTicket == test[row, 'Ticket.Letters']), 2]) 

	if(TempSurv < TempDied){
		BayesResults <- append(BayesResults, 0)
	}
	else{
		BayesResults <- append(BayesResults, 1)
	}
		
	
}

Survived <- BayesResults
#Create data frame to be turned into .csv file
Survived <- data.frame(PassengerId, Survived)

#Create final csv file to be submitted to kaggle
write.csv(BayesSubmission, BayesResultsLoc, row.names=FALSE)