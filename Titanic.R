library('psych')
library('ggplot2')
library('moments')
library('reshape2')
library('reticulate')
library('pivottabler')
library('tidyr')

#Use reticalute package to run python file that cleans data

#Sets location for python 
use_python(PyLoc, required=TRUE)
#Sets folder location for python file I am running 
setwd(PyFileLoc)
py_run_file('TitanicCleanUp.py')

#Read in train data to be used to make model
train <- read.csv(TrainLoc)
#make train into a data frame
train <- as.data.frame(train)

#Change any blank values into NA 
for(i in 1:ncol(train)){
	train[which(train[, i] == ''), i] <- NA
}

#Check how many NA's are in each 
colSums(is.na(train))
#Give a summary of each column of data 
summary(train)

#Create Data frame listing who survived and who died, will be used for graphs 
Status <- train$Survived
Status[which(Status == 1)] <- 'Survived'
Status[which(Status == 0)] <- 'Died'
Data <- data.frame(Status)

#Plot of total number of passengers who died and who survived
SurG <- ggplot(Data, aes(x=Status)) + geom_bar(stat='count', fill=c('Black', 'Blue')) + ggtitle('Number of Passengers Who Survived and Died') + ylab('Number of Passengers') + scale_y_continuous(breaks = seq(0, 700, by = 100), 
	limits=c(0, 600))
SurG

dev.new()

#Plot of number of passengers per class 
SurG2 <- ggplot(train, aes(x=Pclass)) + geom_bar(stat='count', fill=c('Blue','Red','Green')) + ggtitle('Number of Passengers by Ticket Class') + ylab('Number of Passengers') + xlab('Ticket Class')
SurG2

#pivot table of survivors by class
qhpvt(train, 'Pclass', 'Survived', 'n()')
dev.new()

#Plot of number of passengers per gender 
SurG3 <- ggplot(train, aes(x=Sex)) + geom_bar(stat='count', fill=c('Blue','Pink')) + ggtitle('Number of Passengers by Gender') + ylab('Number of Passengers') + xlab('Gender')
SurG3
#Pivot table of survivors by gender
qhpvt(train,'Sex', 'Survived', 'n()')
dev.new()

#Plot of number of passengers per age
SurG4 <- ggplot(train, aes(x=Age)) + geom_histogram(fill='Blue') + ggtitle('Number of Passengers by Age') + ylab('Number of Passengers') + xlab('Age') + scale_x_continuous(breaks = seq(0, 80, by = 10), limits=c(0, 90))
SurG4
#Pivot table of survivors by age 
qhpvt(train,'Age', 'Survived', 'n()')
dev.new()

#Plot of number of passengers per number of siblings/spouses
SurG5 <- ggplot(train, aes(x=SibSp)) + geom_histogram(stat='count', fill = 'Blue') + ggtitle('Number of Siblings/Spouses for each Passenger') + ylab('Number of Passengers') + xlab('Number of Siblings/Spouses')
SurG5
#Pivot table of survivors by number of siblings/spouses
qhpvt(train, 'SibSp', 'Survived', 'n()')
dev.new()

#Plot of number of passengers per number of parents/children
SurG6 <- ggplot(train, aes(x=Parch)) + geom_histogram(stat='count', fill = 'Blue') + ggtitle('Number of Parents/Children for each Passenger') + ylab('Number of Passengers') + xlab('Number of Children/Parents') 
SurG6
#Pivot table of survivors by number of parent/children
qhpvt(train, 'Parch', 'Survived', 'n()')
dev.new()

#Plot of number of passengers per Ticket Letter
SurG7 <- ggplot(train, aes(x=Ticket.Letters)) + geom_histogram(stat='count', fill = 'Blue') + ggtitle('Number of Passengers per Ticket Letter') + ylab('Number of Passengers') + xlab('Ticket Letter') 
SurG7
#Pivot table of survivors by ticket letter
qhpvt(train, 'Ticket.Letters', 'Survived', 'n()')
dev.new()

#Plot of passengers per Fare price
SurG8 <- qplot(train$Fare, geom='histogram', fill = I("blue"), main = 'Number of Passengers per Amount of Fare Paid', ylab = 'Number of Passengers', xlab = 'Amount of Fare Paid')
SurG8
#Pivot table of survivors by fare price
qhpvt(train, 'Fare', 'Survived', 'n()')
dev.new()

#Plot of passenger per Cabin letter
SurG9 <- ggplot(train, aes(x=Cabin.Letters)) + geom_histogram(stat='count', fill = 'Blue') + ggtitle('Number of passengers for each Cabin Letter') + ylab('Number of Passengers') + xlab('Cabin Letters') 
SurG9
#Pivot table of survivors by cabin letters
qhpvt(train, 'Cabin.Letters', 'Survived', 'n()')
dev.new()

#Plot of passenger per embarkment location
SurG10 <- ggplot(train, aes(x=Embarked)) + geom_histogram(stat='count', fill = 'Blue') + ggtitle('Number of Passengers per Embarkment Location') + ylab('Number of Passengers') + xlab('Embarkment Location') 
SurG10
#Pivot table of survivors by location of embarkment 
qhpvt(train, 'Embarked', 'Survived', 'n()')

dev.new()

#Normalized Fare
qplot(log(train$Fare+1), geom='histogram', fill = I("blue"), main = 'Number of Passengers per Amount of log(Fare + 1) Paid', ylab = 'Number of Passengers', xlab = 'log(Fare + 1)')

dev.new()

#Normalize Sibsp
ggplot(train, aes(x=log(SibSp+1))) + geom_histogram(stat='count', fill = 'Blue') + ggtitle('Number of log(Siblings/Spouses+1) for each Passenger') + ylab('Number of Passengers') + xlab('log(Siblings/Spouses + 1)')

dev.new()

#Normalize Parch
ggplot(train, aes(x=log(Parch+1))) + geom_histogram(stat='count', fill = 'Blue') + ggtitle('Number of log(Parents/Children+1) for each Passenger)') + ylab('Number of Passengers') + xlab('log(Children/Parents + 1')
dev.new()

#Create a correlation table to check for multicollinearity
x <- cor(train[,c(6,7,8,10)], , use = 'complete.obs', method = c("pearson", "kendall", "spearman"))
melted_x <- melt(x)
ggplot(data = melted_x, aes(x=Var1, y=Var2, fill=value)) + geom_tile()



