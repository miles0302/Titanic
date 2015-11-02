library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

setwd('~/Dropbox/DataScience/KaggleCompetition/Titanic/')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

# Decison based on 'Sex'
fit <- rpart(Survived ~ Sex, data=train, method="class")
fancyRpartPlot(fit,sub='')

# Build a classification tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train,
             method = 'class')
#new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(fit)

# test using testData
prediction <- predict(fit,test,type = 'class')
testLabel <- data.frame(PassengerId=test$PassengerId,Survived=prediction)
# Output to csv file
write.csv(testLabel,file = 'submission.csv',row.names = FALSE)