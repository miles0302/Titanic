setwd('~/Dropbox/DataScience/KaggleCompetition/Titanic/')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

test$Survived <- NA

combi <- rbind(train,test)

combi$Name <- as.character(combi$Name)

combi$Title <- sapply(combi$Name, function(x){strsplit(x, split = '[,.]')[[1]][2]})

combi$Title <- sub(' ','',combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Don','Capt','Sir','Major')] <- 'Sir'

combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

# for (title in names(table(combi$Title)))
# {
#   combi$Age[is.na(combi$Age) & combi$Title == title] <- 
#     mean(combi$Age[combi$Title == title & is.na(combi$Age) == FALSE])
# }

#combi$Surname <- sapply(combi$Name, function(x) {strsplit(x, split = '[,.]')[[1]][1]})
#combi$FamilyID <- paste(as.character(combi$FamilySize),combi$Surname,sep = '')
#combi$FamilyID[combi$FamilySize <= 2 ] <- 'Samll'

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize,
             data = train,
             method = 'class')
#new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(fit)

# test using testData
prediction <- predict(fit,test,type = 'class')
testLabel <- data.frame(PassengerId=test$PassengerId,Survived=prediction)
# Output to csv file
write.csv(testLabel,file = 'submission.csv',row.names = FALSE)