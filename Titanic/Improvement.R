setwd('~/Dropbox/DataScience/KaggleCompetition/Titanic/')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

set.seed(415)

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

combi$Surname <- sapply(combi$Name, 
                        function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)


# combi$TicketPrefix <- sapply(combi$Ticket, function(x) {
#                                 prefix <- strsplit(as.character(x) , 
#                                                    split = ' ')[[1]][1]
#                                 if (is.na(as.numeric(prefix))) {
#                                   return(prefix)
#                                 }
#                                 else{
#                                   return('')
#                                 }
#                                 })

combi$TicketPrefix <- sapply(combi$Ticket, function(x) {
  prefix <- substr(x,1,1)
  if (is.na(as.numeric(prefix))) {
    return(prefix)
  }
  else{
    return('')
  }
})

combi$TicketPrefix <- factor(combi$TicketPrefix)

combi$CabinClass <- sapply(combi$Cabin, function(x) substr(x,1,1))

combi$CabinClass <- factor(combi$CabinClass)

combi$CabinLocation <- sapply(combi$Cabin, function(x) substr(x,2,2))


ageFit <- rpart(Age ~ Pclass + Sex + SibSp + Parch 
                + Fare + Embarked + Title + FamilySize,
                data = combi[!is.na(combi$Age),],
                method = 'anova')

combi$Age[is.na(combi$Age)] <- predict(ageFit,
                                       combi[is.na(combi$Age),])

combi$Embarked[which(combi$Embarked =='')] <- 'S'
combi$Embarked <- factor(combi$Embarked)

combi$Fare[which(is.na(combi$Fare))] <- 
  median(combi$Fare[!is.na(combi$Fare)])

train <- combi[1:891,]
test <- combi[892:1309,]

# fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch 
#                     + Fare + Embarked + Title + FamilySize + TicketPrefix,
#              data = train,
#              importance=TRUE, 
#              ntree=2000)
# varImpPlot(fit)
# prediction <- predict(fit,test)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp 
               + Parch + Fare + Embarked + Title + FamilySize + FamilyID
               + TicketPrefix + CabinClass,
               data = train, 
               controls=cforest_unbiased(ntree=2000, mtry=3))
prediction <- predict(fit, test, OOB=TRUE, type = "response")

testLabel <- data.frame(PassengerId=test$PassengerId,Survived=prediction)
# Output to csv file
write.csv(testLabel,file = 'submission.csv',row.names = FALSE)