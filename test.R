library(caret)

rawData <- read.csv('train.csv')
testData <- read.csv('test.csv')

# Fill in missing data
rawData[is.na(rawData[,'Age']),'Age'] <- median(rawData[,'Age'],na.rm=TRUE)

# Split Data into 40% testing and 60% training
splitData <- sample(nrow(rawData),
                    floor(nrow(rawData)*0.6))
trainingData <- rawData[splitData,]
cvData <- rawData[-splitData,]

# Label
y <- trainingData[,'Survived']
# Choose 'Sex' as one Feature
X <- trainingData[,'Sex']

# test using the cvData
cvLabel <- rep(0,nrow(cvData))
cvLabel[cvData[,'Sex'] == 'female'] <- 1

# Table out
predTable <- prop.table(table(cvLabel,cvData[,'Survived']))
print(sum(diag(predTable)))

# test using testData
testLabel <- data.frame(PassengerId=testData[,c('PassengerId')],Survived=0)
testLabel[testData[,'Sex'] == 'female','Survived'] <- 1

# Output to csv file
write.csv(testLabel,file = 'submission.csv',row.names = FALSE)