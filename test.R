library(caret)
library(rattle)
library(rpart.plot)

rawData <- read.csv('train.csv')
testData <- read.csv('test.csv')

# Fill in missing data
rawData[is.na(rawData[,'Age']),'Age'] <- median(rawData[,'Age'],na.rm=TRUE)
trainingData[trainingData[,'Embarked']=="",'Embarked'] = "S"

# Split Data into 40% testing and 60% training
splitData <- sample(nrow(rawData),
                    floor(nrow(rawData)*0.6))
trainingData <- rawData[splitData,]
cvData <- rawData[-splitData,]

# Label
y <- trainingData[,'Survived']

# test using the cvData
cvLabel <- rep(0,nrow(cvData))
cvLabel[cvData[,'Sex'] == 'female'] <- 1

# Table out
predTable <- prop.table(table(cvLabel,cvData[,'Survived']))
print(sum(diag(predTable)))

# test using testData
testLabel <- data.frame(PassengerId=testData[,c('PassengerId')],Survived=0)
testLabel[testData[,'Sex'] == 'female','Survived'] <- 1

# Calculate Information Gain using 'Sex'
# Entropy for parent
p1 = sum(y)/length(y)
p0 = 1 - p1
entropyParent = -p1*log2(p1) - p0*log2(p0)


# Choose a Feature
Feature <- 'Sex'
#Feature <- 'Pclass'
#Feature <- 'Embarked'
#Feature <- 'Parch'
X <- factor(trainingData[,Feature])

# Entropy for each class in X
trainingTable  <- prop.table(table(X,y),1)
classEntropy = rowSums(-trainingTable*log2(trainingTable))
# Children Frequency
classFreq <- prop.table(table(X))
# Entropy for children
entropyChildren = sum(classEntropy*classFreq)
# Information Gain using X
IG = entropyParent - entropyChildren
print(IG)

# Information Gain for 'Sex' is about 0.2
# Information Gain for 'Pclass' is about 0.08,
# Information Gain for 'Embark' is about 0.02

trainingDataSelected = trainingData[,c('Sex','Pclass','Embarked','Survived')]
trainingDataSelected[,'Survived'] = factor(trainingDataSelected[,'Survived'])
trainingDataSelected[,'Sex'] = factor(trainingDataSelected[,'Sex'])
trainingDataSelected[,'Pclass'] = factor(trainingDataSelected[,'Pclass'])
trainingDataSelected[,'Embarked'] = factor(trainingDataSelected[,'Embarked'])

# Build a classification tree using 'Sex', 'Pclass', and 'Embark'
treeFit <- rpart(Survived ~ Sex + Pclass + Embarked, data = trainingDataSelected)
#prp(treeFit,varlen=8,faclen=8)
fancyRpartPlot(treeFit,sub='')

# test using the cvData
cvDataSelected = cvData[,c('Sex','Pclass','Embarked')]
cvDataSelected[,'Sex'] = factor(cvDataSelected[,'Sex'])
cvDataSelected[,'Pclass'] = factor(cvDataSelected[,'Pclass'])
cvDataSelected[,'Embarked'] = factor(cvDataSelected[,'Embarked'])

cvLabel <- predict(treeFit,cvDataSelected,type = 'class')

predTable <- prop.table(table(cvLabel,cvData[,'Survived']))
print(sum(diag(predTable)))

# test using testData
testDataSelected = testData[,c('Sex','Pclass','Embarked')]
testDataSelected[,'Sex'] = factor(testData[,'Sex'])
testDataSelected[,'Pclass'] = factor(testData[,'Pclass'])
testDataSelected[,'Embarked'] = factor(testData[,'Embarked'])

testLabel <- predict(treeFit,testDataSelected,type = 'class')

# Output to csv file
write.csv(testLabel,file = 'submission.csv',row.names = FALSE)