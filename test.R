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

# Calculate Information Gain using 'Sex'
# Entropy for parent
p1 = sum(y)/length(y)
p0 = 1 - p1
entropyParent = -p1*log2(p1) - p0*log2(p0)

# Choose 'Sex' as a Feature
Feature <- 'Sex'
# Choose 'Pclass'as a Feature
Feature <- 'Pclass'
# Choose 'Embarked'
# Feature <- 'Embarked'
# trainingData[trainingData[,Feature]=="",Feature] = "S"

X <- as.factor(trainingData[,Feature])

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