# 'caTools' package provides us with functions to split dataset uniformly to test and training
library(caTools)

# Load library 'class' that has the knn() function
library(class)

# Function to split the dataset randomly
splitFile <- function(dataset, trProp, classColPos) {
  # split the dataset
  sample = sample.split(iris[, classColPos], SplitRatio = trProp)
  
  # create training and testing dataset
  train = subset(iris, sample == TRUE)
  test = subset(iris, sample == FALSE)
  
  # save the target labels and remove from the train and test dataset
  trainLabels <- train[, classColPos]
  testLabels <- test[, classColPos]
  train <- train[, -classColPos]
  test <- test[, -classColPos]
  
  # Nomalize function
  normalize <- function(x) {
    return( (x-min(x))/(max(x)-min(x)))
  }
  train
  test
  # Normalize test and training dataset
  gtrn <- as.data.frame(lapply(train, normalize))
  gtsn <- as.data.frame(lapply(test, normalize))
  
  return(list(trn=gtrn, trL=trainLabels, val=gtsn, tsL=testLabels))
}

# Function to plot graph
plotFn <- function(dataSet, graphTitle = '', ylimLo=0) {
  plot(dataSet[, 1], dataSet[, 2],  main = graphTitle, xlab = 'k Nearest Neighbours',
       ylab = 'Accuracy', ylim = c(ylimLo, 1), type = 'o', col = 'red')
  lines(dataSet[, 1], dataSet[, 3], type = 'o', col = 'blue')
  legend(26, 0.6, legend=c("Training Accuracy", "Testing Accuracy"),
         col=c("red", "blue"), lty=1:2, cex=1.4)
}


# Function to use k-NN and return training and testing results
train_test <- function(trainData,trainLabels,testData,testLabels) {
  train <- c()
  test <- c()
  for (k in 1:40) {
    knntr <- knn(trainData, trainData, trainLabels, k=k)
    knnts <- knn(trainData, testData, trainLabels, k=k)
    trTable <- table(knntr, trainLabels)
    tsTable <- table(knnts, testLabels)
    trTable <- prop.table(trTable)
    tsTable <- prop.table(tsTable)
    trainAccuracy <- sum(trTable[1,1], trTable[2,2], trTable[3,3])/sum(trTable)
    testAccuracy <- sum(tsTable[1,1], tsTable[2,2], trTable[3,3])/sum(tsTable)
    train <- c(train, trainAccuracy)
    test <- c(test, testAccuracy)
  }
  acc <- data.frame('k' = 1:40, 'trAc' = train, 'tsAc' = test)
  return(acc = acc)
}

# Single function to split data and then call train_test function
avgTrnTst <- function(dataset, trProp, classColPos) {
  for (i in 1:30) {
    a <- splitFile(dataset, trProp, classColPos)
    b <- train_test(a$trn, a$trL, a$val, a$tsL)
    if (i==1) acd <- b
    else	  acd <- rbind(acd, b)
  }
  library(plyr)
  
  a1 <- ddply(acd,.(k), summarize, meanV = mean(trAc))
  a2 <- ddply(acd,.(k), summarize, meanV = mean(tsAc))
  m  <- merge(a1,a2,by='k')
  
  return(m)
}
