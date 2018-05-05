setwd("G:/DrVinodsML")

library(mlbench)
library(caret)
library(e1071)
data(HouseVotes84)
str(HouseVotes84)


HouseVotes84 <- na.omit(HouseVotes84)

dt = sort(sample(nrow(HouseVotes84), nrow(HouseVotes84)*.7))



trainingData <- HouseVotes84[dt,]


testData <- HouseVotes84[-dt,]

# also save the labels
vote_train_labels <- trainingData$Class
vote_test_labels  <- testData$Class


prop.table(table(vote_train_labels))

prop.table(table(vote_test_labels))


vote_classifier <- naiveBayes(trainingData, vote_train_labels)

vote_test_pred <- predict(vote_classifier, testData)



library(gmodels)
CrossTable(vote_test_pred, vote_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))