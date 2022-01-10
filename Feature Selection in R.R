#libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

#data
data("Sonar")
str(Sonar)

#feature selection
set.seed(111)
boruta <- Boruta(Class ~., data = Sonar, doTrace = 2, maxRuns = 500)

print(boruta)

plot(boruta, las = 2, cex.axis = 0.9)

getNonRejectedFormula(boruta) #making use of these 41 variables

getConfirmedFormula(boruta) #copying and making use of 33 confirmed attributes

#data partition
set.seed(222)
ind <- sample(2, nrow(Sonar), replace = T, prob = c(0.6, 0.4))
train <- Sonar[ind==1, ]
test <- Sonar[ind==2,]

#random forest model
set.seed(333)

#60 to indicate this model will be based on all 60 variables
rf60 <- randomForest(Class ~., data=train)
rf60

#41 variables used from boruta

rf41 <- randomForest(Class ~ V1 + V2 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + 
                       V27 + V28 + V30 + V31 + V32 + V34 + V35 + V36 + V37 + V39 + 
                       V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + V52 + V54 + 
                       V59, data=train)

#33 confirmed attributes from boruta

rf33 <- randomForest(Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
                       V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
                       V31 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + V48 + 
                       V49 + V51 + V52, data = train)

#prediction and confusion matrix
p <- predict(rf60, test)
confusionMatrix(p, test$Class)

p1 <- predict(rf41, test)
confusionMatrix(p1, test$Class)

p3 <- predict(rf33, test)
confusionMatrix(p3, test$Class)

#from this last test we can see how even after reducing so many variables we have high accuracy
