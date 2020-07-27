Company_data <- read.csv(file.choose())
View(Company_data)
str(Company_data)
High_Sales <- ifelse(Company_data$Sales > 10,"High","Low")
Company_data1 <- data.frame(Company_data[2:11],High_Sales)
View(Company_data1)
str(Company_data1)
table(Company_data1$High_Sales)
## Creating train and testing dataset
Comp_train <- Company_data1[1:300,]
View(Comp_train)
Comp_test <- Company_data1[301:400,]
View(Comp_test)
## Buliding a Random Forest Model
library(randomForest)
Company_RF <- randomForest(High_Sales~.,data=Comp_train,na.action = na.roughfix,importance=TRUE,ntree=1000)
## Prediction on Training Data
Com_pred_train <- predict(Company_RF,Comp_train)
library(caret)
confusionMatrix(Comp_train$High_Sales,Com_pred_train)## Accuracy 100%
## Prediction on testing dataset
Com_pred_test <- predict(Company_RF,Comp_test)
confusionMatrix(Comp_test$High_Sales,Com_pred_test)## Accuracy 100%
## Visualization
plot(Company_RF,lwd=2)
legend("topright", colnames(Company_RF$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(Company_RF)
