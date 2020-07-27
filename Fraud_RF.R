library(randomForest)
FC <- read.csv(file.choose())
View(FC)
str(FC)
Tax <- ifelse(FC$Taxable.Income<=30000,"Risky","Good")
Fraud_check <- data.frame(FC[-3],Tax)
View(Fraud_check)
str(Fraud_check)

## Creating Training and Testing dataset
FC_train <- Fraud_check[1:450,]
View(FC_train)
FC_test <- Fraud_check[451:600,]
View(FC_test)
## Buliding a Random Forest Model
FC_forest <- randomForest(FC_train$Tax~.,data=FC_train,na.action=na.roughfix,importance=TRUE,ntree=500)
## Prediction on training dataset
FC_pred <- predict(FC_forest,FC_train)
library(gmodels)
CrossTable(FC_pred,FC_train$Tax)
## Prediction on testing dataset
FC_test_pred <- predict(FC_forest,FC_test)
CrossTable(FC_test_pred,FC_test$Tax)
## Visualization
plot(FC_forest,lwd=2)
legend("topright", colnames(FC_forest$err.rate),col=1:4,cex=0.8,fill=1:4)
varImpPlot(FC_forest)
