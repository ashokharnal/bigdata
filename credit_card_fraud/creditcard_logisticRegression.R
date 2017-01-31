# Amended on 30/01/2017
# Problem: Predict frauds in credit card transaction
# Dealing with imbalanced / unbalanced data
# Kaggle: https://www.kaggle.com/dalpozz/creditcardfraud
# Unbalanced data:  https://github.com/dalpozz/unbalanced
# Logistic Regression: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

# 1. Clear earlier objects and call libraries
rm(list=ls()) ; gc()
library(readr)        # Reading data
library(dplyr)        # Data manipulation
library(unbalanced)   # loads stringi and ggplot2 also
library(caret)        # For confusionMatrix
library(PRROC)        # Calculate/draw area under Precision-Recall (AUPRC) curve

# 2. Set your working directory to where your data files are
setwd("C:\\Users\\ashok\\OneDrive\\Documents\\credit_card_fraud")
# 3. Read file and look at the data
credit<-read_csv("creditcard.csv",col_names = TRUE)
str(credit)
View(credit)
# 4. Partition data using caret's createDataPartition() function 
trainInd<-createDataPartition(credit$Class, p = 0.75, list=FALSE)
train<-credit[trainInd, ]   # train dataset
valid<-credit[-trainInd,]   # validation dataset

# 5. Balance data
balanced <- ubSMOTE(X = train[,-31], Y = as.factor(train$Class),
                    perc.over=200,   # Oversample minority class data
                    perc.under=800,  # Under sample majority class data
                    verbose=TRUE
                   ) 
# 5.1 'balanced' is a list. Column bind its two components
traindata <- cbind(balanced$X, Class = balanced$Y)
# 5.2 Data dimensions
dim(traindata)                     # Less data
table(credit$Class)/nrow(credit)   # Earlier majority class vs minority class ratio
table(traindata$Class)/nrow(traindata) # The ratio now

# 6 Create logistic regression model
glm.model <- glm(Class ~ ., data = traindata, family = "binomial", control = list(maxit = 50))
# 6.1 Have a look at the model
summary(glm.model)

# 7 Make probability predictions
glm.predict <- predict(glm.model, valid, type = "response")
# 7.1 Convert to class values
pred<- as.logical(glm.predict > 0.5)
pred   # A logical vector of TRUE and FALSE
# Assign "1" where pred is TRUE
pred[pred]<-"1"           # pred gets now converetd to character
pred[pred=="FALSE"]<-"0"  # Assign "0" where pred is FALSE
table(pred)               # Distribution of TRUE and FALSE
pred<-as.factor(pred)     

## Evaluate
# 8. Create confusion matrix
confusionMatrix( valid$Class, pred, mode="prec_recall")

# 9. Calculating area under PR curve
vectorWith_Pve_Class<-valid$Class == "1"    # Datapoint-values corresponding to +ve class
vectorWith_Nve_Class<-valid$Class == "0"    # Datapoint-values corresponding to -ve class
cPositive <- glm.predict[vectorWith_Pve_Class]
cNegative <- glm.predict[vectorWith_Nve_Class]
# 9.1 PR Curve. scores.class0 are datapoints with +ve class
#           scores.class1 are datapoints with -ve class
#      With curve = TRUE takes some time. Default curve = FALSE
pr <- pr.curve(scores.class0 = cPositive, # +ve class
               scores.class1 =cNegative,  # -ve class
               curve = TRUE               # Can be FALSE or quick output
               )
pr
# 10. Plots PR curve and also gives area under pr-curve (auprc)
plot(pr)

##########################FINISH################
