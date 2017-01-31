# Amended on 30/01/2017
# Problem: Predict frauds in credit card transaction
# Dealing with imbalanced / unbalanced data
# Kaggle: https://www.kaggle.com/dalpozz/creditcardfraud
# Unbalanced data:  https://github.com/dalpozz/unbalanced
# Logistic Regression: https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

library(readr)        # Reading data
library(dplyr)        # Data manipulation
library(unbalanced)   # loads stringi and ggplot2 also
library(caret)        # For confusionMatrix
library(PRROC)        # Calculate/draw area under Precision-Recall (AUPRC) curve

setwd("C:\\Users\\ashok\\OneDrive\\Documents\\credit_card_fraud")
credit<-read_csv("creditcard.csv",col_names = TRUE)
str(credit)
View(credit)
trainInd<-createDataPartition(credit$Class, p = 0.75, list=FALSE)
train<-credit[trainInd, ]
valid<-credit[-trainInd,]

balanced <- ubSMOTE(X = train[,-31], Y = as.factor(train$Class),
                    perc.over=200,
                    perc.under=800,
                    verbose=TRUE) 
# 6.2
traindata <- cbind(balanced$X, Class = balanced$Y)
# 6.3
dim(traindata)  # Less data
table(credit$Class)/nrow(credit)               # Earlier
table(traindata$Class)/nrow(traindata) # Now

# 6.4 Create model
glm.model <- glm(Class ~ ., data = traindata, family = "binomial", control = list(maxit = 50))
summary(glm.model)
# 6.5 Make probability predictions
glm.predict <- predict(glm.model, valid, type = "response")
# 6.6 Convert to class values
pred<- as.logical(glm.predict > 0.5)
pred   # A logical vector of TRUE and FALSE
# Assign "1" where pred is TRUE
pred[pred]<-"1"           # pred gets now converetd to character
pred[pred=="FALSE"]<-"0"  # Assign "0" where pred is FALSE
table(pred)               # Distribution of TRUE and FALSE
pred<-as.factor(pred)     
# Create confusion matrix
confusionMatrix( valid$Class, pred, mode="prec_recall")

# Calculating area under PR curve
vectorWith_Pve_Class<-valid$Class == "1"    # Datapoint-values corresponding to +ve class
vectorWith_Nve_Class<-valid$Class == "0"    # Datapoint-values corresponding to -ve class
cPositive <- glm.predict[vectorWith_Pve_Class]
cNegative <- glm.predict[vectorWith_Nve_Class]
# PR Curve. scores.class0 are datapoints with +ve class
#           scores.class1 are datapoints with -ve class
# With curve = TRUE takes some time. Default curve = FALSE
pr <- pr.curve(scores.class0 = cPositive, # +ve class
               scores.class1 =cNegative,  # -ve class
               curve = TRUE               # Can be FALSE or quick output
               )
pr
# Plots PR curve and also gives area under pr-curve (auprc)
plot(pr)

##########################FINISH################