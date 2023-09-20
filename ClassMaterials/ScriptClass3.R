#######
### R script for Class 3: 
### Predictive Analytics: Classification 
### (Reminder: make sure all the files and 
### scripts are in the working directory)
###
source("DataAnalyticsFunctions.R")

### Load data data
churndata <- read.csv("./ClassMaterials/customerchurn.csv")
### if you want to visualize the data in a spreadsheet view
### uncomment the line below (works in R Studio)
### View(churndata) 
### summarize the data
summary(churndata)
### This display names and some statistics of variables.
### For instance, we see that churn rate in the data set is 26.5%
sum(churndata$Churn=="Yes")/ nrow(churndata)
churndata$Churn<-factor(churndata$Churn)
churndata$tenure <- as.numeric(churndata$tenure)
###
### We are interested in predicting if a customer with churn
### so the target variable is "Churn" and we define Y=1 if churn occurs
###
### we observe many features of the customers to help us make predictions
###
### 
churndata$gender <- factor(churndata$gender)
churndata$SeniorCitizen <- factor(churndata$SeniorCitizen)
plot(factor(Churn) ~ gender, data=churndata, col=c(8,2), ylab="Churn Rate") 

## Is churn rate statistically independent of gender?
## Using the contingency table of Class 1 we have
m00 <- sum( (churndata$gender == "Female") & (churndata$Churn == "No") ) 
m01 <- sum( (churndata$gender == "Female") & (churndata$Churn == "Yes") ) 
m10 <- sum( (churndata$gender == "Male") & (churndata$Churn == "No") ) 
m11 <- sum( (churndata$gender == "Male") & (churndata$Churn == "Yes") ) 
# Construct the contingency table
ContingencyMatrix <- as.table(rbind(c(m00, m01), c(m10, m11)))
### perform the Pearson chi squares test for independent of factors
chisq.test(ContingencyMatrix)$p.value  # p-value of the statistical test 
### p-value = 0.48... we fail to reject independence
###
plot(factor(Churn) ~ SeniorCitizen, data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Senior Citizen") 
###
plot(factor(Churn) ~ factor(InternetService), data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Type of Internet Service") 
###
m <- aggregate(Churn=="Yes"~ DeviceProtection + TechSupport, data=churndata, FUN = mean)
Labelsm <- paste(m[,1],"/",m[,2])
Labelsm[3] <- "\n No Internet \n Service"
barplot( m[,3], names.arg = Labelsm, col=c(2), ylab = "Churn Rate", xlab = "Device Protection / Tech Support")
###
###
plot(factor(Churn) ~ tenure, data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Tenure (months)") 
### note that treating tenure as "factor" would not make the visualization as neat
### since it would split across months as shown below
plot(factor(Churn) ~ factor(tenure), data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Tenure (months)") 

### Instance-space view for continuous variables in binary classification
###
### plot default outcomes with Duration and Amount of loan information
### Y = 1 (Default) Red;  Y = 0 (Paid) Blue; 
plot(tenure~MonthlyCharges, data=churndata, xlab='Monthly charges (dollars)', ylab='Tenure (months)', main='Churn')
plot(tenure~MonthlyCharges, data=churndata, xlab='Monthly charges (dollars)', ylab='Tenure (months)', main='Churn', col=ifelse(Churn=="Yes",'red','blue'))

par(mar=c(6,6,6,6))
smoothScatter(churndata$MonthlyCharges,churndata$tenure, nrpoints = 0,   xlab="Monthly charges (dollars)", ylab="Tenure (months)", colramp=colorRampPalette(c("white","darkgreen")),  main = "Density for customers")
smoothScatter(churndata$MonthlyCharges[which(churndata$Churn=="Yes")],churndata$tenure[which(churndata$Churn=="Yes")], nrpoints = 0,   xlab="Monthly charges (dollars)", ylab="Tenure (months)", colramp=colorRampPalette(c("white","darkred")),  main = "Density for customers who did churn")
smoothScatter(churndata$MonthlyCharges[which(churndata$Churn=="No")],churndata$tenure[which(churndata$Churn=="No")],  nrpoints = 0, xlab="Monthly charges (dollars)", ylab="Tenure (months)",  main = "Density for customers who did not churn")


### Why not linear regression?
### lets run a linear regression.. we can do it
result_linear <- glm(Churn=="Yes"~tenure+MonthlyCharges, data=churndata)
summary(result_linear)
Customerchurn <- churndata[71:100,]
predict(result_linear,newdata=Customerchurn)
### We get negative probabilities! Imaginige estimating risk.
### Truncating to zero does not help! Near zero margins... 

### Run Logistic Regression. It uses the glm function (generalized linear model)
### We need to specify that it is a logistic regression
### by writing the option: family="binomial"
result <- glm(Churn~tenure+MonthlyCharges, data=churndata, family="binomial")
summary(result)
### We can compute the R squared
Rsq <- 1 - result$deviance/result$null.deviance
Rsq
# R squared 0.2154312
#
#### To predict, it will be very similar to linear regression
### Lets create some customers to forecast. Say observations 4,5,6. 
CustomerChurn <- churndata[4:6,]
### then we call 'predict' providing the model 'result'
### and the new data to be forecasted (the "CustomerToForecast") 
predict(result,newdata=CustomerChurn)
### It was a negative number! No need to panic.
### It is actually the log odds (X beta, not yet the probability of churn)
### Either you do the math by computing the link function or
### you simply call predict with the command: type="response"
predict(result,newdata=CustomerChurn,type="response")
### and we get the probabilities that the model predicts.
### In particular, the first has a nearly 5% chance of churn 
### while the third nearly 75%
###
### We can expect a lot of heterogeneity of the probabilities 
### across customers. For example, here are the actual 
### customers we want to forecast (displaying only "tenure" and "Monthly charges")
churndata[4:6,c("tenure","MonthlyCharges")]
###
### 

# the coefficient of Monthly charges is 
result$coef[3]
# 0.03295389
### Increasing monthly charges by 20 we have log odds of churn to increase by
20 * result$coef[3] 
# 0.6590778 
## and the odd change by a factor of
exp( 20 * result$coef[3] )
### which is nearly double the odds.
###
### Note that the change in actual probabilities depends on the initial value
# if the odds were 1:1 now it is 2:1, and probability is 2/3

### We can plot the data and the 50/50 line induced by the logistic regression
plot(tenure~MonthlyCharges, data=churndata, main="Churn Prediction via Logistic Regression",xlab='Monthly charges (dollars)', ylab='Tenure (months)', col=ifelse(Churn=="Yes",'red','blue'))

aux_vec <- c(min(churndata$MonthlyCharges), max(churndata$MonthlyCharges) )
z_logistic <- (-result$coef[1] - result$coef[3]*aux_vec) / result$coef[2]
lines( aux_vec, z_logistic, lwd=6, lty=2, col="green")  


### Full model
result <- glm(Churn=="Yes"~.-customerID, data=churndata, family="binomial")
### We see the summary of the results as we did for linear regression
summary(result)
### We can compute the R squared
Rsq <- 1 - result$deviance/result$null.deviance
Rsq
### R square = 0.2845



#######################################################
### Run Classification Tree
### install the required packages some packages
install.packages("tree")
library(tree)
install.packages("partykit")
library(partykit)

### This runs the tree. The formula describes what is the label and 
### what are the variables to branch.
churndata$Churn<-factor(churndata$Churn)
Churntree <- tree(Churn~tenure+MonthlyCharges, data=churndata) 
summary(Churntree)
###
### We can obtain a description of the whole tree by 
Churntree
###
### We can easily plot the tree
plot(Churntree) 
### and add some labels explaining what is happening.
### using label = "yval" we obtain the recommendation 
### on the leaves of the tree of Yes or No
text(Churntree, label="yval")
### to get the probabilities we simple use label="yprob"
plot(Churntree)
text(Churntree,label="yprob")
### It provided the Branching rules 
### (if the condition satisfied go to the "left"), and
### at each leaf of the tree it provided 
### the proportion of Y = 1 (churn=Yes) in that group
###
###########################################################
###
### Finally, we can extract  
### the vector of probabilities can be obtained in
Churntree[[1]]$yprob[,2]
###
### and also plot the partition of the space
partition.tree(Churntree, label = "yval", add = FALSE)

## the command below does not seem to provide the probabilities
##partition.tree(Churntree, label = "yprob", add = FALSE)
## I had to "force" it..
tmp<- Churntree[[1]]$yval 
Churntree[[1]]$yval <- paste("prob\nchurn\n",signif(Churntree[[1]]$yprob[,2], digits = 3))
partition.tree(Churntree, label = "yval")
Churntree[[1]]$yval <- tmp
######################
###
###

### FULL MODEL with tree. The formula describes what is the label and 
### what are the variables to branch.

### Using only complete data (note that there are a few missing entries in ~10 observations)
completechurndata <- churndata[complete.cases(churndata), ]
names(completechurndata)
### Setting the types as factors instead of character
###
for ( rrr in names(completechurndata)){
  if ( !(class(completechurndata[,rrr]) %in% c("numeric","factor")) ){
    completechurndata[,rrr] <- as.factor(completechurndata[,rrr])    
  }  
  
}
ChurntreeFull <- tree(Churn ~ . - customerID, 
                      data=completechurndata, control=tree.control(nobs = 7032, mincut = 2, minsize = 5, mindev = 0.002) ) 

summary(ChurntreeFull)
###
### We can obtain a description of the whole tree by 
ChurntreeFull
###
### We can easily plot the tree
plot(ChurntreeFull) 
### and add some labels explaining what is happening.
### using label = "yval" we obtain the recommendation 
### on the leaves of the tree of Yes or No
text(ChurntreeFull, label="yval")
### to get the probabilities we simple use label="yprob"
plot(ChurntreeFull)
text(ChurntreeFull,label="yprob")


### Full model for Logistic Regression
result <- glm(Churn~.-customerID, data=completechurndata, family="binomial")
### We see the summary of the results as we did for linear regression

###
### Plotting FPR and TPR
### Logistic Regression
threshold <- .5
TP <- sum((result$fitted >= threshold)*result$y)
FP <- sum((result$fitted >= threshold)*(!result$y))
FN <- sum((result$fitted <  threshold)*result$y)
TN <- sum((result$fitted <  threshold)*(!result$y))

LR.FPR <- FP / (FP + TN)
LR.TPR <- TP / (TP + FN)

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( LR.FPR), c(LR.TPR))
text( c( LR.FPR+0.26), c(LR.TPR-.00), labels=c("Logistic Regression (0.5)"))

threshold <- .3
TP <- sum((result$fitted >= threshold)*result$y)
FP <- sum((result$fitted >= threshold)*(!result$y))
FN <- sum((result$fitted <  threshold)*result$y)
TN <- sum((result$fitted <  threshold)*(!result$y))

LR.FPR <- FP / (FP + TN)
LR.TPR <- TP / (TP + FN)
points( c( LR.FPR), c(LR.TPR))
text( c( LR.FPR+0.04), c(LR.TPR-.03), labels=c("Logistic Regression (0.3)"))

threshold <- .7
TP <- sum((result$fitted >= threshold)*result$y)
FP <- sum((result$fitted >= threshold)*(!result$y))
FN <- sum((result$fitted <  threshold)*result$y)
TN <- sum((result$fitted <  threshold)*(!result$y))

LR.FPR <- FP / (FP + TN)
LR.TPR <- TP / (TP + FN)
points( c( LR.FPR), c(LR.TPR))
text( c( LR.FPR+0.20), c(LR.TPR+.03), labels=c("Logistic Regression (0.7)"))


threshold <- .5
TP <- sum((result$fitted >= threshold)*result$y)
FP <- sum((result$fitted >= threshold)*(!result$y))
FN <- sum((result$fitted <  threshold)*result$y)
TN <- sum((result$fitted <  threshold)*(!result$y))

LR.FPR <- FP / (FP + TN)
LR.TPR <- TP / (TP + FN)


## ROC is (FPR, TPR)
## 
## Tree
TP <- sum((predict(Churntree,type="class") == "Yes" )*(churndata$Churn=="Yes"))
FP <- sum((predict(Churntree,type="class") == "Yes" )*(churndata$Churn=="No"))
FN <- sum((predict(Churntree,type="class") == "No" )*(churndata$Churn=="Yes"))
TN <- sum((predict(Churntree,type="class") == "No" )*(churndata$Churn=="No"))
Tr.FPR <- FP / (FP + TN)
Tr.TPR <- TP / (TP + FN)
##

plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( LR.FPR,Tr.FPR), c(LR.TPR,Tr.TPR))
text( c( LR.FPR+0.04,Tr.FPR+0.01), c(LR.TPR-.03,Tr.TPR-.04), labels=c("Logistic Regression","Classification Tree"))
### Logistic Regression
threshold <- .3
TP <- sum((result$fitted >= threshold)*result$y)
FP <- sum((result$fitted >= threshold)*(!result$y))
FN <- sum((result$fitted <  threshold)*result$y)
TN <- sum((result$fitted <  threshold)*(!result$y))

LR3.FPR <- FP / (FP + TN)
LR3.TPR <- TP / (TP + FN)
points( c( LR3.FPR ), c(LR3.TPR))
text( c( LR3.FPR ), c(LR3.TPR+.05), labels=c("Logistic Regression with .3"))
###


###### Random Forest

installpkg("randomForest")
library(randomForest)
model1 <- randomForest(Churn~.-customerID, data=completechurndata, nodesize=5, ntree = 500, mtry = 4)
model1
RF.prob <- predict(model1,type = "prob")[,2]
threshold <- .5
RF.y <- completechurndata$Churn=="Yes"
TP <- sum((RF.prob >= threshold)*RF.y)
FP <- sum((RF.prob >= threshold)*(!RF.y))
FN <- sum((RF.prob <  threshold)*RF.y)
TN <- sum((RF.prob <  threshold)*(!RF.y))
MisclassificationRF <- (FP+FN)/(TP+FP+FN+TN)

RF5.FPR <- FP / (FP + TN)
RF5.TPR <- TP / (TP + FN)
points( c( RF5.FPR ), c(RF5.TPR), pch=17)
text( c( RF5.FPR ), c(RF5.TPR+.05), labels=c("Random Forest"))

### Full model for Logistic Regression
result <- glm(Churn~.-customerID, data=completechurndata, family="binomial")
### We see the summary of the results as we did for linear regression
summary(result)
threshold <- .5
TP <- sum((result$fitted >= threshold)*result$y)
FP <- sum((result$fitted >= threshold)*(!result$y))
FN <- sum((result$fitted <  threshold)*result$y)
TN <- sum((result$fitted <  threshold)*(!result$y))
MisclassificationGLM <- (FP+FN)/(TP+FP+FN+TN)


plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c( LR.FPR), c(LR.TPR))
text( c( LR.FPR+0.23), c(LR.TPR-0.00), labels=c("Logistic Regression"))
points( c(Tr.FPR), c(Tr.TPR))
text( c(Tr.FPR+0.08), c(Tr.TPR-.04), labels=c("Classification Tree"))
points( c( RF5.FPR ), c(RF5.TPR), pch=17)
text( c( RF5.FPR+0.03 ), c(RF5.TPR-.025), labels=c("Random Forest"))

### We can compute the R squared
Rsq <- 1 - result$deviance/result$null.deviance
Rsq
### R square = 0.2845

###################################################################
### This is in the Supplementary Material
###################################################################
###
###
###
###
### Support Vector Machine (SVM)
###
### Install and load the required packages
installpkg("e1071")
installpkg("rpart")
library(e1071)
library(rpart)

### We call the SVM 
svm_result <- svm(Churn~tenure+MonthlyCharges, data=churndata, type="C-classification", kernel="linear")#, cost = 10, gamma=1)
### There were some parameters to be defined
### type="C-classification", kernel="linear", cost = 10, gamma=1
### To have to decide on these parameters is a disadvantage in small problems. 
### Performance might change a lot. 
### (Note: The method was created to deal with huge data sets where tuning parameters is unavoidable
### so for those applications it was reasonable to have it)
### Lets read the summary
summary(svm_result)
###
### We can get the  intercept (svm_result$rho)
### and the coefficients (beta_svm) for the
### separating hyperplane which has the following interpretation:
###  X beta_svm + svm_result$rho >  0 would mean be "Default" 
###  X beta_svm + svm_result$rho <= 0 would mean be "No Default" 
svm_result$rho
beta_svm <- t(svm_result$coefs) %*% svm_result$SV
beta_svm
###
###
### We can also use prediction. Going back to "dude" as the one to be predicted
predict(svm_result,newdata=CustomerChurn)
### Note: predict for svm only make predictions for the observations in which you have complete data in the model
##        (no NA entries in the variables used in the model). 
##       To get which observations are complete one can use complete.cases(churndata)


### We can also plot the separating hyperplane
plot(tenure~MonthlyCharges, data=churndata, main="Churn Prediction via SVM", xlab='Monthly charges (dollars)', ylab='Tenure (months)', col=ifelse(Churn=="Yes",'red','blue'))

z_svm <- (-svm_result$rho - beta_svm[2]*aux_vec) / beta_svm[1]
lines( aux_vec, z_svm, lwd=5, lty=2, col = "green" )  
###
###
### we see that the lines are essentially parallel.
### this is with this data. They can be very different.
###
## SVM
TP <- sum((predict(svm_result) == "Yes" )*(churndata$Churn=="Yes"))
FP <- sum((predict(svm_result) == "Yes" )*(churndata$Churn=="No"))
FN <- sum((predict(svm_result) == "No" )*(churndata$Churn=="Yes"))
TN <- sum((predict(svm_result) == "No" )*(churndata$Churn=="No"))
SVM.FPR <- FP / (FP + TN)
SVM.TPR <- TP / (TP + FN)


###
###
### Bond Rating Services (This illustrates classification with more than 2 classes)
### 
### install packages for multinomial logistic regression
installpkg("nnet")
library(nnet)
### load the data
BondRating <- read.csv("BondRating.csv", header=TRUE)
summary(BondRating)

### Turn rating into factor
BondRating$RATING<-factor(BondRating$RATING)
### We call the multinomial logistic regression
bond.result <- multinom(RATING ~ LOPMAR+LFIXCHAR+LGEARRAT+LLEVER, data = BondRating )
### Lets look at the summary. How many coefficients?
summary(bond.result)
### 

### fit plots: plot p_yi distribution for each true yi
# use predict to get in-sample probabilities
bond.predict <- predict(bond.result, newdata = BondRating, "probs")
### bond.predict is a matrix n x K
### get the probs for what actually happened
### note clever use of BondRating$CODERTG to index the second column 
### of the matrix bond.predict (I know, very cool...)
### Here is the probability of true class for each observation
n <- length(BondRating$CODERTG)
trueclassprobs <- bond.predict[cbind(1:n, BondRating$CODERTG)] 
## plot true probs, with varwidth to have the box widths proportional to response proportion.
plot(trueclassprobs ~ BondRating$RATING , col="blue", varwidth=TRUE, xlab="Credit Rating", ylab="P( True Class )") 
### We see that AA have low fitted probabilities 
### (since there are quite a few of them we might be misclassifying that class a bit more)
### Note that the width of the box is proportional
### to the count of the class in the sample
summary(BondRating$RATING)
### We have less AAA than BA ratings in the data.


