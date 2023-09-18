#######
### R script for Online Test 
### (Reminder: make sure all the files and 
### scripts are in the working directory)
###
#######
### Reminder: make sure all the files and 
### scripts are in the working directory

### Some auxiliary files to load
source("DataAnalyticsFunctions.R")
options(warn=-1)

#################################################################
### Load data data
churndata <- read.csv("customerchurn.csv")
churndata$Churn<-factor(churndata$Churn)
churndata$tenure <- as.numeric(churndata$tenure)
#################################################################
### summarize the data
summary(churndata)
#################################################################

#1.There is substantial value in understanding what influences churn rates. 
# In particular, customer stickiness (the nature of your customers to continue 
# to use your products or services, to "stick" with you) is a relevant aspect 
# to consider. Which of the following visualizations provides evidence of 
# "customer stickiness"?

#a)
plot(factor(Churn) ~ factor(gender), data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Gender") 
#
#b) 
plot(factor(Churn) ~ factor(SeniorCitizen), data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Senior Citizen") 
#
#c) 
plot(factor(Churn) ~ MonthlyCharges, data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Monthly charges") 
#
#d) 
plot(factor(Churn) ~ tenure, data=churndata, col=c(8,2), ylab="Churn Rate", xlab="Tenure (months)") 


#2. We would like to know if long term customers (with higher tenure) 
# are receiving discounts or not in their monthly fees.
# Which of the following provides a good argument?

#a) Long term customers are more loyal to the company and therefore they are willing to pay more to stay with the company.
# This is exemplified by the positive correlation between the variables
cor(churndata$tenure,churndata$MonthlyCharges)
plot(MonthlyCharges~tenure, data=churndata, xlab='Monthly charges (dollars)', ylab='Tenure (months)', main='Churn')

#b) Long term customers have contracts which are older and hence more expensive as the technology industry reduces costs over time.
res_tenure.simple <- glm(MonthlyCharges~tenure, data=churndata)
summary(res_tenure.simple)
#This is confirmed by the positive coefficient which is statistically significant.

#c) The company seems to price only based on service feasures and does not 
# seem to price discriminate based on customer's characteristics (as those coefficients
# are not statistically significant).
res_tenure <- glm(MonthlyCharges~.-customerID-Churn-TotalCharges, data=churndata)
summary(res_tenure)$coef[,c(1,4)]

#d) After accounting for a quadratic trend, and adding a new variable 
# to the model, we see that the company also seems to price discriminate 
# based on tenure (although with decreasing impact).
churndata$tenure.sq <- churndata$tenure^2
res_tenure.sq <- glm(MonthlyCharges~.-customerID-Churn-TotalCharges, data=churndata)
summary(res_tenure.sq)$coef[,c(1,4)]
churndata<-churndata[,(names(churndata)!="tenure.sq")]
###################


#3. Based on the logistic regression model with all variables (except customerID) discussed in class,
result.logistic<-glm(Churn~.-customerID, data=churndata,family="binomial")
#among the customers 101 to 110, what is the highest probability of churn and how much is it? 
predict(result.logistic,newdata=churndata[101:110,])
which.max(predict(result.logistic,newdata=churndata[101:110,],type="response"))
max(predict(result.logistic,newdata=churndata[101:110,],type="response"))

#a) It is customer number 110 and it is below 1%

#b)	It is customer number 106 and it is near 45%

#c)	It is customer number 108 and it is above 20%

#d)	It is customer number 103 and it is below 45%



#4. Run the logistic regression model with all variables (except customerID) discussed in class
result.logistic <- glm(Churn~.-customerID, data=churndata, family="binomial")
# Next, we will use the model to classify using different thresholds 
# on the predicted probability (not necessarily .5). We will use a function 
# in FPR_TPR.R that computes the true positive rate and false positive rate. 
# The code below plots several choices.
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
for ( val in seq(from = 0, to = 1, by = 0.05)  ){
      values <- FPR_TPR( (result.logistic$fitted >= val) , result.logistic$y )
      points( values$FPR , values$TPR )    
}
# Which of the following is not true?
#a) Good performance should be above the diagonal.
#b) Although 100% accuracy seems impossible, using this predictive model we can achieve a false positive rate that is 
# three times smaller than the true positive rate.
#c) The points (0,0) and (1,1) are not interesting as they correspond to "always predict negative (no churn)" and "always predict positive (churn)" independently of 
#the customer.
#d) If we choose the predicted probability threshold properly we can achieve FPR=.1 and TPR=.9 or better.

#There are no R codes associated with Questions 5 and 6.

