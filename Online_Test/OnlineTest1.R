#######
### R script for Online Test
### (Reminder: make sure all the files and 
### scripts are in the working directory)
###
###
### Some auxiliary files to load
source("./Online_Test/DataAnalyticsFunctions.R")
#################################################################
installpkg("quantreg")
library(quantreg)
options(warn=-1)
#################################################################
### Load infant weight data
###
load("./Online_Test/WorkspaceOnlineTest1.RData")
###
### This creates a data frame called "DATA" 
### This data set has 198377 observations and 15 variables
#################################################################
summary(DATA)
View(DATA)
#################################################################

#1. As discussed in class, when considering the birthweight, 
# we are concern with low birthweight. Which of the following 
# models is more suitable for that? 
#a) model.a <- glm(weight ~ ., data = DATA)
model.a <- glm(weight ~ ., data = DATA)
    ## .means all the other variables
#b) model.b <- rq(weight ~ ., tau = 0.5, data = DATA)
model.b <- rq(weight ~ ., tau = 0.5, data = DATA)
#c) model.c <- rq(weight ~ ., tau = 0.1, data = DATA)
model.c <- rq(weight ~ ., tau = 0.1, data = DATA)
#d) model.d <- rq(weight ~ ., tau = 0.9, data = DATA)
model.d <- rq(weight ~ ., tau = 0.9, data = DATA)

#2. A pregnant patient came to the doctor's office concerned with the child. Although she is not gaining weight she is considering to keep smoking. 
# In order to convince the mother to quit smoking, an appropriate regression model 
# and interpretation was told the patient. Which of the following options would the medical provider argue to convince her?
#a) Based on 
summary(model.a)$coef["smoke",]
# I am confident that by quitting smoking now you can increase the child's weight on average by 166g. 
# This gain can help the child considerably.
#b) Looking at the tail is important to see the extreme impact smoking can have. Based on 
summary(model.c)$coef["smoke",]
# I am confident that by quitting smoking now it can increase your child's weight by 176g which 
# can  clinically help the cihld.
#c) Based on
summary(model.c)$coef["smoke",]
summary(model.c)$coef["cigsper",]
# I recommend you to simply reduce smoking by 5 cigarettes per day since the impact is roughly the same as not smoking. 
#d) Based on
summary(model.b)$coef["smoke",]
summary(model.c)$coef["smoke",]
summary(model.d)$coef["smoke",]
# no matter if you are concern with extreme or medium birthweights, mothers that do not smoke have 
# child which are about 160g heavier than mothers who smoke. 


#3. To further provide guidance to patients, the medical provider decided to construct a prediction interval 
# for the birthweight. Consider a patient which has the same attribute values as observation number 2922,
patient <- DATA[2922,]
patient
# Which interval below would be a valid 80% prediction interval?
#(a) 
quantile(DATA$weight, probs=c(.1,.9))
#
#(b)
c( lower = predict(model.c, newdata=patient), 
   upper = predict(model.d, newdata=patient) )
#
#(c)
predict.lm(model.a, newdata=patient, interval="confidence", level = 0.80)
#
#(d)
predict.lm(model.a, newdata=patient, interval="prediction", level = 0.95)


##############################################
#For Questions 4 and 5 we will run quantile regression for 
# quantile indices from .1 to .9 and plot the coefficient processes 
# for each coefficient. (Note that the code can take a while to execute.) 

taus <- seq(from=0.1,to=0.9,by=0.05)
rq_taus <-rq(weight ~ ., tau = taus, data = DATA) 
fittaus_rq <- summary(rq_taus)
plot(fittaus_rq)

#4. Which of the variables is not statistically significant for any quantile index? (i.e., does the confidence band include zero for all quantile indices between .1 and .9?)

#a) cigsper, ed.hs, smoke
#b)	cigsper, ed.hs
#c)	cigsper
#d)	none, all variables are statistically significant for some quantile index

#5. Which of the variables seem to have a heterogeneous impact at different quantile indices? (i.e., which variables do not have a horizontal line in the confidence band?)

#a)	all variables except ed.smcol, ed.hs, smoke
#b)	all variables except ed.hs, smoke
#c)	all variables except smoke
#d)	all variables 

######################################

#6. Run the linear regression model with interactions with the
# variable smoke. 

result_interactions <- glm(weight ~ (.)*smoke, data = DATA)
summary(result_interactions)

#If we are concerned with the interactions that are related to smoking, 
#how many of the interactions are statistically significant .05 level using the conservative rule
#to account for multiple testing? Furthermore, does 
summary(result_interactions)$coef["ed.col:smoke",]
#Estimate   Std. Error      t value     Pr(>|t|) 
#0.0609201649 0.0183690702 3.3164533709 0.0009118416 
#suggest that among mothers with college degree smoking increases (on average) 
# the weight of the child?

#a) Accounting for multiple testing the number of significant variables is
summary(result_interactions)$coef[16:27,4] < .05/27
#Regarding the second question, yes, since the coefficient is positive and statistically significant.

#b)	Accounting for multiple testing the number of significant variables is
sum(summary(result_interactions)$coef[16:27,4] < .05)
#Regarding the second question, yes, since the coefficient is positive and statistically significant.

#c)	Accounting for multiple testing the number of significant variables is
sum(summary(result_interactions)$coef[16:27,4] < .05/12)
#Regarding the second question, no. The coefficient is positive but cannot offset 
#the value of the (negative) coefficient of smoke

#d)	Accounting for multiple testing the number of significant variables is
summary(result_interactions)$coef[16:27,4] < .05/27
#Regarding the second question, no. The coefficient is positive but cannot offset 
#the value of the (negative) coefficient of smoke

