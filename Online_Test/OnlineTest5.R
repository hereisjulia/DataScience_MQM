#######
### R script for Online Test 
### Causal Modelling
### (Reminder: make sure all the files and 
### scripts are in the working directory)
###
### NOTE: The quiz can be taken without running the script
### but you might want to make some additional calculations.
source("DataAnalyticsFunctions.R")


### Questions 1 to 3 are about understanding Rubin's model.

### For questions 4 and 5
### Load the Credit Rating Data
#
CR <- read.csv("CreditRatingtoPost.csv")
summary(CR)
#
## Question 4
##############
#Consider the example of measuring the impact of obtaining credit ratings on the 
# leverage of the firm discussed in class. Consider the calculations based on the 
# data obtained via an observational study 
aggregate(Y ~ d, FUN = mean, data= CR) 
resNaive <- aggregate(Y ~ d, FUN = mean, data= CR) 
resNaive$Y[2] - resNaive$Y[1]
#
installpkg("Matching")
library(Matching)
res <- with(CR, Match( Y=Y, Tr=d, X=profit+finance))
summary(res)
#
#How much is the selection bias (see class slides)?
#a) 0.0823945
#b) 0.1379975
#c) 0.2868934
#d) 0.3248909


#################
## Question 5
################
# Consider the example of measuring the impact of obtaining credit ratings on the 
# leverage of the firm discussed in class. Consider the result for the t-test in the 
# controlled experiment
t.test( Yexp ~ dexp, data= CR )
#
#Is this result consistent with the result obtained via matching based on the propensity score (using the Match function) in the data from the observational study? Why?

#a) No, the estimators are very different (negative confidence interval in the controlled experiment while a positive confidence interval in the observational study), therefore they are not consistent.
#b) No, the estimator for the ATT in the experiment is 0.4086137, while 0.055603 in the other case.
#c) Yes, they are consistent as their difference, 0.0570877-0.055603 is less than 1 SE.
#d) Yes, they are consistent because both are small.  

#################
## Question 6 is not related to any R output.
################
