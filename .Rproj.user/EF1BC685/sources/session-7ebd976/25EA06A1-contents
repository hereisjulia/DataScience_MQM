#######################################################
### Undercutting ALLSTATE online Case
###
###
### Load additional file to install packages
source("DataAnalyticsFunctions.R")
###
###
### Load ALLSTATE DATA FILE
ALLcost <- read.csv("ALLSTATEcost.csv")

###
###
### Data Preparation ###########################
################################################
###
### Lets see a summary of it
###
summary(ALLcost)
### there are plenty of things to decide here.
### Which variables have NA's:
### risk_factor also has NA (that should be also a level)
### duration_previous it has 0 and NA's we probably need to treat them differently.
### C_previous
### location
### 
### Lets print the first 6 data points
ALLcost[1:6,]
### we see that the first customer requested 2 quotes
### if we are predicting the behavior of the customer, we should take that in consideration
### but first we will predict the cost quoted by ALLSTATE
### so we start by assuming it does not discriminate across used id and shopping_pt (an assumption)
drop <- c("customer_ID","shopping_pt","record_type","time","location")
### This creates a dataframe (DATA) from d without the columns in drops
DATA <- ALLcost[,!(names(ALLcost) %in% drop)]
###
DATA$car_value <-  factor(DATA$car_value)
DATA$day <-  factor(DATA$day)
DATA$state <-  factor(DATA$state)
duration_NA <-  ifelse( is.na(DATA$duration_previous) , 1, 0 )        ### creating a dummy variable for NA
### number of NA in duration
sum(duration_NA)
### corresponds to 5% of the sample 783/15483
sum(duration_NA)/length(duration_NA)
### It is not that big and we could just drop them in a first analysis
### however we wil create a dummy variable
DATA$duration_previous[duration_NA>0] <-0 ### making NA to zero
### lets look at C_previous
C_NA <-  ifelse( is.na(DATA$C_previous), 1, 0 )        ### creating a dummy variable for NA
### how many?
sum(C_NA)
### very suspecious...
cor(C_NA,duration_NA)
### HAHA... the same observations that do not report previous duration...
### Lets treat C_previous as factor
DATA$C_previous[C_NA>0] <-0 ### making NA to zero
DATA$C_previous <-  factor(DATA$C_previous)                           
### Lets look at risk_factor as well...
risk_NA <- ifelse( is.na(DATA$risk_factor), 1, 0 )
sum(risk_NA)
### The NA for those are different observations...
DATA$risk_factor[risk_NA>0] <-0                     
### treat thatas a level "0" (a new category of risk...)
DATA$risk_factor <-  factor(DATA$risk_factor)                           
###
DATA$homeowner <-  factor(DATA$homeowner)
DATA$married_couple <-  factor(DATA$married_couple)
summary(DATA)
### there should be no NA's in the data at this point....

#################################
#### Question 1: Visualization
#################################
#### A suggestion is to go back to Class 1 and Class 2 Scripts
#### No additional hints for visualization at this point

#################################
#### Question 2: A first linear Regression Model. 
####             Feel free to use this or improve upon it.
#### this is a linear regression with all the variables in DATA
result <- glm(cost ~ ., data = DATA) 
### and to see the results (coefficients, p-values, etc...)
summary(result)
### the R-squared in this case is 
1 - (result$dev/result$null)

### As a side note, note that when running regressions, 
### sometimes R creates new columns automatically to run the regression
### for example, it creates dummies for you if you have columns that are
### factors. To get matrix with all these columns explicitly created 
### for the following regression
result <- glm(cost ~ ., data = DATA)
### simply use the command "model.matrix" as follows
M <- model.matrix(cost~., data = DATA)
summary(M)
### thus the same regression can be run as
resultM <- glm(DATA$cost ~ M)
### Just to make sure, lets see that R2 match...
1 - (resultM$dev/resultM$null)
###
### By having the design matrix you can easily drop or add variables
### based on any rule you like to use.
### For example, if you want to use only the first 5 columns of M
### you simple call with M[,1:5]
resultM5 <- glm(DATA$cost ~ M[,1:5])

#### Another model one can consider is the one that 
#### would include interactions based 
#### on the the coverage options A through G
#### we can add those interactions in addition to the previous variables
result_interactions <- glm(cost ~ .+(A+B+C+D+E+F+G)^2, data = DATA) 
summary(result_interactions)
#### this has all the variables plus all the interations 
####
####

#################################
#### Questions 3 is conceptual questions about modeling framework.
#### No data analysis expected.
#################################

#################################
### Question 4 Provide quotes for new.customers
#################################

## The following command loads the "new.customers" to memory
## it is already formatted in our conventions 
## (where NA's in some variables were turned to level "0")
new.customers <- readRDS("NewCustomers.Rda")
##
str(DATA)
str(new.customers)
"group_size","car_age","age_oldest","age_youngest", "duration_previous","A", "B", "C"                
[16] "D"                 "E"                 "F"                 "G"                 "cost"
predict (result, newdata = new.customers)


#################################
#### Question 5: No start script

