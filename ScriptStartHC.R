#######
### Starter Script for Case 1: 
### Reminder: make sure all the files and 
### scripts are in the working directory
###
### List of files:
### natalityNew.Rda
### DataAnalyticsFunctions.R
###
### Keep in mind that:
### Questions 1, 3 and 4 can be solved after the Cleaning up of the data below.
### Question 2, I provided some initial code to create a list of p-values (and labels)
### Questions 5 and 6 do not need additional data analysis
###########################################

### Initialization with Cleaning up of Data ####
###
### Load auxiliary R file
source("DataAnalyticsFunctions.R")
######################################################
### Load infant weight data
load("natalityNew.Rda")
### This creates a data frame called "d" 
### This data set has 198377 observations
### 19 variables but we need to clean up a little
### Lets see a summary of the data
summary(d)
######################################################
### The following code will clean up the data for you
###  
###
### tri.none is all zeros so we will remove it. 
### birmon is all 6 so we will remove it.
### Also, there are redundancies (and identifier number). 
### We will remove the following varaibles: "id","birmon","tri.none","novisit"
### to do that lets create a vector with those names
drops <- c("id","birmon","tri.none","novisit")
### names(d) has all the column names. We want to REMOVE the names in drops
### names(d) %in% drops tells you which entry of names(d) is in drops
names(d) %in% drops
### this is a value of true/false for each column
### since we want "not in" we reverse that with the command "!"
!( names(d) %in% drops )
### we can use the logical vector above to select the columns
### the following command creates a dataframe (called DATA) 
### which has all the columns from d not in drops
DATA <- d[,!(names(d) %in% drops)]
summary(DATA)
######################################################
### End of data clean up
###
######################################################
###
### You can start with Questions 1, 3, 4 
### (recall that 5 and 6 do note need additional analysis)
###
######################################################
###
### Organizational help for Question 2 
### 
### This creates a matrix with only the 10 binary variables 
MatrixComp <- as.matrix( cbind( DATA$boy, DATA$tri1, DATA$tri2, DATA$tri3, DATA$black, DATA$married, DATA$ed.hs, DATA$ed.smcol, DATA$ed.col, DATA$smoke ))  
### Here is the associated LAbels (for convenience)
LabelsTmp <- c( "boy", "tri1", "tri2", "tri3", "black", "married", "ed.hs", "ed.smcol", "ed.col","smoke")
### Number of columns (should be 10)
NumCol <- ncol(MatrixComp)
### Next we compute the p-values for each pair
pvals <- rep(0, NumCol*(NumCol-1)/2) 
### Also will collect the pair label
ListLabels <- rep("", NumCol*(NumCol-1)/2) 
k <- 0
for (i in 1:(NumCol-1) ){
  for ( j in (i+1):NumCol ){
    k <- k+1
    ### Creates the entries of the contingency table
    m00 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 0) ) 
    m01 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 1) ) 
    m10 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 0) ) 
    m11 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 1) ) 
    ### Construct the contingency table
    ContingencyMatrix <- as.table(rbind(c(m00, m01), c(m10, m11)))
    ### Perform the Pearson chi squares test for independent of factors
    # store the p-value of the test 
    pvals[k] <- chisq.test(ContingencyMatrix)$p.value  
    # create the Label
    ListLabels[k] <- paste(LabelsTmp[i],LabelsTmp[j], sep=" and ")  
  }  
}
###############################################################
### Now you have:
### a list of p-values; and
### a list of labels to help you identify which are the variables 
###
### pvals: is a vector with 45 p-values each associated with an independency test
### ListLabels: is a vector with the labels of the 2 variables used on each the independence test
###
### for example  
### the third test pertains to the pair 
ListLabels[3]
### which p-value is
pvals[3]
################################################################
### You can proceed to Question 2 
################################################################
