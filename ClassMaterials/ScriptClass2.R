#######
### R script for Class 2: 
### Predictive Analytics: Regression Models
###
### (Reminder: make sure all the files and 
### scripts are in the working directory)

### Some auxiliary files to load
source("DataAnalyticsFunctions.R")
### This will turn off warning messages
options(warn=0)
#################################################################
### Load infant weight data
###
load("./ClassMaterials/natalityNew.Rda")
###
### This creates a data frame called "d" 
### This data set has 198377 observations
### 19 variables 
#################################################################
### Cleaning up the Data
###
### We need to clean up a little 
### lets look at a summary
summary(d)
### we see that:
### tri.none is all zeros so we will remove it. 
### birmon is all 6 so we will remove it.
### Also, there are redundancies (and identifier number). 
### We will remove the following
drops <- c("id","birmon","tri.none","novisit")
### This creates a dataframe (DATA) from d without the columns in drops
DATA <- d[,!(names(d) %in% drops)]
###
### Now we can see the new summary of the data
summary(DATA)
###
##################################################################
### Question 1 - Visualization
###
### We will consider visualizing:
### smoking patterns during pregnancy vs. education
### 
### Education variables: DATA$ed.hs, DATA$ed.smcol, DATA$ed.col, 
### Smoking Data: DATA$smoke DATA$cigsper
###
### Even simple things help us getting started to find what we want 
### to do.
### First what is the proportion of mothers that smoke?
mean(DATA$smoke)
### Now lets see how this proportion breaks down by education.
### No High school
mean(DATA$smoke[DATA$ed.hs+DATA$ed.smcol+DATA$ed.col==0])
### With High school
mean(DATA$smoke[DATA$ed.hs>0])
### With Some College
mean(DATA$smoke[DATA$ed.smcol>0])
### With College
mean(DATA$smoke[DATA$ed.col>0])
### Clearly an interesting decreasing pattern.
### Arguably expected but the quantification of it is interesting
### Lets create a single variable that codes all education levels
### so we can plot.
Education <- rep(0,length(DATA$smoke)) ### Code no HS as 0
Education[DATA$ed.hs>0]    <- 1        ### Code HS    as 1 
Education[DATA$ed.smcol>0] <- 2        ### Code SMCOL as 2       
Education[DATA$ed.col>0]   <- 3        ### Code COL   as 3
### Read as factors (and not numbers)
Education <- factor(Education, levels=c(0,1,2,3))  
### Associate Labels with each level of the factors
levels(Education) <- c("No High School","High School","Some College","College")
### this call of "plot" displays the proportion of smokers broken down by education
par(mar=c(1.5,1.5,1.5,1.5))
par(mai=c(1.5,1.5,1.5,1.5))
plot(factor(DATA$smoke) ~ Education, col=c(8,2), ylab="Smoking") 
### 
### Note that 0 and 1 is not that friendly... unless you are a quant 
### person.
###
plot( factor(DATA$smoke==1) ~ Education, col=c(8,2), ylab="Smoking") 
### sligthly better...  
AuxSmoke <- rep("",length(DATA$smoke)) 
AuxSmoke[DATA$smoke==1]    <- "Yes"
AuxSmoke[DATA$smoke==0]    <- "No"
plot( factor(AuxSmoke) ~ Education, col=c(8,2), ylab="Smoking") 

###
### How about the actual cigarretes per day
### Can we also visualize that?
### For instance the histogram of cigs/day for smokers
hist(DATA$cigsper[DATA$smoke>0], breaks = 40, xlab = "Cigarretes/day", main="Histogram of Cigs/day for Smokers")
### We see some peaks. Bins might not be very well adjusted
### Lets look at each demographics of education
### and also plotting a "smooth" histogram, i.e. the density function
### Lets manipulate the data first
CigNoHS <- data.frame(CigarretesPerDay = DATA$cigsper[DATA$smoke>0 & (DATA$ed.hs+DATA$ed.smcol+DATA$ed.col==0)])
CigHS <- data.frame(CigarretesPerDay = DATA$cigsper[DATA$smoke>0 & DATA$ed.hs>0])
CigSmCol <- data.frame(CigarretesPerDay = DATA$cigsper[DATA$smoke>0 & DATA$ed.smcol>0])
CigCol <- data.frame(CigarretesPerDay = DATA$cigsper[DATA$smoke>0 & DATA$ed.col>0])

#Now, combine your four dataframes into one.  First make a new column in each.
CigNoHS$edu <- 'No High School'
CigHS$edu <- 'High School'
CigSmCol$edu <- 'Some College'
CigCol$edu <- 'College'
#and combine into your new data frame vegLengths
EduCig <- rbind(CigNoHS,CigHS,CigSmCol,CigCol)

#now make a density plot (smooth histogram) based on ggplot2
installpkg("ggplot2")
library(ggplot2)


ggplot(EduCig, aes(CigarretesPerDay, fill = edu))+ 
     geom_density(aes(colour = edu), alpha = 0.2)+
     coord_cartesian(xlim = c(0, 50)) 


### Very curious the bias for round numbers
### 5, 10, 15, 20, 30, 40
### This shows a bit more mixed picture but College and Some College 
### Tend also to smoke less
###########################################
##########################################################################################
#### Question 2 - Next consider the 10 dummy (binary) variables in Exhibit 1. 
####              Test significance with 0.05 rule and with FDR q=0.001.
####              If discrepancies, which one do you think got it wrong?
MatrixComp <- as.matrix( cbind( DATA$boy, DATA$tri1, DATA$tri2, DATA$tri3, DATA$black, DATA$married, DATA$ed.hs, DATA$ed.smcol, DATA$ed.col, DATA$smoke ))  
LabelsTmp <- c( "boy", "tri1", "tri2", "tri3", "black", "married", "ed.hs", "ed.smcol", "ed.col","smoke")
NumCol <- ncol(MatrixComp)
pvals <- rep(0, NumCol*(NumCol-1)/2) 
ListLabels <- rep("", NumCol*(NumCol-1)/2) 
k <- 0
for (i in 1:(NumCol-1) ){
  for ( j in (i+1):NumCol ){
    k <- k+1
    m00 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 0) ) 
    m01 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 1) ) 
    m10 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 0) ) 
    m11 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 1) ) 
    # Construct the contingency table
    ContingencyMatrix <- as.table(rbind(c(m00, m01), c(m10, m11)))
    ### perform the Pearson chi squares test for independent of factors
    pvals[k] <- chisq.test(ContingencyMatrix)$p.value  # store the p-value of the test
    ListLabels[k] <- paste(LabelsTmp[i],LabelsTmp[j], sep=" and ")  # create the Label
  }  
}
### How many tests are we performing after all?
length(pvals)
### How many of those are significant at 0.05?
sum( (pvals <= 0.05) )  
### so it looks like the number of independet pairs is
length(pvals)-sum( (pvals <= 0.05) )
###
### Lets see what Benferroni correction gives us.
N <- length(pvals)
cutoff <- 0.05/N
signif <- (pvals <= cutoff) 
### Accordingly to this rule, the number of significant is
sum(signif)
### and accordingly to this rule the number of independent pairs is
length(pvals)-sum( (pvals <= cutoff) )

### We can list the top 2 combinations 
topindices <- order(pvals)[1:2]
cbind( ListLabels[topindices], pvals[topindices])
### We can list the bottom combinations 
### Say the last 9 coefficients
### (recall that after the loop, k = NumCol*(NumCol-1)/2)
### we can order p-values via order(pvals) and get the components [(k-9):k]
bottomindices <-order(pvals)[(k-8):k]
cbind(ListLabels[bottomindices],pvals[bottomindices])
### Recall that the cutoff is
cutoff
### So the first one is significant while the other 9 are not.
###
### Indeed, all variables are independent from "boy" 
### but the cut-off 0.05 gets "boy" to be related with "married" and "black"
## in this specific example (because of "biological reasoning")
## we believe that there were 2 false positives when using cutoff = 0.05


###########################################
###########################################
### Question 3 - In your opinion, can any of the variables provided 
### in Exhibit 1 help to predict birthweight? 
### Provide (a simple) evidence that supports your view based on data.
### 
### Yes! It sonds very likely that some of these variables we
### will have some explanatory value. Simple measure: correlation.
### It seems reasonable that knowing if it is a "Boy" or not would help to predict.
cor( DATA$boy,DATA$weight )
### Yes! Correlation 0.1. It definitely has some predictive power.
### Technically this already drives the point home. 
###
### Lets compute the correlation matrix (just need to call cor with the dataframe)
CorMatrix <- cor(DATA)
### the command CorMatrix <- cor(DATA, DATA) would work in the same way
### First Column of that matrix corresponds to weight vs. all variables:
CorMatrix[,1]
### Although small, all variables seem to be correlated with weight. 
###
###
summary( glm( DATA$weight ~ DATA$boy ) )
### p-value leads to a statistically significant variable.
###
### Since we are talking about corelations we can visualize things a bit better:
### install and load package for displaying correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(CorMatrix, method = "square")
corrplot(CorMatrix, method = "ellipse")
corrplot(CorMatrix, method = "circle")
corrplot(CorMatrix, method = "square")
### for other visualizations in this package
### check: http://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
###
##########################################################################################


##########################################################################
### Question 4 Run a linear regression 
###
### The response variable Y will be "weight" (dependent, target)
### we will add all the other variables in the regression.
result <- glm(weight ~ ., data = DATA) 
###
### Lets see the summary of the model with coef and p-values
summary(result)
### All p-values are very significative! Largest p-value is 3.84e-05. 
### Both rules accept all coefficient
###
### For the conservative rule, significance is 
### p-value <= 0.05 / # of tests
### then the following command counts hot many were not significant
sum( summary(result)$coef[,4] > 0.05/length(summary(result)$coef[,4]) )
### All significant!
###
### Note that other rules that are less conservative will also lead to 
### significance of all regressors.
###
########################################################################
### It is more interesting to look at simultaneous confidence intervals
### Using Bonferroni correction
### the critical value should be 
qnorm(1-(0.025/14))
### instead of the qnorm(1-0.025) = 1.96
###
### For regressions I will suggest you to do simultaneous confidence intervals
### via bootstrap.
### Simultaneous Confidence Intervals in the following source code

source("GaussianMultiplierBootstrapLinearReg.R")
xxx<- model.matrix(weight ~ .-1, data = DATA) 
### this create a matrix with columns based on DATA withou weight;the -1 removes the intercept
yyy<- DATA$weight
indices <- seq(from=1,to=ncol(xxx)) ### columns in "xxx" you want to be simultaneous 
rep <- 2000  ### number of bootstrap replication, typically we want 1000
res_bootstrap <- GMbootstrapLinearRegression(yyy,xxx,indices, repetitions=rep, lvl=c(0.68,0.9,0.95))
### confidence intervals are constructed as usual: PE +/- cv * se
### where the critical values cv are chosen so the intervals  **simultaneously**
### contain all the parameters in indices with the pre-specified confidence.
### PE (point estimate), se (standard errors) and can be found in
res_bootstrap$simultaneous
### 
### and the critical values were computed for 68%, 90% and 95% 
res_bootstrap$cv
###
### recall that the standard 95% confidence interval used cv = 1.96
### if one used the conservative rule, to compute the 95% confidence interval
### we need to be 99.64% confident for each interval (so that 0.36% * 14 ~= 5% )
qnorm(1-(0.025/14))
### which leads to 3% less in precision. It does not sound much for now.
### 14 confidence intervals are not that many as we will see later.
###########################################################################
###
### Class 2 starts here 
###
### For completeness we rerun the linear regression here...
### The response variable Y will be "weight" (dependent, target)
### we will add all the other variables in the regression.
result_lm <- lm(weight ~ ., data = DATA) 
result <- glm(weight ~ ., data = DATA) 
###
### Lets see the summary of the model with coef and p-values
summary(result)
### you just ran a regression with 200K observations!
### If you run lm you see the R squared directly in the summary.
### Using glm you can compute the Rsquare as
1 - (result$dev/result$null)
### where result$dev is the deviation of the fitted model
### and the result$null is the deviation of the fitted null model
###
### Lets look at the summary of the model with coef and p-values
summary(result)
###
### Interpretation of coefficient:
### For example:
### boys are heavier on average (additional 110g)
### "married": additional 60g on average for married mothers
### The impact of education on average: 
### high school yields additional 16g
### some college yields additional 32g
### college yields additional 38g
###
### Statistical Significance:
### Since the p-value of many coefficients is <2e-16 
### (this is scientific notation, <2{10^(-16)}, VERY SMALL )
### The sample size is very large so p-values reflect that
### we are very confident that there is an effect

###
## fit plots and R^2 
## (the 'bty="n"' option removes boxes around your plot)
color<-c("pink","blue")
plot(result$fitted ~ DATA$weight, ylim=c(2,4.5),  xlab="Actual Birthweight", ylab="Fitted Birthweight", col=color[1+DATA$boy], bty="n")
abline(a=0,b=1)#  add a line with slope 1, intercept 0
legend("bottomright",legend=c("Girl","Boy"),fill=color, bty="n")
cor(result$fitted,DATA$weight)^2
###
###
### Forecast with Linear Regression
###
### First we need to have the features for which we want to predict.
### those should be on the same format as the data 
### (i.e. same columns) for this example lets define 
### based on the first three observations DATA[1:3,] 
DataToPredict <-DATA[1:3,]
### DataToPredict has the observarions we want to forecast
### To get point estimates (plug in the line)
predict(result, newdata=DataToPredict)
### To get standard errors we add a command
predict(result, newdata=DataToPredict, se.fit=TRUE)
### we can get confidence intervals for the predictions easily
predict.lm(result, newdata=DataToPredict, interval="prediction", level = 0.95)
### it provides the point estimate (fit), lower end (lwr), and upper end (upr) 
### Note that the warning is because the model "result"
### was computed with a "glm" function instead of the "lm" function
### We get the same numbers if we use "result_lm" (but no warning)
###
predict.lm(result_lm, newdata=DataToPredict, interval="prediction", level = 0.95)
###
###
###
### Now we will move to Quantile Regression
###
### It is not about the mean. It is about quantiles
### we could focus on a low quantile (say tau = 0.1)
### we could focus on a high quantile (say tau = 0.9)
### It will be always of interest to find "tail effects" 
### that are move severe than "median effects"
### Potentially, linear regression would miss those effects.
###
### We begin by installing the packages for quantile regression
installpkg("quantreg")
library(quantreg)
###############################################################

### the command rq runs the quantile regression
### you need to specify the quantile index, say tau = 0.5 for the median
### (This will take a few minutes. The quantile regression requires more
### computational power than multiple regression. One of the reasons why
### multiple regression was used more widely.)
median <- rq(weight ~ ., tau = 0.5, data = DATA) 
### Deviance for the fitted model:
### sum of the absolute value of all residuals
tau <- 0.5
n <- length(resid(median))
DevModel <- sum( (1-tau)*pmax(rep(0,n),resid(median))+tau*pmax(rep(0,n),-resid(median)))
### Deviance for the null model that has no variables
### sum of the absolute value of weight - median(weight)
Dnull <- sum(tau*pmax(rep(0,n),DATA$weight-quantile(DATA$weight,probs=tau))+(1-tau)*pmax(rep(0,n),-DATA$weight+quantile(DATA$weight,probs=tau)))
### R squared
1-DevModel/Dnull


### Smaller than in the regression (R^2=0.11) but these explain very different things!
### Always remember not to over interpret R2. 

### Now lets turn to the coefficients.
### Compare with linear regression coefficietns with the median regression (take a moment and read them)
cbind( coef(result) , coef(median) )
### Very similar! This suggests that the conditional mean and conditional
### median are similar.
### However, look at the lower tail say tau = 0.05
lower.tail <- rq(weight ~ ., tau = 0.05, data = DATA) 
coef(lower.tail)
### Look at the upper tail say tau = 0.95
upper.tail <- rq(weight ~ ., tau = 0.95, data = DATA) 
coef(upper.tail)
### the impact of married doubled! the impact of doctor visits tripled!
### education much more important!
### This is a key modeling feature of quatile regression. We can get middle, tail or both
### aspects of the target variable.
###
###
### Lets look at multiple quantiles
### set the list of quantile indices = 0.1, 0.2, 0.3,..., 0.9
### In-class pictures were generated with
### taus <- seq(from=0.025,to=0.975,by=0.005)
### This takes a long time (great for offline)
### I suggest you run with 
taus <- seq(from=0.1,to=0.9,by=0.05)
### It will still take some time...
### fit the models for these quantile indices
rq_taus <-rq(weight ~ ., tau = taus, data = DATA)
fittaus_rq <- summary(rq_taus)
### fittaus_rq[1] stores the solutions
### for the first value of tau (=0.1)

### The following command plots each of the 15 coefficient 
### for all quantile indices in taus
plot(fittaus_rq)
### Maybe too many? Lets plot specifically the ones we discussed in class
### First the coefficient of Married
plot(fittaus_rq, parm = 3, xlab="Quantile Index", ylab="Coefficient Value",main="Coefficient for Married ")
### Keep in mind that Married status can be seen as a proxy for a stable relation
### This was more true a few years ago (which is the case of the data)
### than it is now with many more non-married people on stable relations
### In any case, the red lines pertain to the linear regresion model
### the gray and black pertains to the quantile regression.
### Of course the linear regression does not change with tau so it is a flat line
### The black curve is the point estimate while the gray band is the confidence region
### The analysis of the quantile regression tells you that Married 
### increases the weight across the whole distribution
### however the positive impact is much larger (3 times) when you need it the most
### i.e., in the lower tail of the distribution. In the upper tail, the impact 
### is not so dramatic. This provides strong evidence for heterogeneous effect
### of Married in the birthweight 
### (in opposition to the constant shift impact that linear regression measures)
###
### Next lets look at education. We have three variables for the highest education level:
### High School; Some College; College
### We will make one plot for each and compare the impact of education as it changes
plot(fittaus_rq, parm = 8, main="Coefficients for High School",xlab=expression(tau), xlim=c(0.1,0.9), ylim=c(-0.01,0.08))
plot(fittaus_rq, parm = 9, main="Coefficients for Some College",xlab=expression(tau),xlim=c(0.1,0.9), ylim=c(-0.01,0.08))
plot(fittaus_rq, parm = 10, main="Coefficients for College",xlab=expression(tau),xlim=c(0.1,0.9), ylim=c(-0.01,0.08))
### Recall that the red lines pertains to the linear regression.
### We see that the impact on the conditional mean seems to increase with education level
### and it is statistically significant.
### In the plot for the variable  highschool, we see that the impact across quantile seems to be constant.
### suggesting a location shift (homogeneous impact) of high school
### However, for college we have strong evidence 
### (since the gray region is far from the red lines)
### that the impact is not the same across different quantile levels. Again
### the larger impact is when you need the most (lower tail). 
### It does not seem to help much when you are on the good side. Suggesting that
### people are well informed and if they know that things are going well they 
### do not need to put additional effort. In contrast, being well informed and knowing
### that things might not be doing so well it leads you to have a more proactive 
### attitude which in turn makes you have a higher impact.

### Finally, we have the coefficients for pre natal care
plot(fittaus_rq, parm = 5, main="Coefficients for visits up to first trimester",xlab=expression(tau), ylim=c(0.05,0.375))
plot(fittaus_rq, parm = 6, main="Coefficients for visits up to second trimester",xlab=expression(tau), ylim=c(0.05,0.375))
plot(fittaus_rq, parm = 7, main="Coefficients for visits up to third trimest",xlab=expression(tau), ylim=c(0.05,0.375))

### Next we will discuss the forecasting 
### and the estimation of the probability distribution function 
### conditionally on X
###
### Create two units to forecast based on the first observation
### One weill be "low" as we will set education level to be low
### and smoking
x.low <- (DATA[1,-1])
x.low$ed.smcol <- 0 ## but we set no high school
x.low$smoke    <- 1 ## smoking
x.low$cigsper  <- 14 ## 14 cigarettes/day
### the second will be "high" as we will set the education level to be high
### and smoking to be low
x.high <- DATA[1,-1]
x.high$ed.smcol <- 0
x.high$ed.col <- 1  ### put college

## This computes the quantiles X'beta_tau 
## for each of the two cases
qs.low <-predict(rq_taus, newdata=x.low)
qs.high <-predict(rq_taus, newdata=x.high)

### Lets plot the quantile function for the "high"
plot( taus,qs.high,lwd=2, type ="l", col = "blue",xlab=expression(tau),ylab="", main="Quantile function (Kg)")

### Lets plot the quantile functions for both (using step function) in the same plot
### the command "add=TRUE" plots on top of the previous plot
plot( c(taus,taus), c(qs.low,qs.high), type ="n", xlab=expression(tau), ylab="Quantile (Kg)" )
plot( stepfun(taus,c(qs.low[1],qs.low)),do.points=FALSE,  add=TRUE, col.hor = "red", col.vert="red"  )
plot( stepfun(taus,c(qs.low[1],qs.high)),do.points=FALSE, add=TRUE, col.hor = "blue", col.vert="blue"  )

### Compute the density
ps.wts <- (c(0,diff(taus)) + c(diff(taus),0)) / 2
a.low <- akj(qs.low,qs.low,ps.wts)
a.high <- akj(qs.high,qs.high,ps.wts)
### Lets plot the density (recall that the support of the density is the quantiles)
### this just fix the axis so we can plot them both together
plot(c(qs.low,qs.high), c(a.low$dens, a.high$dens), type="n",xlab="Weight (Kg)", ylab="Density")
lines(qs.low,a.low$dens,col="red")
lines(qs.high,a.high$dens,col="blue")


###################################################
### Scale (log transformation)
CC <- read.csv("GDP-ImportALL.csv", header=TRUE)
summary(CC)
plot(CC$IMPORT, CC$GDP, type ="n", xlab="IMPORTS", ylab="GDP")
points( CC$IMPORT, CC$GDP, cex=1)
text( CC$IMPORT, CC$GDP, CC$Country.Code,cex=1)

plot(log(CC$IMPORT), log(CC$GDP), type ="n", xlab="log(IMPORTS)", ylab="log(GDP)")
points( log(CC$IMPORT), log(CC$GDP), cex=1)
text( log(CC$IMPORT), log(CC$GDP), CC$Country.Code,cex=1)
#
###################################################
### Interactions
### next lets create an interaction term of smoke with all other variables
### the formula weight ~ (.)*smoke means use Y= weight and 
### X = all the variables + interaction of smoke and all the variables
result_interactions <- glm(weight ~ (.)*smoke, data = DATA) 
summary(result_interactions)
### note that R expluded the interaction smoke:cigsper, output
### smoke:cigsper          NA         NA      NA       NA
### because smoke:cigsper = cigsper and would be a multicolinearity problem
### if you want the variables used in the regression as a matrix
X <- model.matrix (weight ~ (.)*smoke, data = DATA) 
### Here X is a matrix with all the columns used in the regression with all the interactions
ncol(X)
### total of 28 columns which includes the intercept.


#####################################################
### Example of the local polynomial 
### a nonparametric procedure to estimate the conditional mean
### First I display the smooth scatter plot to see the data
smoothScatter(DATA$m.wtgain,DATA$weight, xlab="Weight gain (lbs)", ylab="Birthweight (kg)", ylim = c(3,4))
### this plots the estimated conditional mean based on
### the local polynomials.
lines(lowess(DATA$m.wtgain,DATA$weight,f=.2),col="red")
### We see the nonlinear pattern that would be hard to 
### capture in a linear regression (unless we add 
### additional nonlinear terms)



