#######
### R script for Causal Modelling Class 
### 
### (Reminder: make sure all the files and 
### scripts are in the working directory)
###
###
source("DataAnalyticsFunctions.R")
##############
## The code below illustrate what to do when
## the treatment is a continuous variable and
## the number of controls in X is very large
##
##############
### Freakonomics: Abortion and Crime

####### donohue and levitt 2001/2008: abortion and crime

## example reading non csv data: this is a dump from STATA
## skip says skip the first line of the file, sep="/t" says 'tab separated'
data <- read.table("abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
                 "a_murd","a_viol","a_prop",'prison','police',
                 'ur','inc','pov','afdc','gun','beer')

## prison: log of lagged prisoners per capita
## police: the log of lagged police per capita
## ur: the unemployment rate
## inc: per-capita income
## pov: the poerty rate
## AFDC: generosity at year t-15
## gun: dummy for concealed weapons law
## beer: beer consumption per capita 

data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are quite different
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year-85
t2 <- t^2
s <- factor(data$state) ## the states are numbered alphabetically

controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
summary(data)
### if we focus on murder we use
y <- data$y_murd
d <- data$a_murd
### if we focus on violent crime we use
y <- data$y_viol
d <- data$a_viol

## The abortion 'a_' variables are weighted average of abortion rates where
## weights are determined by the fraction of the type of crime committed by
## various age groups. For example, if 60% of violent crime were committed by 18
## year olds and 40% were committed by 19 year olds in state i, the abortion rate
## for violent crime at time t in state i would be constructed as .6 times the
## abortion rate in state i at time t ?^' 18 plus .4 times the abortion rate in
## state i at time t ?^' 19. See Donohue and Levitt (2001) for further detail:
## "effective abortion rate" per 1,000 live births. The "effective abortion rate" is the weighted average of the
## abortion rates of the birth cohorts in a state, with the weights determined by the share of total
## arrests nationally for a particular crime category of individuals of that age

### We will consider 3 models
### Simple regression   y ~ d
### Mutiple regression  y ~ d + X (DL)
### Double Selection that also accounts for the assignment mechanism

### Before running DL model, lets look at the simplistic model without controls
summary(simple <- glm(y ~ d, data=controls) )$coef['d',]


## note for convenience here I've made y,d,t, global: they are not in controls.
summary(orig <- glm(y ~ d+t+s+., data=controls) )$coef['d',]
## this is the levitt analysis: higher abortion leads to lower crime

## Now the same analysis, but for cellphones rather than abortion
cell <- read.csv("us_cellphone.csv")
cellrate <- (cell[,2]-cell[1,2])/(cell[13,2]-cell[1,2])
## what if we're just fitting a quadratic trend?
## there are many things that increased with similar shapes over time
## (cellphone usage, yoga revenues, home prices, ...)
plot(1985:1997, tapply(d, t, mean), xlab="year", ylab="adjusted rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
phone <- cellrate[t+1]
## clearly, cellphones fight crime.
summary(tech <- glm(y ~ phone+t+s+., data=controls))$coef['phone',]

## what is happening here is that murder has been increasing quadratically,
## and we have no other controls that do so.  To be correct, you need
## to allow quadratic trends that could be caused by other confounding variables (e.g. technology)

## each state should also have a different baseline linear and quadratic trend
## and, at very least, controls should interact with each other.
## Here's a model that removes more confounders
# (no intercept, since we've removed the reference level from state)
summary(interact <- glm(y ~ d + s*(t+t2+.^2), data=controls))$coef['d',] 
## It's broken! The issue is that we've built more controls than observations
dim(model.matrix(y ~ d + s*(t+t2+.^2), data=controls))
## Quadratic trend interaction leads to more variables than observations
## if we were stopped by this, we'd never be able to discover anything!

## so we need a way to select only important controls
## try using a lasso 
## refactor state to have NA reference level
s <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x = model.matrix(~ s*(t+t2+.^2), data=controls)[,-1]
dim(x)
### indeed more variables than sample size.

#####
##### Double Selection Procedure for Robust confidence Intervals
##### 3 steps
##### 1. run Lasso of Y on controls X
##### 2. run Lasso of d on controls X
##### 3. Take all selectedX (Step 1 and 2) and run Linear regression of
#####         Y on treatment d and selectedX
#####
installpkg("glmnet")
library(glmnet)

##
## I have strong reasons to prefer the Theoretical Penalty Choice
## instead of Cross Validation Here.
## It turns out that CV focus too much in prediction and overfits
## (if you want to use CV, at least use the 1se rule)
num.features <- ncol(x)
num.n <- nrow(x)
### Step 1
## we set penalty level using the theoretical choice
supp1 <- which.max(abs(cor(x,y)))
res <- glm(y~x[,supp1])
w <-sd(res$residuals)
lambda.theory1 <-w* qnorm(1- (0.01/num.features)) *sqrt(1/num.n)
### Similar to
### lambda.theory1 <- w*sqrt(2*log(num.features/0.01)/num.n)

## call Lasso 
lassoTheory1 <- glmnet(x,y,lambda = lambda.theory1)
## get the support
supp1 <- support(lassoTheory1$beta)
### Step 1 selected
length(supp1)
colnames(x[,supp1])
### controls
###
### Step 2
supp2 <- which.max(abs(cor(x,d)))
res <- glm(d~x[,supp2])
w <-sd(res$residuals)
lambda.theory2 <-w* qnorm(1- (0.01/num.features)) *sqrt(1/num.n)
### similar to
### lambda.theory2 <- w*sqrt(2*log(num.features/0.05)/num.n)

lassoTheory2 <- glmnet(x,d,lambda = lambda.theory2)
supp2<-support(lassoTheory2$beta)
### Step 2 selected
length(supp2)
### controls
colnames(x[,supp2])
###
### Step 3
inthemodel <- unique(c(supp1,supp2)) # unique grabs union
selectdata <- cbind(d,x[,inthemodel]) 
selectdata <- as.data.frame(as.matrix(selectdata)) # make it a data.frame
dim(selectdata) ## p about half n

## run a a linear regression of Y on d and all selected
causal_glm <- glm(y~., data=selectdata)
## The theory actually says that the standard SE calc for gamma is correct!
## despite the model selection
summary(causal_glm)$coef["d",]
##
## Not significant!
## 
## to make your life easier, I implemented a function
source("causal_source.R")
CausalLinear(y,d,x)



#### DML with Lasso
### Step 1.
### lassoTheory is the LASSO model selected
V <- y - predict(lassoTheory1,newx=x)
### Step 2.
### lassoTheory is the LASSO model selected
W <- d - predict(lassoTheory2,newx=x)
### Step 3.
summary(glm( V ~ W ))$coef["W",]

#### DML with RF
#### Note that there is some randomness in RF
#### therefore the precise numbers are likely to differ
installpkg("randomForest")
library(randomForest)
### Step 1.
### lassoTheory is the LASSO model selected
n <- nrow(x)
datarf <- data.frame(x,y)
predy <- rep(0,n)
rf <- randomForest( y ~ ., data = datarf)
predy <- predict(rf, newdata=datarf)
### Step 2.
datarf <- data.frame(x,d)
predd <- rep(0,n)
rf <- randomForest( d ~ ., data = datarf)
predd <- predict(rf, newdata=datarf)
### Step 3.
V <- y-predy
W <- d-predd
summary(glm( V ~ W ))$coef["W",]


### DML with Split Sampling
#### Note that there is some randomness in RF
#### therefore the precise numbers are likely to differ
installpkg("randomForest")
library(randomForest)
nfold <- 2 
n <- nrow(x) # the number of observations
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
datarf <- data.frame(x,y)
predy <- rep(0,n)
for ( id in 1:nfold ){
rf <- randomForest( y ~ ., data=datarf[which(foldid!=id),])
predy[which(foldid==id)] = predict(rf, newdata=datarf[which(foldid==id),])
}
datarf <- data.frame(x,d)
predd <- rep(0,n)
for ( id in 1:nfold ){
  rf <- randomForest( d ~ ., data=datarf[which(foldid!=id),])
  predd[which(foldid==id)] = predict(rf, newdata=datarf[which(foldid==id),])
}
V <- y-predy
W <- d-predd
summary(glm( V ~ W ))$coef["W",]

