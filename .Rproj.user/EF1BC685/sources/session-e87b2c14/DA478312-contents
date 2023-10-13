#######################################################
### R script for Class 4: 
### Model Selection and Overfitting
### (Reminder: make sure all the files and 
### scripts are in the working directory)
###

### Some auxiliary files to load
source("DataAnalyticsFunctions.R")


installpkg("tree")
library(tree)
installpkg("partykit")
installpkg("libcoin")
installpkg("randomForest")
library(randomForest)
library(libcoin)
library(partykit)
### This will turn off warning messages
options(warn=-1)
############################
set.seed(1)
churndata <- read.csv("customerchurn.csv")

churndata <- churndata[complete.cases(churndata), -1]

## there are 7043 complete observations
## in this class, to understand the concepts we will work with 3000
## observations sampled from that pool
churndata <- churndata[sample(nrow(churndata), 3000), ]
### and we will split in two groups. 
summary(churndata)

mean(churndata$Churn=="Yes")

nfold <- 2 
n <- nrow(churndata) # the number of observations
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

## We have 1500 observations in our first set of regressions
##
model.logistic.interaction <-glm(Churn=="Yes"~.^2, data=churndata, subset=which(foldid==1), family="binomial")
model.logistic <-glm(Churn=="Yes"~., data=churndata, subset=which(foldid==1), family="binomial")
model.tree <- tree(factor(Churn)~ ., data=churndata, subset=which(foldid==1)) 
model.null <- glm(Churn=="Yes"~1, data=churndata, subset=which(foldid==1), family="binomial")

R2.model.logistic.interaction <-R2(y=churndata$Churn[foldid==1]=="Yes", pred=model.logistic.interaction$fitted, family="binomial")
R2.model.logistic <- R2(y=churndata$Churn[foldid==1]=="Yes", pred=model.logistic$fitted, family="binomial")
R2.model.tree     <-R2(y=factor(churndata$Churn[foldid==1]), pred=predict(model.tree, newdata=churndata[which(foldid==1),], type="vector")[,2], family="binomial")
R2.model.null     <- 0
# Model Fit via R2
M1<- c("Summary of R2 for the three method:\n" )
M2<- paste( " Logistic regression with interactions: R2 = ", R2.model.logistic.interaction,"\n")
M3<- paste( " Logistic regression:                   R2 = ", R2.model.logistic,"\n")
M4 <- paste(" Classification Tree:                   R2 = ", R2.model.tree,"\n")
M5 <- paste(" Null:                                  R2 = ", R2.model.null,"\n")

cat(M1,M2,M3,M4,M5)

par(mar=c(1.5,1.5,1,1))
par(mai=c(1.5,1.5,1,1))
barplot(c(R2.model.logistic.interaction, R2.model.logistic, R2.model.tree, R2.model.null), las=2, xlab="", names = c("logistic\n regression with\n interaction terms", "logistic\n regression", "classification\n tree","null model\n"), ylab = bquote(R^2))

### Logistic regression with interactions has better R2 than others.
###
### Lets plot FPR and TPR
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
val<- .5
values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
points( values$FPR , values$TPR )
ACC.model.logistic.interaction <- values$ACC
text( values$FPR+.12, values$TPR+.05, labels=c("LR with interaction"))
values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
points( values$FPR , values$TPR)    
ACC.model.logistic <- values$ACC
text( values$FPR+.02, values$TPR, labels=c("LR"))
values <- FPR_TPR( (predict(model.tree,type="class") == "Yes") , model.logistic.interaction$y )
points( values$FPR , values$TPR )    
ACC.model.tree <- values$ACC
text( values$FPR, values$TPR-.02, labels=c("tree"))

for( val in seq(from=0,to=1,by=0.05)){
  values <- FPR_TPR( (model.logistic.interaction$fitted >= val) , model.logistic.interaction$y )
  points( values$FPR , values$TPR, pch = 21, bg="red" )
  values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
  points( values$FPR , values$TPR, pch = 22, bg="blue" )    
  values <- FPR_TPR( (predict(model.tree,type="vector")[,2] >= val) , model.logistic.interaction$y )
  points( values$FPR , values$TPR, pch = 23, bg="green" )    
}


### Accuracy of the model
barplot(c(ACC.model.logistic.interaction, ACC.model.logistic, ACC.model.tree), xpd=FALSE, ylim=c(.75,.9), xlab="Method", names = c("\n logistic regression \n with interaction terms", "\n logistic\n regression ", "\n classification \n tree"), ylab = "Accuracy")

### How many coefficients in the model?
nrow(summary(model.logistic.interaction)$coef)
### we have a lot of dummy variables; we actually had 
length(model.logistic.interaction$coef)
### but 454-263=191 were compeltely redundant. 
### do you think all of them matter? Lets look at p-values
pvals<-summary(model.logistic.interaction)$coef[,4]
hist(pvals, breaks = seq(from=0,to=1,by=0.05), xlab="p-values", main="P-values of Logistic regression model with interactions", col="lightblue")


############################################
### Side note of looking at p-values:
###traditional rule (which does not control for multiplicity)
alpha <- 0.05
signif <- which(pvals <= alpha) 
length(signif)  
### It turns out that the conservative rule is very conservative
alpha <- 0.05
signif <- which(pvals <= alpha/nrow(summary(model.logistic.interaction)$coef)) 
length(signif)  
###




###################################################
## Out of sample prediction experiment
##
## We started with Holdout sample which splits the 
## sample in 2 parts. 
## Later we did k-fold CV which splits the data into k "folds". 
## The code below allows for that by setting the variable 
## nfold to the value you would like.
## 
## We start with nfold <- 2 and use first part for training
###################################################
### HOLDOUT SAMPLE
###
###################################################
### create an empty dataframe of results
OOS <- data.frame(logistic.interaction=NA, logistic=NA, tree=NA, null=NA) 

### Set the second part for testing (first for training)
k <- 2
### Set the other part for training (if not k)
train <- which(foldid!=k) # train on all but fold `k'
test  <- which(foldid==k) # test on fold k

### Do not worry about the warning messages. 
### These are minor numerical issues in this case.
model.logistic.interaction <-glm(Churn=="Yes"~.^2, data=churndata, subset=train, family="binomial")
model.logistic <-glm(Churn=="Yes"~., data=churndata, subset=train,family="binomial")
model.tree <- tree(factor(Churn)~., data=churndata, subset=train) 
model.null <- glm(Churn=="Yes"~1, data=churndata, subset=train, family="binomial")

## get predictions: type=response so we have probabilities
pred.logistic.interaction <- predict(model.logistic.interaction, newdata=churndata[-train,], type="response")
pred.logistic             <- predict(model.logistic, newdata=churndata[-train,], type="response")
pred.tree                 <- predict(model.tree, newdata=churndata[-train,], type="vector")[,2]
pred.null                 <- predict(model.null, newdata=churndata[-train,], type="response")

## calculate and log R2
# Logistic Interaction
OOS$logistic.interaction <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.logistic.interaction, family="binomial")
# Logistic
OOS$logistic <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.logistic, family="binomial")
# Tree
OOS$tree <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.tree, family="binomial")
#Null model (just intercept)
OOS$null <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.null, family="binomial")
#Null Model guess
sum(churndata$Churn[train]=="Yes")/length(train)


### Lets list the results stored in the dataframe OOS
OOS
###
### The null essentially 0 (as we would expect)
### However, the logistic with interaction was overfitting the data
### and the other two models had a better OOS R squares 
### Note that the actual value might change a bit with the 
### random number generator when we spliut the sample.
###
### Note that each observation was used 
### either to train OR to test. Not both.
###
### Cross validation (below) improves on that.
###
###################################################
### K-Fold cross validation
###
### Essentially the same code as before just that 
### we will use more folds and plot the uncertainty
### Number of OOS validation `folds'


### K Fold Cross Validation
###
### create a vector of fold memberships (random order)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(logistic.interaction=rep(NA,nfold), logistic=rep(NA,nfold), tree=rep(NA,nfold), null=rep(NA,nfold)) 

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the two regressions and null model
  model.logistic.interaction <-glm(Churn=="Yes"~.^2, data=churndata, subset=train, family="binomial")
  model.logistic <-glm(Churn=="Yes"~., data=churndata, subset=train,family="binomial")
  model.tree <- tree(factor(Churn)~ ., data=churndata, subset=train) 
  model.nulll <-glm(Churn=="Yes"~1, data=churndata, subset=train,family="binomial")
  ## get predictions: type=response so we have probabilities
  pred.logistic.interaction <- predict(model.logistic.interaction, newdata=churndata[-train,], type="response")
  pred.logistic             <- predict(model.logistic, newdata=churndata[-train,], type="response")
  pred.tree                 <- predict(model.tree, newdata=churndata[-train,], type="vector")
  pred.tree <- pred.tree[,2]
  pred.null <- predict(model.nulll, newdata=churndata[-train,], type="response")
  
  ## calculate and log R2
  # Logistic Interaction
  OOS$logistic.interaction[k] <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.logistic.interaction, family="binomial")
  OOS$logistic.interaction[k]
  # Logistic
  OOS$logistic[k] <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.logistic, family="binomial")
  OOS$logistic[k]
  # Tree
  OOS$tree[k] <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.tree, family="binomial")
  OOS$tree[k]
  #Null
  OOS$null[k] <- R2(y=churndata$Churn[-train]=="Yes", pred=pred.null, family="binomial")
  OOS$null[k]
  #Null Model guess
  sum(churndata$Churn[train]=="Yes")/length(train)
  
  ## We will loop this nfold times (I setup for 10)
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}
### Do not worry about the warning messages. 
### These are minor numerical issues in this case.
### 
### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))



### If you kept at least 10 folds, we can plot a box blot 
### so see how OOS R2 fluctuates across fold
if (nfold >= 10){
### This plots a box plot with the performance of the three models
  names(OOS)[1] <-"logistic\ninteraction"
### Lets zoom in  to see better the performance of 
### the small and the null model
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
  names(OOS)[1] <-"logistic.interaction"
}
###
###
###


##########################################
### Next we will start with Regularization via Lasso
#### install package for Lasso
installpkg("glmnet")
library(glmnet)
#### Lets run Lasso
#### First lets set up the data for it
#### the features need to be a matrix ([,-1] removes the first column which is the intercept)
Mx<- model.matrix(Churn ~ .^2, data=churndata)[,-1]
My<- churndata$Churn == "Yes"
### This defined the features we will use the matrix Mx (X) and the target My (Y)
###
#### Lasso requires a penalty parameter lambda

num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
### (do not ask how I know that...)
### 
### next we call Lasso providing the 
### features as matrix Mx
### the target as a vector My
### telling it is for logistic, family="binomial"
### and specifying lambda = lambda.theory
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
### by calling the summary we see the list of object in sclassoTheory
summary(lassoTheory)
### lassoTheory$a0 gives you the intercept
### lassoTheory$beta gives you all the coefficients. Many were set to zero.
### lassoTheory$lambda has the value of lambda stored.
### We can see the support of the selected coeffcients
### these are the indices
support(lassoTheory$beta)
### these are the labels
colnames(Mx)[support(lassoTheory$beta)]
### there are in total
length(support(lassoTheory$beta))
### coefficients selected by the model using the theoretical choice 
###
###
### If we omit the lambda, the function will solve for all values of lambda
### it takes a bit longer but not much longer at all. 
lasso <- glmnet(Mx,My, family="binomial")
### By running for all vaules of lambda we get many models back
### now we have the summary
summary(lasso)
### the length of lambda = 100. This means that it stored 
### the solutions for 100 different values of lambda.
### for each lambda we have an intercept a0 and 453 coefficients
### now lasso$a0 is a vector with 100 components (one for each lambda)
### and lasso$beta is a matrix of 453 x 100 components 
### (each columns corresponds to a solution for a value of lambda)
### These are the first 5 values
lasso$lambda[1:5]
### They are in decreasing order so the most sparse solution is beta[,1]
### For each coefficient we can plot its "Path" as we change lambda
par(mar=c(1.5,1.5,0.75,1.5))
par(mai=c(1.5,1.5,0.75,1.5))

## Make 2 plots side by side: mfrow=c(1,2)  # c(nrow,ncol)
par(mfrow=c(1,2))
coef_ind <- 5
par(mar=c(1.5,0.5,0.75,0.5))
par(mai=c(1.5,0.5,0.75,0.5))
plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")
coef_ind <- 2
par(mar=c(1.5,0.5,0.75,0.5))
par(mai=c(1.5,0.5,0.75,0.5))

plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")

## make it back to 1 plot only
par(mfrow=c(1,1))
par(mar=c(1.5,1.5,1.5,1.5))
par(mai=c(1.5,1.5,1.5,1.5))

### we can see all the 453 coefficients
### (note that the x axis is not lambda but L1 norm.
### The L1 norm is large when lambda is small and vice-versa.)
plot(lasso, xvar="lambda", main="# of non-zero coefficients", ylab ="Coefficient values", xlab = expression(paste("log(",lambda,")")))

### Now that we can actually compute the Lasso for all values of lambda,
### the whole "path" of models can be evaluated by a OOS experiment
### we can attempt to use cross valiadation to actually pick lambda.
### the following command yields a cross validation procedure
### the following command takes some time.
lassoCV <- cv.glmnet(Mx,My, family="binomial")
### We can plot the fitting graph
### red dots are mean values and the bars are the uncertainty
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

### There are some rules that people like to use:
### The minimum of the mean values stored in lambda.min
### 1se to the right stored in lambda.1se
### if we had to compute lambda.min we can simply write
lassoCV$lambda[which.min(lassoCV$cvm)]
### sclassoCV$cvm has the mean values
### which.min(sclassoCV$cvm) returns the index that minimizes it
### and remember that sclassoCV$lambda is the vector of lambda
### in any case we have lambda.min and lambda.1se.
### 
### lambda.min is perceived as aggressive (picks too many variables)
### lambda.1se is perceived as conservative (picks too few variables)
###
### Btw, where do you think the Theoretical one stands?
### we plot them in the previous picture
text(log(lassoCV$lambda.min), .95,"min",cex=1)
text(log(lassoCV$lambda.1se), 1,"1se",cex=1)

lines(c(log(lambda.theory),log(lambda.theory)),c(0.3,2.4),lty=3,col="blue")
text(log(lambda.theory), 1.05,"theory",cex=1)
### The theory one is between them! without any computation!
### either the theory work or we are a bit lucky today.

#### Post Lasso #####
#### we select a model based on the proposed rules
#### and perform cross validation

### The code is the same as the CV before so I removed the comments
PL.OOS <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
features.theory <- support(lassoTheory$beta)
length(features.theory)

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
data.theory <- data.frame(Mx[,features.theory],My)

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=data.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Churn~1, data=churndata, subset=train, family="binomial") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Churn~1, data=churndata, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=data.theory[-train,], type="response")
  PL.OOS$PL.min[k] <- R2(y=My[-train], pred=predmin, family="binomial")
  PL.OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se, family="binomial")
  PL.OOS$PL.theory[k] <- R2(y=My[-train], pred=predtheory, family="binomial")

  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
   
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  L.OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin, family="binomial")
  L.OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se, family="binomial")
  L.OOS$L.theory[k] <- R2(y=My[-train], pred=predlassotheory, family="binomial")
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}



R2performance <- cbind(PL.OOS,L.OOS,OOS)
par( mar=  c(8, 4, 4, 2) + 0.6 )
names(OOS)[1] <-"logistic\ninteraction"
barplot(colMeans(R2performance), las=2,xpd=FALSE, ylim=c(0,.3) , xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))
names(OOS)[1] <-"logistic.interaction"
m.OOS <- as.matrix(R2performance)
rownames(m.OOS) <- c(1:nfold)
par(mar=c(1.5,1.5,1.5,3))
par(mai=c(1.5,1.5,1.5,3))
barplot(t(as.matrix(m.OOS)), beside=TRUE, ylim=c(0,.4) ,legend=TRUE, args.legend=c(x= "topright", y=0.92,bty = "n"),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))


### We could have done the same for 
### accuracy Performance

PL.OOS.TP <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.OOS.TP <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 
PL.OOS.TN <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.OOS.TN <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 
PL.OOS.FP <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.OOS.FP <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 
PL.OOS.FN <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), PL.theory=rep(NA,nfold)) 
L.OOS.FN <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold), L.theory=rep(NA,nfold)) 

OOS.TP <- data.frame(logistic.interaction=rep(NA,nfold), logistic=rep(NA,nfold), tree=rep(NA,nfold), null=rep(NA,nfold)) 
OOS.TN <- data.frame(logistic.interaction=rep(NA,nfold), logistic=rep(NA,nfold), tree=rep(NA,nfold), null=rep(NA,nfold)) 
OOS.FP <- data.frame(logistic.interaction=rep(NA,nfold), logistic=rep(NA,nfold), tree=rep(NA,nfold), null=rep(NA,nfold)) 
OOS.FN <- data.frame(logistic.interaction=rep(NA,nfold), logistic=rep(NA,nfold), tree=rep(NA,nfold), null=rep(NA,nfold)) 
val <- .3
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=data.min, subset=train, family="binomial")
  if ( length(features.1se) == 0){  r1se <- glm(Churn~1, data=churndata, subset=train, family="binomial") 
  } else {r1se <- glm(My~., data=data.1se, subset=train, family="binomial")
  }
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Churn~1, data=churndata, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  predtheory <- predict(rtheory, newdata=data.theory[-train,], type="response")
  
  values <- FPR_TPR( (predmin >= val) , My[-train] )
  PL.OOS$PL.min[k] <- values$ACC
  PL.OOS.TP$PL.min[k] <- values$TP
  PL.OOS.TN$PL.min[k] <- values$TN
  PL.OOS.FP$PL.min[k] <- values$FP
  PL.OOS.FN$PL.min[k] <- values$FN

  values <- FPR_TPR( (pred1se >= val) , My[-train] )
  PL.OOS$PL.1se[k] <- values$ACC
  PL.OOS.TP$PL.1se[k] <- values$TP
  PL.OOS.FP$PL.1se[k] <- values$FP
  PL.OOS.TN$PL.1se[k] <- values$TN
  PL.OOS.FN$PL.1se[k] <- values$FN
  values <- FPR_TPR( (predtheory >= val) , My[-train] )
  PL.OOS$PL.theory[k] <- values$ACC
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.1se)
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  values <- FPR_TPR( (predlassomin >= val) , My[-train] )
  L.OOS$L.min[k] <- values$ACC
  L.OOS.TP$L.min[k] <- values$TP
  L.OOS.TN$L.min[k] <- values$TN
  L.OOS.FP$L.min[k] <- values$FP
  L.OOS.FN$L.min[k] <- values$FN
  values <- FPR_TPR( (predlasso1se >= val) , My[-train] )
  L.OOS$L.1se[k] <- values$ACC
  L.OOS.TP$L.1se[k] <- values$TP
  L.OOS.TN$L.1se[k] <- values$TN
  L.OOS.FP$L.1se[k] <- values$FP
  L.OOS.FN$L.1se[k] <- values$FN
  values <- FPR_TPR( (predlassotheory >= val) , My[-train] )
  L.OOS$L.theory[k] <- values$ACC
  L.OOS.TP$L.theory[k] <- values$TP
  L.OOS.TN$L.theory[k] <- values$TN
  L.OOS.FP$L.theory[k] <- values$FP
  L.OOS.FN$L.theory[k] <- values$FN
  
  
  ## fit the two regressions and null model
  ##### full model uses all 200 signals
  model.logistic.interaction <-glm(Churn=="Yes"~.^2, data=churndata, subset=train, family="binomial")
  model.logistic <-glm(Churn=="Yes"~., data=churndata, subset=train,family="binomial")
  model.tree <- tree(factor(Churn)~ ., data=churndata, subset=train) 
  model.nulll <-glm(Churn=="Yes"~1, data=churndata, subset=train,family="binomial")
  ## get predictions: type=response so we have probabilities
  pred.logistic.interaction <- predict(model.logistic.interaction, newdata=churndata[-train,], type="response")
  pred.logistic             <- predict(model.logistic, newdata=churndata[-train,], type="response")
  pred.tree                 <- predict(model.tree, newdata=churndata[-train,], type="vector")
  pred.tree <- pred.tree[,2]
  pred.null <- predict(model.nulll, newdata=churndata[-train,], type="response")
  
  ## calculate and log R2
  # Logistic Interaction
  values <- FPR_TPR( (pred.logistic.interaction >= val) , My[-train] )
      
  OOS$logistic.interaction[k] <- values$ACC
  OOS.TP$logistic.interaction[k] <- values$TP
  OOS.FP$logistic.interaction[k] <- values$FP
  OOS.TN$logistic.interaction[k] <- values$TN
  OOS.FN$logistic.interaction[k] <- values$FN
  # Logistic
  values <- FPR_TPR( (pred.logistic >= val) , My[-train] )
  OOS$logistic[k] <- values$ACC
  OOS.TP$logistic[k] <- values$TP
  OOS.TN$logistic[k] <- values$TN
  OOS.FP$logistic[k] <- values$FP
  OOS.FN$logistic[k] <- values$FN
  # Tree
  values <- FPR_TPR( (pred.tree >= val) , My[-train] )
  OOS$tree[k] <- values$ACC
  OOS.TP$tree[k] <- values$TP
  OOS.TN$tree[k] <- values$TN
  OOS.FP$tree[k] <- values$FP
  OOS.FN$tree[k] <- values$FN
  #Null
  values <- FPR_TPR( (pred.null >= val) , My[-train] )
  OOS$null[k] <- values$ACC
  OOS.TP$null[k] <- values$TP
  OOS.TN$null[k] <- values$TN
  OOS.FP$null[k] <- values$FP
  OOS.FN$null[k] <- values$FN
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
names(OOS)[1] <-"logistic\ninteraction"
ACCperformance <- cbind(PL.OOS,L.OOS,OOS)
names(OOS)[1] <-"logistic.interaction"
barplot(colMeans(ACCperformance), xpd=FALSE, ylim=c(.7,.8), xlab="Method", ylab = "Accuracy")
m.OOS <- as.matrix(ACCperformance)
rownames(m.OOS) <- c(1:nfold)
par(mar=c(1.5,1.5,1.5,1))
par(mai=c(1.5,1.5,1.5,1))
barplot(t(as.matrix(m.OOS)), beside=TRUE, legend=TRUE, args.legend=c(x= "topright", y=0.92,bty = "n"),
        ylab= bquote( "Out of Sample Accuracy"), xlab="Fold", names.arg = c(1:10))



### Lets plot FPR and TPR
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
#
TPR = sum(OOS.TP$tree)/(sum(OOS.TP$tree)+sum(OOS.FN$tree))  
FPR = sum(OOS.FP$tree)/(sum(OOS.FP$tree)+sum(OOS.TN$tree))  
#text( FPR, TPR, labels=c("tree"))
points( FPR , TPR )
#
TPR = sum(OOS.TP$logistic)/(sum(OOS.TP$logistic)+sum(OOS.FN$logistic))  
FPR = sum(OOS.FP$logistic)/(sum(OOS.FP$logistic)+sum(OOS.TN$logistic))  
#text( FPR, TPR, labels=c("LR"))
points( FPR , TPR )

#
TPR = sum(OOS.TP$logistic.interaction)/(sum(OOS.TP$logistic.interaction)+sum(OOS.FN$logistic.interaction))  
FPR = sum(OOS.FP$logistic.interaction)/(sum(OOS.FP$logistic.interaction)+sum(OOS.TN$logistic.interaction))  
#text( FPR, TPR, labels=c("LR wih int"))
points( FPR , TPR )
#
TPR = sum(PL.OOS.TP$PL.min)/(sum(PL.OOS.TP$PL.min)+sum(PL.OOS.FN$PL.min))  
FPR = sum(PL.OOS.FP$PL.min)/(sum(PL.OOS.FP$PL.min)+sum(PL.OOS.TN$PL.min))  
#text( FPR, TPR, labels=c("PL.min"))
points( FPR , TPR )
#
TPR = sum(PL.OOS.TP$PL.1se)/(sum(PL.OOS.TP$PL.1se)+sum(PL.OOS.FN$PL.1se))  
FPR = sum(PL.OOS.FP$PL.1se)/(sum(PL.OOS.FP$PL.1se)+sum(PL.OOS.TN$PL.1se))  
#text( FPR, TPR, labels=c("PL.1se"))
points( FPR , TPR )
#
TPR = sum(PL.OOS.TP$PL.theory)/(sum(PL.OOS.TP$PL.theory)+sum(PL.OOS.FN$PL.theory))  
FPR = sum(PL.OOS.FP$PL.theory)/(sum(PL.OOS.FP$PL.theory)+sum(PL.OOS.TN$PL.theory))  
#text( FPR, TPR, labels=c("PL.1se"))
points( FPR , TPR )
#
TPR = sum(L.OOS.TP$L.min)/(sum(L.OOS.TP$L.min)+sum(L.OOS.FN$L.min))  
FPR = sum(L.OOS.FP$L.min)/(sum(L.OOS.FP$L.min)+sum(L.OOS.TN$L.min))  
#text( FPR, TPR, labels=c("L.min"))
points( FPR , TPR )
#
TPR = sum(L.OOS.TP$L.1se)/(sum(L.OOS.TP$L.1se)+sum(L.OOS.FN$L.1se))  
FPR = sum(L.OOS.FP$L.1se)/(sum(L.OOS.FP$L.1se)+sum(L.OOS.TN$L.1se))  
#text( FPR, TPR, labels=c("L.1se"))
points( FPR , TPR )
#
TPR = sum(L.OOS.TP$L.theory)/(sum(L.OOS.TP$L.theory)+sum(L.OOS.FN$L.theory))  
FPR = sum(L.OOS.FP$L.theory)/(sum(L.OOS.FP$L.theory)+sum(L.OOS.TN$L.theory))  
#text( FPR, TPR, labels=c("L.1se"))
points( FPR , TPR )






##################################################################
####### Supplementary Material  ##################################
##################################################################
### Lets plot Learning Curve
### This defined the features we will use the matrix Mx (X) and the target My (Y)
Mx<- model.matrix(Churn ~ .^2, data=churndata)[,-1]
My<- churndata$Churn == "Yes"
num.features <- ncol(Mx)
num.n <- nrow(Mx)

NumberOfVars <- c( seq(from=1, to=30, by=1), seq(from=31, to=num.features, by=30)  )
plot( c( 0, log(num.features) ), c(1000, 5000), type="n", xlim=c(0,log(num.features)), ylim=c(1000, 5000), bty="n", xlab = "Complexity of the model (log of # of vars)", ylab="Objective Function (Deviation)")
for(j in NumberOfVars){ #num.features
  model <-glm(My[train]~ Mx[train,1:j], family="binomial")
  ## get predictions: type=response so we have probabilities
  pred <- predict(model, newdata=data.frame(Mx[-train,1:j]), type="response")
  ## calculate and record R2
  InSample.Dev <- model$dev
  OOS.Dev <- deviance (y=churndata$Churn[-train], pred=pred, family="binomial")
  InSample.R2 <- R2(y=model$y, pred=model$fitted, family="binomial")
  OOS.R2 <- R2(y=churndata$Churn[-train], pred=pred, family="binomial")
  points( log(j) , InSample.Dev , bg ="red", pch=21 )
  points( log(j) , OOS.Dev , bg ="blue", pch =23)
  InSample.Dev
  OOS.Dev
}

## A forward stepwise procedure
## define the smallest model (null model)
null <- glm(Churn~1, data=churndata, family="binomial")
## define the largest model (in this case with all interactions)
full <- glm(Churn~.^2, data=churndata, family="binomial")
## forward stepwise: it takes a long time!
## the function system.time(...) will provide the time "..." took to run
system.time(fwd <- step(null, scope=formula(full), dir="forward"))
### fwd stored the best model selected by the Forward regression
### the size of the selected model is
length(coef(fwd)) 
### the main issue is that it depends heavily on the order the variables
### are included in the model. In contrasts, other tools consider 
### all possible combinations.
###################################################################


