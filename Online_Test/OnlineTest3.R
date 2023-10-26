#######################################################
### R script for Online Test: 
### 
### (Reminder: make sure all the files and 
### scripts are in the working directory)
###
source("./Online_Test/DataAnalyticsFunctions.R")
load("./Online_Test/WorkspaceOnlineTest3part1.RData")

options(warn=-1)

summary(churndata)

nfold <- 2 
n <- nrow(churndata)
set.seed(1)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
train <- which(foldid==1)
test  <- which(foldid==2)

model.logistic <-glm(Churn~., data=churndata, subset=train, family="binomial")


#Question 1. Consider using a logistic regression model to estimate the probability a customer will churn. 

#a)
pred.logistic<- predict(model.logistic, newdata=churndata[train,], type="response")
resp <-R2(y=churndata$Churn[train], pred=pred.logistic, family="binomial")
paste("We propose to use a logistic regression model. Its ", expression(R^2),"is", resp)

#b) 
pred.logistic <- predict(model.logistic, newdata=churndata[test,], type="response")
resp <-R2(y=churndata$Churn[test], pred=pred.logistic, family="binomial")
paste("We propose to use a logistic regression model. Its OOS", expression(R^2),"is", resp)

#c)
pred.logistic<- predict(model.logistic, newdata=churndata[train,], type="response")
values <- FPR_TPR( (pred.logistic >= .3) , churndata$Churn[train]=="Yes" )
resp <- values$ACC
paste("We propose to use a logistic regression model. Its in-sample accuracy is", resp)

#d)
pred.logistic<- predict(model.logistic, newdata=churndata[test,], type="response")
values <- FPR_TPR( (pred.logistic >= .3) , churndata$Churn[test]=="Yes" )
resp <- values$ACC
paste("We propose to use a logistic regression model. Its OOS accuracy is", resp)

#############
#Question 2
#In order to use make predictions with a 
# logistic regression model you need to pick 
# a threshold t for the predicted probability 
# so that you predict Y=1 if the predicted 
# probability is larger than t. 
# The code provided in the file considers 
# many choices of t and evaluate the 
# associated models using false positive rate and true positive rate via 10-fold cross validation.
nfold <- 10 
n <- nrow(churndata)
# make sure 
set.seed(2)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

val <- seq(from=0,to=1,by=0.025)
l.val <- length(val)
CV.logistic.FPR<-matrix(0,nrow=l.val,ncol=nfold)
CV.logistic.TPR<-matrix(0,nrow=l.val,ncol=nfold)
                 
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  model.logistic <-glm(Churn~., data=churndata, subset=train,family="binomial")
  ## get predictions: type=response so we have probabilities
  pred.logistic             <- predict(model.logistic, newdata=churndata[-train,], type="response")
  
  # Logistic
  for (kk in 1:l.val){
  values <- FPR_TPR( (pred.logistic >= val[kk]) , churndata$Churn[-train]=="Yes" )
  CV.logistic.FPR[kk,k] <- values$FPR
  CV.logistic.TPR[kk,k] <- values$TPR
  }
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

### Lets plot FPR and TPR
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( rowMeans(CV.logistic.FPR) , rowMeans(CV.logistic.TPR))

## the values of t are in
val
## the corresponding values of the False Positive Rate (FPR) are in
rowMeans(CV.logistic.FPR)
## the corresponding values of the True Positive Rate (TPR) are in
rowMeans(CV.logistic.TPR)

############################################################
############################################################

source("DataAnalyticsFunctions.R")
load("WorkspaceOnlineTest3part2.RData")

options(warn=-1)
################################
####### k-NN
################################
installpkg("class")
library(class)


### the german credit data was loaded in load("WorkspaceOnlineTest5.RData")
### otherwise we should could call germancredit <- read.csv("germancredit.csv") 
summary(germancredit)
set.seed(1)
### Creating train and test subset
nfold <- 2 
n <- nrow(germancredit)
set.seed(1)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
train <- which(foldid==1)
test  <- which(foldid==2)
#####################################################
#####################################################

#4. Similarity measures motivate various predictive methods. A marketing consultant wants to apply k-nearest neighbor as a predictive model. He creates a matrix with several features of past customers  
x <- model.matrix(Default~., data=germancredit)[,-1]
y <- germancredit$Default
#The consultant also has a code to create "train" and "test" subsets. Which of the following options of reasoning/code is more appropriate to evaluate the k-nn's predictive accuracy?
#a) Since k-nn is based on similarity there is no need to have a out of sample measure of performance  
knn15 <- knn(train=x[train,], test=x[train,], cl=y[train], k=15, prob=TRUE)
prob.winning.class <-attr(knn15, "prob")
prob.default <-ifelse(knn15=="1", prob.winning.class, 1-prob.winning.class)
BinaryAccuracy( prob.default>.5, germancredit$Default[train])

#b) Since k-nn is based on similarity we need to use a zero threshold on the probability    
knn15 <- knn(train=x[train,], test=x[train,], cl=y[train], k=15, prob=TRUE)
prob.winning.class <-attr(knn15, "prob")
prob.default <-ifelse(knn15=="1", prob.winning.class, 1-prob.winning.class)
BinaryAccuracy( prob.default>0, germancredit$Default[train])

#c) Since k-nn is based on similarity we should train in the test set to construct the predictions   
knn15 <- knn(train=x[test,], test=x[test,], cl=y[train], k=15, prob=TRUE)
prob.winning.class <-attr(knn15, "prob")
prob.default <-ifelse(knn15=="1", prob.winning.class, 1-prob.winning.class)
BinaryAccuracy( prob.default>0, germancredit$Default[train])

#d) k-nn is subject to overfitting as any data mining tool. We should provide some out of sample accuracy measure  
knn15 <- knn(train=x[train,], test=x[test,], cl=y[train], k=15, prob=TRUE)
prob.winning.class <-attr(knn15, "prob")
prob.default <-ifelse(knn15=="1", prob.winning.class, 1-prob.winning.class)
BinaryAccuracy( prob.default>.5, germancredit$Default[test])

# 5. A marketing consultant want to segment the market of customers that apply to loans.
#   The consultant proposes to segment the market into three groups.
# Which of the following commands is more suitable for that?
# a) Create numeric covariates based on the variables 
# and apply the k-means algorithm with 3 centers  
# The consultant also summarizes how the clusters perform wrt default.
x <- model.matrix(Default~., data=germancredit)[,-1]
three.clusters <- kmeans(x, 3, nstart=10)
tapply(germancredit$Default, three.clusters$cluster, table )
# b)  Create numeric covariates based on the variables, 
# standardize the columns, and apply the k-means algorithm with 3 centers.  
# The consultant also summarizes how the clusters perform wrt default.
x <- model.matrix(Default~., data=germancredit)[,-1]
x.scaled <- scale(x)
three.clusters <- kmeans(x.scaled, 3, nstart=10)
tapply(germancredit$Default, three.clusters$cluster, table )
# c) Create numeric covariates based on the variables, 
# append the information of default, and apply the k-means algorithm with 3 centers  
# The consultant also summarizes how the clusters perform wrt default.
x <- model.matrix(Default~., data=germancredit)[,-1]
three.clusters <- kmeans( cbind(germancredit$Default,x), 3, nstart=10)
tapply(germancredit$Default, three.clusters$cluster, table )
# d) Create numeric covariates based on the variables, 
# append the information of default, standardize the columns, and apply the k-means algorithm with 3 centers  
# The consultant also summarizes how the clusters perform wrt default.
x <- model.matrix(Default~., data=germancredit)[,-1]
x.scaled <- scale(cbind(germancredit$Default,x))
three.clusters <- kmeans(x.scaled, 3, nstart=10)
tapply(germancredit$Default, three.clusters$cluster, table )

#6. The use of 3 clusters suggested by the consultant in Question 5 is 
# motivated by simplicity. In order to select the number of clusters
# in a data-driven, you intend to use information criteria (a regularization technique).
# More specifically, BIC (Bayesian Information Criteria) due to its performance to avoid overfitting.
set.seed(1)
x <- model.matrix(Default~., data=germancredit)[,-1]
kfit <- lapply(1:200, function(k) kmeans(scale(x),k))

source("DataAnalyticsFunctions.R") # contsints the function kIC (you should have already loaded it)
## Note that the function kIC selects k=# of clusters via Information Criteria
## there are two options: "A" for AIC (default) or "B" for BIC
kbic  <- sapply(kfit,kIC,"B")
plot(kbic, xlab="# of clusters (k)", ylab="Information Criterion", ylim=range(c(kbic)), type="l", lwd=2)
abline(v=which.min(kbic),col=4)
text(70,mean(kbic),"BIC")
kmin <- which.min(kbic)
k3   <- 3
# Compare R2
1 - sum(kfit[[kmin]]$tot.withinss)/kfit[[kmin]]$totss
1 - sum(kfit[[k3]]$tot.withinss)/kfit[[k3]]$totss
# Although there is no "perfect" choice for the number of clusters, which of the following statements better characterize the choice of the number of clusters?
#
# a) It seems that 3 clusters are "too few" as they explain only 8.6% of the variation while BIC (which is guarding from
# overfitting) generateds a model with 37.7%. The consultant should consider segmenting further the market market (from the current 3 segments) to obtain a better balance. For example six clusters yields
1 - sum(kfit[[6]]$tot.withinss)/kfit[[6]]$totss
#
# b) It seems that 3 clusters is more than enough as the BIC selects less than 10 clusters.
kmin
#
# c) It is important to use AIC instead of BIC because it is important to obtain more clusters to further segment.
kaic  <- sapply(kfit,kIC, "A")
which.min(kaic)
plot(kaic, xlab="# of clusters (k)", ylab="Information Criterion", ylim=range(c(kaic)), type="l", lwd=2)
#
# d) The difference between the number of clusters selected based on BIC and AIC is substantial which indicates we cannot use k-means to segment.
which.min(kaic)
1 - sum(kfit[[which.min(kaic)]]$tot.withinss)/kfit[[which.min(kaic)]]$totss
which.min(kbic)
1 - sum(kfit[[which.min(kbic)]]$tot.withinss)/kfit[[which.min(kbic)]]$totss
#
#