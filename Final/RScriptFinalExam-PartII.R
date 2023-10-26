#######################
### Final Exam - Part I
### General Questions:
#######################

exp(-logodds) = 9
exp(2.1972246)
newlogodds <- -2.1972246 + 9.434740e-04 * 500
1/(1+exp(-newlogodds))


#######################
### Final Exam - Part II
### General Questions:
#######################


source("./Final/DataAnalyticsFunctions.R")



######################################################
### The following two questions pertain to the churn problem
### discussed in class.
load("./Final/WorkspaceFinal-PartII-Churn.RData")
options(warn=-1)
summary(churndata)

#Question 8. #########################################
### Next we will start with Regularization via Lasso
### install package for Lasso
installpkg("glmnet")
library(glmnet)
#### Create a model with interactions of degree 3 
Mx<- model.matrix(Churn ~ .^3, data=churndata)[,-1]
My<- churndata$Churn == "Yes"
### This defined the features we will use the matrix Mx (X) and 
### the target My (Y)
num.features <- ncol(Mx)
num.n <- nrow(Mx)
num.churn <- sum(My)
w <- (num.churn/num.n)*(1-(num.churn/num.n))
#### For the binomial case, a theoretically valid choice is
lambda.theory <- sqrt(w*log(num.features/0.05)/num.n)
### next we call Lasso providing the 
### features as matrix Mx
### the target as a vector My
### telling it is for logistic, family="binomial"
### and specifying lambda = lambda.theory
lassoTheory <- glmnet(Mx,My, family="binomial",lambda = lambda.theory)
### by calling the summary we see the list of object in sclassoTheory
summary(lassoTheory)
### lassoTheory$a0 gives you the intercept
### lassoTheory$beta gives you the coefficients of the 4179 variables. 
### As expectd, many were set to zero.
### lassoTheory$lambda has the value of lambda stored.
### We can see the support of the selected coeffcients
### these are the indices
support(lassoTheory$beta,tr=0)
### the function supports returns the indices in the vector
### whose components'magnitude are above the value 
### specified in tr. If no value of tr is specified, it sets tr=10^-6
### these are the labels
colnames(Mx)[support(lassoTheory$beta,tr=0)]
### there are in total
length(support(lassoTheory$beta,tr=0))
### variables
###
### If we omit the lambda, the function will solve for all values of lambda
### it takes a bit longer but not much longer at all. 
lasso <- glmnet(Mx,My, family="binomial")
### Now that we can actually compute the Lasso for all values of lambda,
### the whole "path" of models can be evaluated by a OOS experiment
### we can attempt to use cross valiadation to actually pick lambda.
### the following command yields a cross validation procedure
### (note that this call will take some time)
lassoCV <- cv.glmnet(Mx,My, family="binomial")
### We can plot the fitting graph
### red dots are mean values and the bars are the uncertainty
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
### the following command takes some time.
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))
lines(c(log(lassoCV$lambda.min),log(lassoCV$lambda.min)), c(0, 2), lty=2, col = "blue")
lines(c(log(lassoCV$lambda.1se),log(lassoCV$lambda.1se)), c(0, 2), lty=2, col = "green")
lines(c(log(lassoTheory$lambda),log(lassoTheory$lambda)), c(0, 2), lty=2, col = "pink")

lassoMin <- glmnet(Mx,My, family="binomial",lambda = lassoCV$lambda.min)
length(support(lassoMin$beta, tr= 0))

### if you need to look into the information in lassoCV
### you can see which are the fields by usinghttp://127.0.0.1:39427/graphics/07cce5a3-4ded-4cc5-97b7-66c6d7b9ad2e.png
attributes(lassoCV)


### Question 9. ####
### The code runs 10-fold CV for 4 methods:
### Lasso (theory and min choice of lambda);
### Post-Lasso (based on theory and min choices of lambda)
PL.OOS <- data.frame(PL.min=rep(NA,nfold),  PL.theory=rep(NA,nfold)) 
L.OOS <- data.frame(L.min=rep(NA,nfold),  L.theory=rep(NA,nfold)) 
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)],tr=0)
which(lassoCV$lambda==lassoCV$lambda.min)
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)],tr=0)
length(features.min)
features.theory <- support(lassoTheory$beta,tr=0)
length(features.theory)

data.min <- data.frame(Mx[,features.min],My)
data.theory <- data.frame(Mx[,features.theory],My)

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ### This is the CV for the Post Lasso Estimates
  rmin <- glm(My~., data=data.min, subset=train, family="binomial")
  
  if ( length(features.theory) == 0){ 
    rtheory <- glm(Churn~1, data=churndata, subset=train, family="binomial") 
  } else {rtheory <- glm(My~., data=data.theory, subset=train, family="binomial") }
  
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  predtheory <- predict(rtheory, newdata=data.theory[-train,], type="response")
  PL.OOS$PL.min[k] <- R2(y=My[-train], pred=predmin, family="binomial")
  PL.OOS$PL.theory[k] <- R2(y=My[-train], pred=predtheory, family="binomial")
  
  ### This is the CV for the Lasso estimates  
  lassomin  <- glmnet(Mx[train,],My[train], family="binomial",lambda = lassoCV$lambda.min)
  lassoTheory <- glmnet(Mx[train,],My[train], family="binomial",lambda = lambda.theory)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlassotheory <- predict(lassoTheory, newx=Mx[-train,], type="response")
  L.OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin, family="binomial")
  L.OOS$L.theory[k] <- R2(y=My[-train], pred=predlassotheory, family="binomial")
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}



R2performance <- cbind(PL.OOS,L.OOS)
barplot(colMeans(R2performance), xpd=FALSE, ylim=c(0,.3) , xlab="Method", ylab = bquote( "10-Fold Cross Validation OOS" ~ R^2))
m.OOS <- as.matrix(R2performance)
rownames(m.OOS) <- c(1:nfold)
par(mar=c(1.5,1.5,1.5,3))
par(mai=c(1.5,1.5,1.5,3))
barplot(t(as.matrix(m.OOS)), beside=TRUE, ylim=c(0,.4) ,legend=TRUE, args.legend=c(xjust=0, yjust=1),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))



######################################################
### The following two questions pertain to the churm problem
### discussed in class.
################################################


source("./Final/DataAnalyticsFunctions.R")
load("./Final/WorkspaceFinal-PartII-German.RData")

options(warn=-1)
################################
####### k-NN
################################
installpkg("ElemStatLearn")
library(ElemStatLearn)
installpkg("class")
library(class)
### the german credit data was loaded in the workspace
### otherwise we should could call germancredit <- read.csv("germancredit.csv") 
summary(germancredit)
str(germancredit)
set.seed(1)

x <- model.matrix(Default~., data=germancredit)[,-1]
kfit <- lapply(1:200, function(k) kmeans(scale(x),k))

source("./Final/DataAnalyticsFunctions.R") # contsints the function kIC (you should have already loaded it)
## Note that the function kIC selects k=# of clusters via Information Criteria
## there are two options: "A" for AIC (default) or "B" for BIC
kbic  <- sapply(kfit,kIC,"B")
plot(kbic, xlab="# of clusters (k)", ylab="Information Criterion", ylim=range(c(kbic)), type="l", lwd=2)
abline(v=which.min(kbic),col=4)
text(70,mean(kbic),"BIC")
kmin <- which.min(kbic)


# 10. In the presence of many features it can be hard to interpret and  visualize the output of clustering.
# In order to compare the clusters, the marketing consultant is considering a visualization.
# Which of the following visualizations can be more useful to illustrate the business setting?

# a) The radius of the bubble relates to the market size of the cluster, and the coordinate a given by variation explained.
radius <- sqrt(kfit[[kmin]]$size/pi)
symbols(kfit[[kmin]]$withinss, (kfit[[kmin]]$withinss/kfit[[kmin]]$totss),circles=radius, xlab = "Variation in the cluster", ylab="% of total variation", bg = col, inches = .5)
text((kfit[[kmin]]$withinss), (kfit[[kmin]]$withinss/kfit[[kmin]]$totss), names=1:kfit[[kmin]]$size)

# b) The radius of the bubble relates to the market size of the cluster, and the coordinate a given by explained variation.
radius <- sqrt(kfit[[kmin]]$size/pi)
symbols((kfit[[kmin]]$withinss/kfit[[kmin]]$size), (kfit[[kmin]]$withinss/kfit[[kmin]]$totss),circles=radius, xlab = "Average of variation per observation in the cluster", ylab="% of total variation", bg = col, inches = .5)
text((kfit[[kmin]]$withinss/kfit[[kmin]]$size), (kfit[[kmin]]$withinss/kfit[[kmin]]$totss), names=1:kfit[[kmin]]$size)

# c) The radius of the bubble relates to default rates, and the coordinate a given by explained variation.
cluster.default <- tapply(germancredit$Default, kfit[[kmin]]$cluster,mean)
radius <- sqrt(cluster.default)
symbols((kfit[[kmin]]$withinss/kfit[[kmin]]$size), (kfit[[kmin]]$withinss/kfit[[kmin]]$totss),circles=radius, xlab = "Average of variation per observation in the cluster", ylab="% of total variation", bg = col, inches = .5)
text((kfit[[kmin]]$withinss/kfit[[kmin]]$size), (kfit[[kmin]]$withinss/kfit[[kmin]]$totss), names=1:kfit[[kmin]]$size)

# d) The radius of the bubble relates to default rates, and the coordinate a given by segment size and average loan within cluster.
cluster.amount <- tapply(germancredit$amount, kfit[[kmin]]$cluster,mean)
cluster.default <- tapply(germancredit$Default, kfit[[kmin]]$cluster,mean)
radius <- sqrt(cluster.default)
symbols((cluster.amount), (kfit[[kmin]]$size),circles=radius, xlab = "Average loan amount within cluster", ylab="Segment size", bg = col, inches = .5)
text((cluster.amount), (kfit[[kmin]]$size), names=1:kfit[[kmin]]$size)



#11. The marketing consultant wants to uncover "latent variables" of customers. 
# For that he intends to use PCA and study the "top variables" in the first principal component.
#
installpkg("plfm")
library(plfm)
x <- model.matrix(Default~., data=germancredit)[,-1]
## the command above return the matrix X associated with the model
## Default~. under the dataframe "germancredit (recall that Default is the variable Yes/No for the loan)
## the last part "[,-1]" removes the intercept from the matrix which was automatically created
feature.labels <- colnames(x)
pca.features <- prcomp(x, scale=TRUE)
#pich the first principal component
loadings <- pca.features$rotation[,1]
## Pick the incorrect statement:
# a) The first principal component seems explain more than any other principal component.
plot(pca.features,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
#
# b) The most important variable in the first factor is 
which.max(loadings)
max(loadings)
#
# c) The first component uses a combination of all attributes (even if with small coefficient).
loadings
names(germancredit)
#
# d) Looking at the loadings ordered by magnitude
loadings[order(abs(loadings), decreasing=TRUE)[1:length(loadings)]]
# can help to identify the important variables and to create a latent interpretation, 
# but one needs to be careful.



pixels <- 40*20
parameter <- matrix()
layers <- c(16,16,16,1)
parameter$a <-  (pixels+1)*layers[1] + (layers[1]+1)*layers[2] + (layers[2]+1)*layers[3] + (layers[3]+1)*layers[4]
layers <- c(32,32,32,1)
parameter$b <- (pixels+1)*layers[1] + (layers[1]+1)*layers[2] + (layers[2]+1)*layers[3] + (layers[3]+1)*layers[4]
layers <- c(64,32,32,1)
parameter$c <- (pixels+1)*layers[1] + (layers[1]+1)*layers[2] + (layers[2]+1)*layers[3] + (layers[3]+1)*layers[4]
layers <- c(16, 8, 8,1)
parameter$d <- (pixels+1)*layers[1] + (layers[1]+1)*layers[2] + (layers[2]+1)*layers[3] + (layers[3]+1)*layers[4]
parameter
