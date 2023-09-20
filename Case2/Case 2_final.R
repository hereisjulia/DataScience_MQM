##################
###import data ###
##################
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(stringr)
source("DataAnalyticsFunctions.R")
data <- read_csv("https://raw.githubusercontent.com/hereisjulia/DataScience_MQM/main/Case2/ALLSTATEcost.csv")
drop <- c("customer_ID","shopping_pt","record_type","time","location")
data <- data[,!(names(data) %in% drop)]

#Question 1
#Evaluating level of coverage options by cost
coverage <- data[, c(20,13:19)]
names(coverage)[2:8] <- c("Collision", "Towing", "BI", "PD", "RR", "Comp", "Med/PIP")
coverage_long <- coverage %>% pivot_longer( 2:8, names_to = "option", values_to = "level")
plot1 <- coverage_long %>% group_by(option, level) %>% summarize(mean = mean(cost), number_of_deal = length(option))
ggplot(data = plot1)+
  geom_point(aes(x = level, y = `mean`, size = `number_of_deal`, color = option))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Coverage Options & Amount vs. Quotes", x = "Quote for customers", y = "Level of Coverage")+
  facet_wrap(~option, ncol = 3)+
  theme_bw()

#Question 2
model1 <- glm(cost ~ .+(A+B+C+D+E+F+G)^2, data = data)
summary(model1)
Rsq <- 1 - (model1$dev/model1$null)
Rsq

#Question 3
#NO ANALYSIS NEEDED

#Question 4
# import new data
new.customers <- readRDS("NewCustomers.Rda")
new.customers$state <- as.character(new.customers$state)
new.customers$car_value <- as.character(new.customers$car_value)
coloumns <- names(new.customers)[c(1,3:5,7:20)]
for (col_name in coloumns) {
  new.customers[[col_name]] <- as.numeric(new.customers[[col_name]])
}

#### 3 model built #####
#GLM
model1 <- glm(cost ~ .+(A+B+C+D+E+F+G)^2, data = data)
summary(model1)
model1pred <- predict(model1, newdata = new.customers)
model1pred

#DECISION TREE
library(tree)
library('sandwich') 
library('party') 
library('rpart') 
model2 <- ctree(cost~., data = data[,-c(2,6)]) 
model3 <- rpart(cost~., data = data) 
model2pred <- predict (model2, newdata = new.customers) 
model3pred <- predict (model3, newdata = new.customers) 
model2pred #ctree
model3pred #rpart

#RANDOM FOREST
library(randomForest)
data_nona <- na.omit(data)
model4 <- randomForest(cost~., data=data_nona, nodesize=5, ntree = 500, mtry = 4)
model4pred <- predict(model4, newdata = new.customers)
model4pred

# Choosing model through OOS
training_data <- data_nona[1:as.integer(nrow(data_nona) * 0.8),]
testing_data <- data_nona[-c(1:as.integer(nrow(data_nona) * 0.8)),]
### train model
testing_model1 <- glm(cost ~.+(A+B+C+D+E+F+G)^2, data = training_data)
testing_model2 <- ctree(cost~., data = training_data[,-c(2,6)])
testing_model3 <- rpart(cost~., data = training_data)
testing_model4 <- randomForest(cost~., data=training_data, nodesize=5, ntree = 500, mtry = 4)
### Predict Using testing data as newdata
testPred1 <- predict(testing_model1, newdata = testing_data)
testPred2 <- predict(testing_model2, newdata = testing_data)
testPred3 <- predict(testing_model3, newdata = testing_data)
testPred4 <- predict(testing_model4, newdata = testing_data)
### Compare with actual data
Test1 <- testing_data %>% mutate(testPredict = testPred1)
Test2 <- testing_data %>% mutate(testPredict = testPred2)
Test3 <- testing_data %>% mutate(testPredict = testPred3)
Test4 <- testing_data %>% mutate(testPredict = testPred4)
### R2 calculating and plotting
RegressionModel <- R2(Test1$cost, Test1$testPredict, family = "gaussian") # 0.4515599
CART_ctree <- R2(Test2$cost, Test2$testPredict, family = "gaussian") # 0.207664
CART_rpart <- R2(Test3$cost, Test3$testPredict, family = "gaussian") # 0.3683401
RandomForest <- R2(Test4$cost, Test4$testPredict, family = "gaussian") # 0.368745
OOS_Perform <- data.frame(Model = c("RegressionModel", "CART_ctree", "CART_rpart", "RandomForest"),
                          R2 = c(RegressionModel, CART_ctree, CART_rpart, RandomForest))
ggplot(OOS_Perform)+
  geom_col(aes(x = Model, y = R2), fill = "lightsteelblue3")+
  geom_text(aes(x = Model, y = R2+0.03, label = round(R2,3)))+
  ylim(0, 0.6)+
  labs(title = "OOS Performance (R2)")+
  theme_bw()

###############################
# Expected Return
compare <- data_nona$cost-predict(model1, newdata = data_nona)
compare <- data.frame(diff = compare)
compare$diff <- as.numeric(compare$diff)
compare <- compare %>% mutate(smaller = compare$diff > 0,) %>% cbind(data_nona)
sd <- sd(compare$diff)
n <- length(compare$diff)
se <- sd/sqrt(n)
ERplot <- data.frame(x = seq(-1,-300, by = -1))
ERplot <- ERplot %>% mutate(tstat = (x-mean(compare$diff))/sd,
                            P_Win = 1-pnorm(tstat, lower.tail = TRUE),
                            OurQuote1 = model1pred[1] + x,
                            OurQuote2 = model1pred[2] + x,
                            OurQuote3 = model1pred[3] + x)
ERplot <- ERplot %>% mutate(ExpectReturn = (OurQuote1+OurQuote2+OurQuote3)*P_Win)
ERplot %>% filter(ExpectReturn == max(ExpectReturn))

ggplot(data = ERplot)+
  geom_line(aes(x = x, y = ExpectReturn))+
  geom_text(aes(x = -66, y = 1600, label = "1639.678"))+
  geom_text(aes(x = -66, y = 500, label = "x= -66"))+
  ylim(500,2000)+
  theme_bw()

ourQuote <- model1pred -66
ourQuote

#Question 5
#NO ANALYSIS NEEDED
