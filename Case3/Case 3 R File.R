library(caret)
library(glmnet)
library(randomForest)
library(dplyr)
library(tidyverse)
library(cluster)
election_data <- read.csv("./Case3/ElectionDataAlone.csv")
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)
(data_mean)
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
summary(election_data)

#Factor conversion
election_data$County <- as.factor(election_data$County)
election_data$State <- as.factor(election_data$State)
election_data$FIPS <- as.factor(election_data$FIPS)
election_data$Region <- as.factor(election_data$Region)
election_data$ElectionType <- as.factor(election_data$ElectionType)

#Split the original file
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

#Add margins
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)

##### Question 1 #####

##plot1
us_map <- map_data("state") 
state_abbr_mapping <- data.frame(
  State = tolower(c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")),
  Abbreviation = c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)
us_map$State <- state_abbr_mapping$Abbreviation[match(us_map$region, state_abbr_mapping$State)]
state_map <- merge(us_map, election_data_train, by.x = "State", by.y = "State", all.x = TRUE)
ggplot(state_map)+
  geom_map(aes(fill = Obama_margin_percent, x =  long, y = lat, map_id = region), map = us_map, colour="white", size = 0.5)+
  labs(title = "Obama's Margin in States", fill = "Margin(%)") +
  theme_void()

##plot2
a<-election_data_train %>% group_by(Region) %>% summarize(ObamaVotes = mean(Obama/TotalVote)*100, PovertyRate = mean(Poverty))%>%
  pivot_longer(-c(1), names_to = "type", values_to = "Percent" )
ggplot(data = a)+
  geom_col(aes(x = Region, y = Percent, fill = type), position = position_dodge(width = 0.8))+
  geom_text(aes(x = Region, y = Percent, label = round(Percent,2)))+
  labs(title = "Votes for Obama (%) vs Poverty Rate (%)")

##### Question 2 #####

#### here are some question we asked ChatGPT for the whole process: https://chat.openai.com/c/0966bd8b-a7f0-41d3-96ae-fa97589535be

#Split the training data for OOS
set.seed(123456) 
train_indices <- sample(1:nrow(election_data_train), 0.8 * nrow(election_data_train))
train_data_OOS <- election_data_train[train_indices, ]
test_data_OOS <- election_data_train[-train_indices, ]  


#Column deletion
test_data <- subset(test_data_OOS, select = -c(County, State, Region, FIPS))
train_data <- subset(train_data_OOS, select = -c(County, State, Region, FIPS))
election_data_test <- subset(election_data_test, select = -c(County, State, Region, FIPS))

#Total vote count
a <- model.matrix(TotalVote ~ . - 1, train_data)
b <- train_data$TotalVote
lasso <- glmnet(a, b, alpha=1)
cv.lasso <- cv.glmnet(a, b, alpha=1)
best_lambda <- cv.lasso$lambda.min
coef(lasso, s=best_lambda)
prediction_lasso <- predict(lasso, newx=a, s=best_lambda)
plot(lasso)
plot(cv.lasso)

# Extract the coefficients from the Lasso model at the optimal lambda
lasso_coefficients <- coef(lasso, s = best_lambda)

# Filter out the variables with non-zero coefficients
selected_vars <- lasso_coefficients[lasso_coefficients[,1] != 0, , drop = FALSE]

# Print the names of the selected variables
print(rownames(selected_vars))


#Obama
c <- model.matrix(Obama ~ . - 1, train_data)
d <- train_data$Obama
lassoO <- glmnet(c, d, alpha=1)
cv.lassoO <- cv.glmnet(c, d, alpha=1)
best_lambdaO <- cv.lasso$lambda.min
coef(lasso, s=best_lambda)
prediction_lassoO <- predict(lasso, newx=c, s=best_lambda)
plot(lassoO)
plot(cv.lassoO)

# Extract the coefficients from the Lasso model at the optimal lambda
lasso_coefficientsO <- coef(lassoO, s = best_lambdaO)

# Filter out the variables with non-zero coefficients
selected_varsO <- lasso_coefficientsO[lasso_coefficientsO[,1] != 0, , drop = FALSE]

# Print the names of the selected variables
print(rownames(selected_varsO))

#Random forest TotalVote on OOS
random_total <- randomForest(TotalVote ~ ., data = train_data, ntree = 100, mtry = 8, importance = TRUE)
print(random_total)
predictions_total <- predict(random_total, newdata=test_data)
MAE_T <- mean(abs(predictions_total - test_data$TotalVote))
MAE_T
plot(random_total)
varImpPlot(random_total)

#Random forest Obama on OOS
random_obama <- randomForest(Obama ~ ., data = train_data, ntree = 100, mtry = 8, importance = TRUE)
print(random_obama)
predictions_obama <- predict(random_obama, newdata=test_data)
MAE_O <- mean(abs(predictions_obama - test_data$Obama))
MAE_O
plot(random_obama)
varImpPlot(random_obama)

#Random forest Clinton on OOS
random_clinton <- randomForest(Clinton ~ ., data = train_data, ntree = 100, mtry = 8, importance = TRUE)
print(random_clinton)
predictions_clinton <- predict(random_clinton, newdata=test_data)
MAE_C <- mean(abs(predictions_clinton - test_data$Clinton))
MAE_C
plot(random_clinton)
varImpPlot(random_clinton)

#Random forest TotalVote on TEST
total_test <- randomForest(TotalVote ~ ., data = train_data, ntree = 100, mtry = 8, importance = TRUE)
print(total_test)
predictions_total_test <- predict(total_test, newdata=election_data_test)
MAE_TT <- mean(abs(predictions_total_test - election_data_test$TotalVote))
MAE_TT
plot(total_test)
varImpPlot(total_test)

#Random forest Obama on TEST
random_obama <- randomForest(Obama ~ ., data = train_data, ntree = 100, mtry = 8, importance = TRUE)
print(random_obama)
predictions_obama <- predict(random_obama, newdata=test_data)
MAE_O <- mean(abs(predictions_obama - test_data$Obama))
MAE_O
plot(random_obama)
varImpPlot(random_obama)

#Random forest Clinton on TEST
random_clinton <- randomForest(Clinton ~ ., data = train_data, ntree = 100, mtry = 8, importance = TRUE)
print(random_clinton)
predictions_clinton <- predict(random_clinton, newdata=test_data)
MAE_C <- mean(abs(predictions_clinton - test_data$Clinton))
MAE_C
plot(random_clinton)
varImpPlot(random_clinton)

#Question 3

subset3 <- select(election_data, -County, -State, - Region, -FIPS, -ElectionDate, -ElectionType, -TotalVote, -Obama, -Clinton)
df3 <- scale(subset3)


# Number of clusters to consider
max_k <- 15 
wss <- numeric(max_k)

for (k in 1:max_k) {
  kmeans_result <- kmeans(df3, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

# Plot the total within sum of squares vs. number of clusters
tibble(k = 1:max_k, wss = wss) %>%
  ggplot(aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  ggtitle("Elbow Method for Optimal k") +
  xlab("Number of clusters (k)") +
  ylab("Total Within Sum of Squares")

#KMeans
set.seed(123)
km.res <- kmeans(df3, 4, nstart = 25)
print(km.res)
aggregate(election_data, by=list(cluster=km.res$cluster), mean)
dd <- cbind(election_data, cluster = km.res$cluster)
head(dd)

#Heatmap
cluster_centers <- km.res$centers

heatmap(as.matrix(cluster_centers), 
        main="Cluster Centers", 
        scale="row",
        Colv=NA)  
heatmap(as.matrix(cluster_centers), labRow = row_labels, labCol = column_labels, 
        cexRow = 0.8, cexCol = 0.8, srtCol = 45)
