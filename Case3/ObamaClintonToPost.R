########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
########################################################
source("DataAnalyticsFunctions.R")
# read data into R
election_data <- read.csv("./Case3/ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)

##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
# write.csv(electionDataTrain, "electionDataTrain.csv")
# write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)
View(election_data_train)
###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###

## target variable: Obama_margin_percent

#####Correlation Heatmap #####
library(RColorBrewer)
library(corrplot)
names(corrPlot)
corrDrop <- c("County","State","Region", "ElectionDate","ElectionType", "FIPS","Obama","Obama_wins")
corrPlot <- election_data_train[,!names(election_data_train) %in% corrDrop]
str(corrPlot)
Corr <- cor(corrPlot)
Corr
CorrplotColor <- brewer.pal(n = 8, name = "BrBG")
corrplot(Corr, method = "color",col = CorrplotColor, tl.col = "black")
library(ggplot2) 
library(maps) 
#####

###
### Question 1: Provide a visualization (very flexible format, 
### it does not need to be related to the election)
### 

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

###
### Question 2: Prediction. No additional script beyond the cleaning up the data
### provided above. (Hint: FIPS variable bahaves as an identifier for each observation
### so you might not want to include it in a linear regression.)
###
trainIndex1 <- createDataPartition(election_data_train$Obama_margin_percent, p = 0.8, list = FALSE)
train1 <- election_data_train[trainIndex1, ] #80%
test1 <- election_data_train[-trainIndex1, ] #20%

names(train1)
### Linear Model
drop <- c("County","State", "ElectionDate","ElectionType","Obama_margin","Obama_wins", "TotalVote", "Clinton", "Obama")
train1 <- train1[,!names(train1) %in% drop]
linearModel <- glm(Obama_margin_percent ~., data = train1)
summary(linearModel)
LM_pred <- as.numeric(predict(linearModel, newdata = test1))
LM_perform <- cbind(test1, LM_pred) %>% mutate(LMdiff = LM_pred-Obama_margin_percent)
View(LM_perform)

### CART
library(party)
region_mapping <- c("Northeast"=1, "Midwest" =2, "West" =3, "South" = 4 )
train1$Region <- as.integer(factor(train1$Region, levels = names(region_mapping)))
test1$Region <- as.integer(factor(test1$Region, levels = names(region_mapping)))


tree <- ctree(Obama_margin_percent ~., data = train1) 
tree_pred <- predict(tree, newdata = test1)
tree_perform <- cbind(test1, tree_pred) 
names(tree_perform)[45] <- "tree_pred"
tree_perform <- tree_perform %>% mutate(Treediff = tree_pred - Obama_margin_percent)

### Random Forest




ggplot()+
  geom_abline(slope = 0, intercept = 0, color = "red", size = 2)+
  geom_point(data = LM_perform, aes(x = Obama_margin_percent, y = LMdiff))+
  geom_point(data = tree_perform, aes(x = Obama_margin_percent, y = Treediff), color = "blue")



### Question 3: Keep in mind that unsupervised learning 
### is used to explore the data. Feel free to consider only a subset of the 
### demographic variables. 
###

###
### Question 4. First part: impact of changing hispanic demographic
###
#### 
HispanicSimple <- glm( Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(HispanicSimple)

####
### Question 4. First part: impact of changing black demographic
####
#### 
BlackSimple <- glm( Obama_margin_percent ~ Black, data = election_data_train )
summary(BlackSimple)
####ohh

####
### Question 4. Second part
####
#### Model with 1771 controls to measure the impact of 1% larger Hispanic demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Hispanic

CausalLinear(y,d,x)

a <- summary(glm(y~d+x))
a$coefficients %>% View()

#### Model with 1771 controls to measure the impact of 1% larger Black demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Black


####


####
#### Feel free to compare/contrast your results with the following simple regression model



####
#### Question 5: No additional R code. Keep in mind that you can build upon your previous 
#### analysis or develop new analysis.
####

