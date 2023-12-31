# Section: Finance - Team 2
# Greg Clifford, Freya Ma, Julia Tsai, Qingyang Lui, Smit Shailendra Kesarkar
####################################

#1
library(tidyverse)
library(ggplot2)
plot1 <- DATA %>% select(mom.age, smoke) %>% filter(smoke == 1) %>% group_by(mom.age) %>% count(smoke)
ggplot(data = plot1)+
  geom_point(aes(x = mom.age, y = n), position = position_stack(reverse = TRUE))+
  labs(title = "Pregnancy Women Smoke over Ages", x = "Mom's age", y = "Population")

plot2 <- DATA %>% select(mom.age, smoke) %>% group_by(mom.age) %>% count(smoke)
plot2$smoke <- as.integer(plot1$smoke)
plot2$smoke <- plot2$smoke %>% factor(c(1,0))

ggplot(data = plot2)+
  geom_col(aes(x = mom.age, y = n, fill = smoke), position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = c("1" = "blue", "0" = "black"),
                    labels = c("1" = "yes", "0" = "no")) +
  labs(title = "Pregnancy Women Smoke over Ages", x = "Mom's age", y = "Population")

#####################################
#2 
pvals
alpha <- pvals < 0.05
alpha
ListLabels[alpha]
#Bonferroni correction
new_alpha <- 0.05/45
alpha2 <- pvals < new_alpha
alpha2
ListLabels[alpha2]
####################################
#3
#Mother's weight and average weight
DATA_PIVOT <- DATA %>%
  group_by(mom.age) %>%
  summarize(average_weight = mean(weight, na.rm = TRUE))
qplot(DATA_PIVOT$mom.age, DATA_PIVOT$average_weight)
#Smoker vs. non-smoker and average weight
DATA_PIVOT2 <- DATA %>%
  group_by(smoke) %>%
  summarize(average_weight = mean(weight, na.rm = TRUE))
barplot(DATA_PIVOT2$average_weight, names.arg=DATA_PIVOT2$smoke, xlab = "Smoker", ylab = "Average Weight")
#Cigarettes per day and average weight
DATA_PIVOT3 <- DATA %>%
  group_by(cigsper) %>%
  summarize(average_weight = mean(weight, na.rm = TRUE))
qplot(DATA_PIVOT3$cigsper, DATA_PIVOT3$average_weight)
###################################### 
#4
model1 <- lm(weight ~ black + married + boy + tri1 + tri2 + tri3 + ed.hs + ed.smcol + ed.col + mom.age + smoke + cigsper + m.wtgain + mom.age2, data = DATA)
summary(model1)
 
 
 