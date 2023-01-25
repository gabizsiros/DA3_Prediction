#target - earnings per hour, 4 models

library(tidyverse)
library(caret)
df <- read.csv('morg-2014-emp.csv')
summary(df)


#Selecting marketing and sales managers occ2012 = 50
df <-  df %>%  filter(occ2012 == 50)
summary(df)

#adding wage
df <- df %>% 
  mutate(wage =earnwke/uhours)


fct_count(as.factor(df$grade92), prop = T)
df <-  df %>% filter(grade92 > 38 & grade92 < 45)


fct_count(as.factor(df$ethnic), prop = T) #race is mostly missing 94%

fct_count(as.factor(df$ownchild), prop = T) # keeping all values

fct_count(as.factor(df$sex), prop = T)

#age will not be filtered as grade92 is already filtered
df %>% summarise(
  frequency=n(),
  min = min(age),
  P1 = quantile(age, 0.01), 
  D1 = quantile(age, 0.1), 
  Q1 = quantile(age, 0.25), 
  Me = quantile(age, 0.5), 
  Q3 = quantile(age, 0.75), 
  D9 = quantile(age, 0.9), 
  P99 = quantile(age, 0.99),
  max = max(age))  

hist(df$age)

df %>% select(wage, age, grade92, sex, ownchild) %>%  plot()



df %>% summarise(
  frequency=n(),
  min = min(wage),
  P1 = quantile(wage, 0.01), 
  D1 = quantile(wage, 0.1), 
  Q1 = quantile(wage, 0.25), 
  Me = quantile(wage, 0.5), 
  Q3 = quantile(wage, 0.75), 
  D9 = quantile(wage, 0.9), 
  P99 = quantile(wage, 0.99),
  max = max(wage))  

df <-  df %>% filter(wage < 73 & wage > 7)

#grade factoring (39-44)
df <- df %>% mutate(edu = factor(df$grade92,levels = c(39:44), 
                          labels = c('edu1','edu1',
                                     'edu2','edu2',
                                     'edu3','edu3')),
                    .after = grade92)

df <- df %>%  mutate(df,
                     gender = factor(df$sex, levels = c(1,2), labels = c('male','female')),
                     .after = sex)


ggplot (df, aes(y = wage)) +
  stat_summary_bin(aes(x = age)) +
  geom_smooth(aes(x = age), method = 'loess') +
  theme_bw() 

ggplot (df,aes(y=wage)) +
  geom_point(aes(x = age)) + 
  geom_smooth(aes(x = age), method = 'loess') +
  theme_bw()

ggplot (df, aes(y = wage)) +
  geom_boxplot(aes(x = edu)) + theme_bw() 

ggplot (df, aes(y = wage)) +
  geom_boxplot(aes(x = as.factor(ownchild))) + theme_bw()


model1 <- as.formula(wage ~ age)
model2 <- as.formula(wage ~ age + gender)
model3 <- as.formula(wage ~ age + gender + edu)
model4 <- as.formula(wage ~ age + gender + edu + ownchild)




reg1 <- lm(wage ~ age, data = df)
reg2 <- lm(wage ~ age + gender, data = df)
reg3 <- lm(wage ~ age + gender + edu, data = df)
reg4 <- lm(wage ~ age + gender + edu + ownchild, data = df)

modelsummary::msummary(list(reg1,reg2,reg3, reg4))

# Cross-validation

# set number of folds
k <- 4

set.seed(42)
cv1 <- train(wage ~ age, df, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(42)
cv2 <- train(model2, df, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(42)
cv3 <- train(model3, df, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(42)
cv4 <- train(model4, df, method = "lm", trControl = trainControl(method = "cv", number = k))

data.frame(c("model","RMSE","CV RMSE", "BIC"),
           c("1",RMSE()))

BIC(reg1)
RMSE(predict(reg1))
modelsum$
