#target - earnings per hour, 4 models

rm(list=ls())
setwd("C:/Users/Gabi/Desktop/CEU/DA3_Prediction/A1/")
library(tidyverse)
library(caret)
df <- read.csv('morg-2014-emp.csv')
summary(df)


## Feature enigeneering

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

## Graphical data overview

ggplot (df, aes(y = wage)) +
  stat_summary_bin(aes(x = age)) +
  geom_smooth(aes(x = age), method = 'loess') +
  theme_bw() 

ggplot (df,aes(y=wage)) +
  geom_point(aes(x = age)) + 
  geom_smooth(aes(x = age), method = 'loess') +
  theme_bw()

ggplot (df, aes(y = wage)) +
  geom_boxplot(aes(x = edu, fill = gender)) + theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("Undergrad", "University", "Phd")) +
  labs(x = "",y = "Earning per week ($)", title = "Earnings per week factoring gender and eduction") +
  theme(legend.position = c(0.35,0.85))

ggplot (df, aes(y = wage)) +
  geom_boxplot(aes(x = as.factor(ownchild))) + theme_bw()





###heatmap 
library(Hmisc)

#setting up correlation matrix. occupation has to be dropped since it is a constant in this sample and the martix results in Na
correlation <- rcorr(as.matrix(select(df,-occ2012 & where(is.numeric))))

heatmap(correlation$r)


heatmap(cor(as.matrix(select(df,-c(occ2012,ethnic) & where(is.numeric)))))



model1 <- as.formula(wage ~ age)
model2 <- as.formula(wage ~ age + gender)
model3 <- as.formula(wage ~ age + gender + edu)
model4 <- as.formula(wage ~ age + gender + edu  + uhours)


reg1 <- lm(wage ~ age, data = df)
reg2 <- lm(wage ~ age + gender, data = df)
reg3 <- lm(wage ~ age + gender + edu, data = df)
reg4 <- lm(wage ~ age + gender + edu + uhours, data = df)


models <- c("reg1", "reg2","reg3", "reg4")

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

cv <- c("cv1", "cv2", "cv3", "cv4")

#Compare model performance of these models 
#(a) RMSE in the full sample, 
#(2) cross-validated RMSE and 
#(c) BIC in the full sample. 


library(data.table)


summary <- data.table(Model = integer(), RMSE = double(), `CV RMSE` = double(), BIC = double())
for(i in 1:4) {
  summary <- rbind(summary,  data.table(
    Model = i,
    RMSE = RMSE(predict(get(models[i])),get(models[i])$model$wage),
    `CV RMSE` = get(cv[i])$results$RMSE,
    BIC = BIC(get(models[i]))
  ))
}

print(summary)

ggplot(summary,aes(x= Model)) +
  geom_line(aes(y= RMSE), color = 'green')+
  geom_line(aes(y= `CV RMSE`), color = 'red')+
  #geom_line(aes(y= BIC), color = 'blue')+
  theme_bw()

prediction <- data.table(Prediction = c(predict(reg1),predict(reg2),predict(reg3),predict(reg4)),
                         Actual = c(reg1$model$wage,reg2$model$wage,reg3$model$wage,reg4$model$wage),
                         Model = c('Model1','Model2','Model3','Model4'))


ggplot(prediction)+
  geom_point(aes(x = Prediction, y = Actual, color = Model)) +
  geom_abline(intercept = 0, slope = 1,color = 'black' ,linetype = 'dashed')+
  facet_grid(~Model)+
  theme_bw()
