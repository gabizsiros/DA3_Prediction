#Download libraries
library(tidyverse)
library(ggplot2)
library(modelsummary)
library(kableExtra)
library(stargazer)
library(caret)
library(data.table)
library(fixest)
library(corrplot)
library(reshape2)
if(!require(geosphere)){
  install.packages("geosphere")
  library(geosphere)
}
library(ranger)


#### Introduction
#In this assignment we use the airbnb dataset from: http://insideairbnb.com/get-the-data.html We chose the city of Rome with more than 24,000 number of observations. We built 3 models to predict the price of accomodation per night for a midsize apartment and discuss their performance.

#### Data exploration
#As we explored the datatable, it contains various types of accomodation in Rome. We decided to narrow down our inquiry to apartments by room_type as 'Entire home/apt.' We also make sure to filter the data for the place to accomodate at least 2 people and not more than 6. We manipulate the data on neighborhoods, so that districts('neighbourhood_cleansed') are represented as factor. We further explore the data with datasummaries to identify our possible independet variables of interest. 

#Download data
listings <- read.csv('listings.csv')

#Exploring the data
#amenities <- unique(listings$amenities)
unique(listings$bathrooms_text)
unique(listings$accommodates)
unique(listings$bathrooms)           
unique(listings$price)
unique(listings$minimum_nights)
table(listings$minimum_nights)
listings$minimum_night
unique(sort(listings$bedrooms))

#Transforming data on amenities into variables
#str <- gsub("\\[|\\]", "", amenities)
#result <- strsplit(str, "\", \"")
#char_vector <- unlist(result)
# get the unique values
#unique_values <- unique(char_vector)

#Filtering out relevant type of accomodation, as we are only interested in separate apartments for 2-6 people.
apartments<-listings %>% filter(room_type=="Entire home/apt") %>% filter(accommodates>=2)%>% filter(accommodates<=6) #14744 observations
#choosing interesting variables for the datatable and further exploration
dt<- apartments %>% select(
  id,
  neighbourhood=neighbourhood_cleansed,
  latitude,
  longitude,
  accommodates,
  bathrooms_text,
  beds,
  amenities,
  price,
  minimum_nights)

#Cleaning data on districts
dt$neighbourhood <- sub(" .*", "", dt$neighbourhood)
dt$neighbourhood <- as.factor(dt$neighbourhood)

#Transforming data on available bathrooms in the property into integer
dt <- dt %>% mutate(bathrooms_num = as.integer(gsub("[a-zA-Z\\s]+", "",
                                                    dt$bathrooms_text)),
                    .after= bathrooms_text )
dt <- dt %>%
  mutate(
    bathrooms_num = ifelse(is.na(dt$bathrooms_num),
                           median(dt$bathrooms_num, na.rm = T),
                           ifelse(bathrooms_num == 0,1,bathrooms_num)
    ), .after = bathrooms_num
  )

#Transform price into numeric
dt$price <- as.numeric(gsub("\\$", "", dt$price))

#Introduce dummies for selected amenities on wifi, free parking and air conditioning.
dt$wifi <- ifelse(grepl("Wifi", dt$amenities), 1, 0)
dt$free_parking <- ifelse(grepl("free.*parking", dt$amenities), 1, 0)
dt$ac <- ifelse(grepl("air.*conditioning", dt$amenities), 1, 0)

#Deciding on further data cleaning based on short-term or long-term rental.
summary(dt$minimum_nights)
quantile(dt$minimum_nights,.95) #95% of the data is less or equal than 5 minimum nights

dt <- dt %>% filter(dt$minimum_nights <= 5)
boxplot(dt$price)

sum(is.na(dt$price))

quantile(dt$price,c(.01,.05,.1,.25,.5,.75,.9,.95,.99), na.rm = T) 

#Having observed the datasummary on price, we will nor consider extreme values of upper 5%(possibly luxury apartments)
dt <- subset(dt, price<= quantile(dt$price, c(.95), na.rm = T))

summary(dt$price)

#Fill in the missing observations on price with the median
dt <- dt %>%
  mutate(
    price = ifelse(is.na(dt$price),mean(dt$price, na.rm = T), price))

fct_count(dt$neighbourhood, sort = T, prop = T) # 56% is in city center

sum(is.na(dt$beds))

#Loading data on the distance to the center 

long_cent <- 12.496366
lat_cent <- 41.902782

dt <- dt %>% mutate(dist = distHaversine(cbind(dt$longitude, dt$latitude), c(long_cent, lat_cent)) / 1000)

#### CORRELATION MATRIX###############################################################
numeric_dt <- keep( dt, is.numeric )
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
cormat <- round(cor(numeric_dt),2)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
cormat<- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))+
  coord_fixed()+
  ggtitle("Correlation Matrix")
print(cormat)

### MODELS #####################################################################

#variables: dist, accommodates, wifi, neighborhood, free_parking, ac

model1 <- as.formula(price ~ dist) ##basic model intiuitively on dist 
model2 <- as.formula(price ~ dist + bathrooms_num + accommodates) ##incl numerical var
model3 <- as.formula(price ~ dist + bathrooms_num + accommodates + wifi + neighbourhood) ## incl signifacant dummies and factor
model4 <- as.formula(price ~ dist + bathrooms_num + accommodates + wifi + neighbourhood + ac + free_parking) ## including all interesting vars

reg1 <- lm(model1, data = dt)
reg2 <- lm(model2, data = dt)
reg3 <- lm(model3, data = dt)
reg4 <- lm(model4, data = dt)


models <- c("reg1", "reg2","reg3", "reg4")

# set number of folds

k <- 5

set.seed(1927)
cv1 <- train(model1, dt, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1927)
cv2 <- train(model2, dt, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1927)
cv3 <- train(model3, dt, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(1927)
cv4 <- train(model4, dt, method = "lm", trControl = trainControl(method = "cv", number = k))

cv <- c("cv1", "cv2", "cv3", "cv4")


modelsummary::msummary(list(reg1,reg2,reg3, reg4))


variables <- c(1,3,5,7)




ols_summary <- data.table(Model = integer(), Variables = integer(), RMSE = double(), MAE = double(), R2 = double(), BIC = double())
for(i in 1:4) {
  ols_summary <- rbind(ols_summary,  data.table(
    Model = paste("Model",i),
    Variables = variables[i],
    RMSE = get(cv[i])$results$RMSE,
    MAE = get(cv[i])$results$MAE,
    R2 = get(cv[i])$results$Rsquared,
    BIC = BIC(get(models[i]))
  ))
}


ols_summary 
### keep 4th model

ols_summary <- ols_summary[4,]

### LASSO #####################################################################



# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = k)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

set.seed(20230125)
lasso_model <- caret::train(
  model4,
  data = dt,
  method = "glmnet",
  preProcess = c("center", "scale"),
  trControl = train_control,
  tuneGrid = tune_grid,
  na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `s1`)  

print(lasso_coeffs)

#lasso_coeffs_nz<-lasso_coeffs %>%
#  filter(coefficient!=0)
#print(nrow(lasso_coeffs_nz))

# DELETE: Evaluate model. CV error:
#lasso_cv_rmse <- lasso_model$results %>%
#  filter(lambda == lasso_model$bestTune$lambda) %>%
#  dplyr::select(RMSE)

#lasso_cv_metrics <- lasso_model$results %>%
#  filter(lambda == lasso_model$bestTune$lambda) %>%
#  dplyr::select(RMSE, Rsquared)

#lasso_metrics <- tibble(
#  model='LASSO',
#  coefficients = nrow(lasso_coeffs_nz), 
#  BIC = NA, 
#  R2 =  lasso_cv_metrics[2][[1]], 
#  RMSE_train = NA,
#  RMSE_test =  lasso_cv_metrics[1][[1]] )

#cv_result <- rbind(cv_result, lasso_metrics)


## selecting the best model for metrics
lasso_cv_metrics <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda)


lasso_summary <- data.table(Model = integer(), Variables = integer(), RMSE = double(), MAE = double(), R2 = double(), BIC = double())
lasso_summary <-  data.table(
    Model = paste("lasso"),
    Variables = variables[4],
    RMSE = lasso_cv_metrics$RMSE,
    MAE = lasso_cv_metrics$MAE,
    R2 = lasso_cv_metrics$Rsquared,
    BIC = NA)


### RANDOM FOREST #############################################################



train_control <- trainControl(method = "cv",
                              number = k,
                              verboseIter = FALSE)
#tune_grid <- expand.grid(
#  .mtry = c(8),
#  .splitrule = "variance",
#  .min.node.size = c(50)
#)


# run model

#set.seed(1927)
#system.time({
#  rf_model_1 <- train(
#    model3,
#    data = dt,
#    method = "ranger",
#    trControl = train_control,
#    tuneGrid = tune_grid,
#    importance = "impurity"
#  )
#})
#rf_model_1





set.seed(1927)
system.time({
  rf_model <- train(
    model4,
    data = dt,
    method = "ranger",
    trControl = train_control,
    tuneGrid = expand.grid(
      .mtry = c(4:9),
      .splitrule = "variance",
      .min.node.size = c(50)
    ),
    importance = "impurity"
  )
})
rf_model
## random forest best result has 7 predictors when selecting model4 formula, with 7 variables

 
set.seed(1927)
system.time({
  rf_model2 <- train(
    model3,
    data = dt,
    method = "ranger",
    trControl = train_control,
    tuneGrid = expand.grid(
      .mtry = c(4:9),
      .splitrule = "variance",
      .min.node.size = c(50)
    ),
    importance = "impurity"
  )
})
rf_model2
## random forest best result has also 7 predictors when selecting model3 formula, with 5 variables


###autotune

#set.seed(1927)
#system.time({
#  rf_model3 <- train(
#    model4,
#    data = dt,
#    method = "ranger",
#    trControl = train_control,
#    mportance = "impurity"
#  )
#})
#rf_model3


forest_summary <- data.table(Model = integer(), Variables = integer(), RMSE = double(), MAE = double(), R2 = double(), BIC = double())
forest_summary <-  data.table(
  Model = paste("random forest"),
  Variables = variables[4],
  RMSE = rf_model$results$RMSE,
  MAE = rf_model$results$MAE,
  R2 = rf_model$results$Rsquared,
  BIC = NA)

#leave 4th rf model, .mtry=7
forest_summary <- forest_summary[4,]

diff_models <- c("ols_summary", "lasso_summary", "forest_summary")

summary_all <- data.table(Model = integer(), Variables = integer(), 
                          RMSE = double(), MAE = double(), R2 = double())

for (i in 1:length(diff_models)) {
  summary_all <-  rbind(summary_all, tibble(
  Model = diff_models[i],
  Variables = get(diff_models[i])$Variables,
  RMSE = get(diff_models[i])$RMSE,
  MAE = get(diff_models[i])$MAE,
  R2 = get(diff_models[i])$R2))}

summary_all$Model <- c("OLS","LASSO","Random Forest")

####PLOTs######################################################################

#data visualisations
fig1<- ggplot(
  data = dt,
  aes(x = neighbourhood, y = price)) +
  geom_boxplot(
    aes(group = neighbourhood),
    linewidth = 0.5, width = 0.6, alpha = 0.8, na.rm=T,
    outlier.shape = NA)+
  labs(x = "Neighbourhood",y = "Price(in EUR)")
fig1
#density plot on distance and price
fig2<- ggplot(dt, aes(x = dist, y=price)) +
  stat_summary_bin(bins = 100) +
  theme_bw() +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1)) +
  scale_x_continuous(labels = function (x) {
    paste(formatC(x,format = "d",big.mark =","),"km")
  })+
  labs(title = 'Distance')
fig2
fig3<-ggplot(dt, aes(x=price)) +
  geom_density(alpha=0.3) +
  theme_bw() +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1)) +
  scale_x_continuous(labels = function (x) {
    paste(formatC(x,format = "d",big.mark =","),"EUR")
  })+
  labs(title = 'Price distribution')
fig3


fig4<- ggplot(dt, aes(x = dist, y=price)) +
  geom_point(color = 'purple', alpha = 0.2) +
  geom_smooth(method =  loess, color = 'blue') +
  theme_bw() +
  theme(legend.position="top") +
  guides(fill=guide_legend(nrow=1)) +
  scale_x_continuous(labels = function (x) {
    paste(formatC(x,format = "d",big.mark =","),"km")
  })+
  labs(title = 'Price vs Distance from the city center')
fig4

##Extra task
#Loading and cleaning the calendar data
calendar <- read.csv("calendar.csv")
calendar <- calendar%>%
  rename(
    id = listing_id,
    price_extra=price)%>%
  select(id, date, price_extra)
calendar$price_extra <- as.numeric(gsub("\\$", "", calendar$price_extra))


calendar <- calendar %>%
  group_by(id) %>%
  summarise(mean = mean(price_extra))


#Joining the data tables
data_extra <- left_join(dt, calendar, by = "id")
data_extra <- data_extra %>% rename( future_price = mean)

data_extra <- data_extra %>% mutate(OLS_pred = predict(cv4),
                                    Lasso_pred = predict(lasso_model),
                                    RF_pred = predict(rf_model))



### validity

#future price has 409 NAs. replacing with mean

data_extra <- data_extra %>%
  mutate(
    future_price = ifelse(is.na(data_extra$future_price),
                          mean(data_extra$future_price, na.rm = T), 
                          future_price))


summary(data_extra$future_price)

futureOLS <-  RMSE(data_extra$OLS_pred,data_extra$future_price)
futureLASSO <- RMSE(data_extra$Lasso_pred,data_extra$future_price)
futureRF <- RMSE(data_extra$RF_pred,data_extra$future_price)

valid <- data.frame(
  Model = summary_all$Model,
  Past_Data_RMSE = summary_all$RMSE,
  Future_Data_RMSE = c(futureOLS, futureLASSO, futureRF)
)



ggplot( data= data_extra, aes(x= future_price))+
  geom_point(aes(y= RF_pred),colour = 'red')+
  geom_abline(intercept = 0, slope = 1,color = 'black' ,linetype = 'dashed')


data_extra2 <- data_extra %>% select(price, future_price, OLS_pred,Lasso_pred,RF_pred)
# Reshape the data into a long format
data_long <- gather(data_extra2, key = "Series", value = "Price", OLS_pred, Lasso_pred, RF_pred, price, future_price)

# Plot the data using ggplot2
ggplot(data = data_long, aes(x = Price, color = Series)) +
  geom_density() +
  scale_color_manual(values = c("OLS_pred" = "orange", "Lasso_pred" = "blue", "RF_pred" = "red", "price" = "black", "future_price" = "green")) +
  labs(x = "", y = "Price") +
  theme_classic()

