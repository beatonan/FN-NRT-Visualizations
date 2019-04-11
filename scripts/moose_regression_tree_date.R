library(tidyverse)
library(rpart)
library(rpart.plot)
library(here)
library(caret)
library(Metrics)
library(ipred)
library(randomForest)
library(gbm)
library(RANN)
library(glmnet)
library(ranger)
library(caretEnsemble)
library(imputeMissings)

#re-assign input data name to work with code below

ct_in <- date_in 


####------CT model to predict breakup date-------####
date_model <- rpart(formula = doy ~ ., #removing mar_rain by adding code -mar_rain creates a logical model
                  data = ct_in, 
                  method = "anova")
print(date_model)

printcp(date_model)

# plot tree
rpart.plot(x = date_model, yesno = 2, type = 0, extra = 1)

####-----Split data into train/valid/test sets------####
#training set: used to train the original models
#validation set: used to tune model hyperparameters and estimate prediction error for model selection
#test set: used to calculate generalization error of the final chosen model

# # Set seed and create assignment for train, validate and test sets 
# set.seed(1)
# assignment <- sample(1:3, size = nrow(ct_in), prob = c(0.7, 0.15, 0.15), replace = TRUE)
# 
# # Create a train, validation and tests from the original data frame 
# date_train <- ct_in[assignment == 1, ]  # subset date to training indices only
# date_valid <- ct_in[assignment == 2, ]  # subset date to validation indices only
# date_test <- ct_in[assignment == 3, ]   # subset date to test indices only

# Set seed and create assignment for train and test only 
set.seed(1)
assignment <- sample(1:2, size = nrow(ct_in), prob = c(0.7, 0.30), replace = TRUE)

# Create a train, validation and tests from the original data frame 
date_train <- ct_in[assignment == 1, ]  # subset date to training indices only
date_test <- ct_in[assignment == 2, ]   # subset date to test indices only

date_model<- rpart(formula = doy ~ ., #removing mar_rain by adding code -mar_rain creates a logical model
                  data = date_train, 
                  method = "anova")
print(date_model)

printcp(date_model)

# plot tree
rpart.plot(x = date_model, yesno = 2, type = 0, extra = 1)

####-------Evaluate performance of model------####
#Note: RMSE is more sensitive than MAE to error distributions with higher deviations since the errors are squared

# Generate predictions on a test set
pred <- predict(object = date_model,  # model object 
                newdata = date_test)  # test dataset

# Compute the RMSE
rmse(actual = date_test$doy, 
     predicted = pred)

# Compute the MAE
mae(actual = date_test$doy, 
     predicted = pred)

# Compute the bias
bias(actual = date_test$doy, 
    predicted = pred)

#calculate r-squared
cor(date_test$doy,pred, method = "pearson")

xmin <- min(pred, date_test$doy)

#combine pred and actual into df and scatterplot
date_test %>%
  mutate(pred = pred) %>%
  ggplot(aes(x = doy, y = pred))+
  geom_point()+
  geom_abline(linetype = "dotted")+
  xlim(100, 135) + ylim(100, 135)+
  labs(y = "predicted doy", x = "observed doy")+
  theme_bw()

ggsave(filename = here::here("plots", "simple_rt_scattter.png"))

#In regression, R-squared is the proportional improvement in prediction from the regression model, compared to the mean model SST/SSE
#Note: quite a bit of difference in values between different methods of calculation of cor()

####-----Tuning Model Hyperparameters----####
#minsplit: minimum number of data points required before attempting a split; default = 20
#cp: complexity parameter; default = 0.1
## control tree size, smaller value, more complex tree
## 10-fold cross validation with different CP values computed and stored in model object
#maxdeph: maximum depth of the tree; default = 30

##-----Pruning by optimized CP-----##

# Plot the "CP Table"
plotcp(date_model)

# Print the "CP Table"
print(date_model$cptable)

# Retrieve optimal cp value based on cross-validated error
opt_index <- which.min(date_model$cptable[, "xerror"])
cp_opt <- date_model$cptable[opt_index, "CP"]

# Prune the model (to optimized cp value)
date_model_opt <- prune(tree = date_model, 
                         cp = cp_opt)

# Plot the optimized model
rpart.plot(x = date_model_opt, yesno = 2, type = 0, extra = 1)

##-----Grid Search----##

# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(1, 4, 1)
maxdepth <- seq(1, 6, 1)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)

# Check out the grid
head(hyper_grid)

# Print the number of grid combinations
nrow(hyper_grid)

#code at the end of Machine Learning with Tree Based Models in R Chaper 2 shows how to do
#this with for loops but says it can be done using carat package automatically - figure out carat method

####-----Bagging Trees-----####

#ensemble of many models 
#simple average of all models is 
#will reduce varaince and avoid overfitting
#Bootstrap AGGregatING

#Steps:
#Bootstrap sample training dataset (typically select 0.5n with replacement)
#create tree models from ensemble
#predict values using each model
#average values of all models
#averaging reduces variance and leaves bias unchanged

#set seed

# Bagging is a randomized model, so let's set a seed for reproducibility
set.seed(123)

# Train a bagged model
date_model <- bagging(formula = doy ~ ., 
                        data = date_train,
                        coob = TRUE)

# Print the model
print(date_model)

####-----Random Forest Model-----####
#algorithm should recognize that the dependent variable is continuous and perform a regression tree

#impute missing values
ct_in_imp <- impute(ct_in)


set.seed(1)  # for reproducibility
date_rf <- randomForest(doy ~ ., 
                        data = ct_in,
                        importance=TRUE, na.action=na.omit)

print(date_rf)

#####----------simple linear model----------#####

date_lm <- ct_in %>%
  na.omit %>%
  lm(formula = doy ~.)





summary(date_lm)
#does not produce logical output

