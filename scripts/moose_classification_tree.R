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

#import data (see current data exploration)

#run data munging script to get data in format required for classifaction tree modelling

####------CT model to predict breakup severity using Moosonee Climate and Baseline Hydrology Metrics-------####
mf_ct <- rpart(formula = severity ~ ., 
                  data = mf_clim_in, 
                  method = "class",
                  control = rpart.control(minsplit = 1))

print(mf_ct)

printcp(mf_ct)

# plot tree
rpart.plot(x = mf_ct, yesno = 2, type = 0, extra = 1)



####------CT model to predict breakup severity using Moosonee Climate and Baseline Hydrology Metrics-------####
moose_ct <- rpart(formula = severity ~ ., 
                  data = ct_clim_in, 
                  method = "class",
                  control = rpart.control(minsplit = 7))

print(moose_ct)

printcp(moose_ct)

# plot tree
rpart.plot(x = moose_ct, yesno = 2, type = 0, extra = 1)



####------CT model using WISKI WL metrics-------####
moose_ct <- rpart(formula = severity ~ ., 
                      data = ct_input, 
                      method = "class")

print(moose_ct)

# Display the results **does not work?
rpart.plot(x = moose_ct, type = 0, extra = 0)

rpart.plot(x = moose_ct)

####------CT model using Baseline Hydrology metrics-------####

# bl_ct_in_filt <- bl_ct_in %>%
#   select(severity,ice_days)

bl_hydro_ct <- rpart(formula = severity ~ ., 
                  data = bl_ct_in, 
                  method = "class")

print(bl_hydro_ct)

printcp(bl_hydro_ct)

# Display the results 
rpart.plot(x = bl_hydro_ct, yesno = 2)

rpart.plot(x = bl_hydro_ct)

####--------train/test split-------####

#if using this technique make sure the subset is random (not the first 80% and last 20%)

# Total number of rows in the credit data frame
n <- nrow(ct_clim_in)
# Number of rows for the training set (80% of the dataset)
n_train <- round(0.8 * n) 
# Create a vector of indices which is an 80% random sample
set.seed(13)
train_indices <- sample(1:n, n_train)
# Subset the data frame to training indices only
moose_train <- ct_clim_in[train_indices, ]  
# Exclude the training indices to create the test set
moose_test <- ct_clim_in[-train_indices, ]

#generate training model

moose_train_ct <- rpart(formula = severity ~ ., 
                     data = moose_train, 
                     method = "class")

print(moose_train_ct)

# Generate predicted classes using the model object
class_prediction <- predict(object = bl_hydro_ct,  
                            newdata = moose_train,  
                            type = "class") 

# Calculate the confusion matrix for the test set
conf_matrix <- confusionMatrix(data = class_prediction,         
                reference = moose_train$severity) 

####------Model Comparison-----####
#impurity measure (Gini Index)
#rpart will select the classification that minimizes the Gini index
#other metrics include entropy, information gained..ect. figure these out

# Train a gini-based model

moose_gini <- rpart(formula = severity ~ . -q_max -year, 
                       data = ct_clim_in, 
                       method = "class",
                       parms = list(split = "gini"))

print(moose_gini)

rpart.plot(x = moose_gini, type = 0, extra = 1)

# Train a information-based model
moose_info <- rpart(formula = severity ~ . -year, 
                    data = ct_clim_in, 
                    method = "class",
                    parms = list(split = "information"))

print(moose_info)

# Generate predictions on the validation set using the gini model
pred1 <- predict(object = moose_gini,
                 newdata = moose_test,
                 type = "class")    

# Generate predictions on the validation set using the information model
pred2 <- predict(object = moose_info, 
                 newdata = moose_test,
                 type = "class")

# Compare classification error
ce(actual = moose_test$severity, 
   predicted = pred1)
ce(actual = moose_test$severity, 
   predicted = pred2) 

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

flood_model <- bagging(formula = severity ~ ., 
                      data = moose_train,
                      coob = TRUE)

# Print the model
print(flood_model)

#useful classification metrics in confusion matrix output
#ROC curve and AUC (area under curve)
##1 is perfect classification, 0.5 is no better than random chance; 0 means all observations are classified wrong

# Generate predicted classes using the model object
class_prediction <- predict(object = flood_model, 
                            newdata = moose_test,  
                            type = "class")         # return classification labels
# Print the predicted classes
print(class_prediction)

# Calculate the confusion matrix for the test set
confusionMatrix(data = class_prediction,         
                reference = moose_test$severity) 

# Generate predictions on the test set
pred <- predict(object = flood_model, 
                newdata = moose_test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)                

# Compute the AUC (`actual` must be a binary vector)
auc(actual = ifelse(moose_test$severity == "severe", 1, 0), 
    predicted = pred[,"severe"])  

####------Cross Validation-------####

# Specify the training configuration
ctrl <- trainControl(method = "cv",     # Cross-validation
                     number = 20,        # 5 folds
                     classProbs = TRUE,                  # For AUC
                     summaryFunction = twoClassSummary)  # For AUC


# Cross validate the severity model using "treebag" method; 
# Track AUC (Area under the ROC curve)

ct_clim_in_na <- ct_clim_in %>%
  na.omit() %>%
  mutate(severity = if_else(severity == "severe", "severe", "non.severe")) 

set.seed(1)  # for reproducibility
#develop model with cross validation (throws error)
moose_caret_model <- train(severity ~ ., 
                            data = ct_clim_in_na, 
                            method = "treebag",
                            metric = "ROC",
                            trControl = ctrl)
print(moose_caret_model)

# Generate predictions on the test set
pred <- predict(object = moose_caret_model, 
                newdata = moose_test,
                type = "prob")

# Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
auc(actual = ifelse(moose_test$severity == "yes", 1, 0), 
    predicted = pred[,"severity"])

####---------Random Forest Model----------####
#similar to bagged method but randomness is added by using a subset of variables to determine splitting
#this creates trees that are less correlated which improves overall model performance

# Train a Random Forest
moose_train_na <- moose_train %>%
  select(-q_max, -dam_out, -ice_on, -ice_off, -ice_days, -doy.y, -year) %>%
  na.omit()

set.seed(1)  # for reproducibility
moose_rf <- randomForest(formula = severity ~ ., 
                             data = moose_train_na)

# Print the model output                             
print(moose_rf)

#default number of variables (or m tries) is the square root of the number of variables
#OOB (out of bag) error estimate; error based on data not selected in the bootstrapped sample

# Grab OOB error matrix & take a look
err <- moose_rf$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
#corresponds to error displayed using print function above
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

#plot model
plot(moose_rf)
# Add a legend since it doesn't have one by default
legend(x = "right", 
       legend = colnames(err),
       fill = 1:ncol(err))


# Generate predicted classes using the model object
class_prediction <- predict(object = moose_rf,  # model object 
                            newdata = moose_test,  # test dataset
                            type = "class")         # return classification labels

# Calculate the confusion matrix for the test set
cm <- confusionMatrix(data = class_prediction,          # predicted classes
                      reference = moose_test$severity)  # actual classes
print(cm)

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)

# Generate predictions on the test set
pred <- predict(object = moose_rf, 
                newdata = moose_test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)                

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
auc(actual = ifelse(moose_test$severity == "severe", 1, 0), 
    predicted = pred[,"severe"]) 

####----Tuning Random Forest Hyperparameters----####
#ntree; number of trees in iteration
#mtry; number of variables sampled at each split ***one of the most important
#sampsize; number of samples to train on; default - 62.3%
#nodesize; number of samples in the terminal nodes
#maxnodes; max number of terminal nodes (use to limit tree growth)

#can use tuneRF() to tune mtry paramter

# Execute the tuning process


set.seed(1)              
res <- tuneRF(x = subset(moose_train_na, select = -severity),
              y = moose_train_na$severity,
              ntreeTry = 500)

# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)

# If you just want to return the best RF model (rather than results)
# you can set `doBest = TRUE` in `tuneRF()` to return the best RF model
# instead of a set performance matrix.

##----Tuning Via Tree Depth by looping through a generated df of test parameters

# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(moose_train_na) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(moose_train_na) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = severity ~ ., 
                        data = moose_train_na,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

####---------Boosted Trees---------####
#2 Algorithms - Gradient Boosting Machine (GBM); Adaboost
#Adaboost;
#1 - train model with each observation assigned equal weight
#2 - weight observations based ease of classification (easy classified low weight)
#3 - grow tree with weighted data
#4 - calculate classification error from ensemble of weighted and unweighted model
#5 - grow 3rd tree to predict residuals (don't really get this)
#6 - repeat steps 1-5
#GMB; similar to adaboost but "shortcomings" are identified by gradients not high weights
#in short; GBM considers past fits when iterativly generating models
#pros; when tuned properly, will outperform other ML algorithms(including deep learning)on many datasets
#pros; can optimize to user defined cost function
#cons; can overfit data and is sensitive to outliers and noise

# Convert "yes" to 1, "no" to 0
moose_train$severity<- ifelse(moose_train$severity == "yes", 1, 0)
moose_train_na$severity<- ifelse(moose_train_na$severity == "yes", 1, 0)
ct_clim_in$severity<- ifelse(ct_clim_in$severity == "yes", 1, 0)

# Train a 10000-tree GBM model
set.seed(1)
moose_gbm <- gbm(formula = severity ~ ., 
                    distribution = "bernoulli", 
                    data = ct_clim_in,
                    n.trees = 10000)

# Print the model object                    
print(moose_gbm) 

# summary() prints variable importance
summary(moose_gbm)
#produces a model with no valid predictors so the summary function does not work
#can output variable importance here and in RF model - would be good to look at this

##----Prediction Usinf a GBM----##

# Since we converted the training response col, let's also convert the test response col
moose_test$severity <- ifelse(moose_test$severity == "yes", 1, 0)

# Generate predictions on the test set
preds1 <- predict(object = moose_gbm, 
                  newdata = moose_test,
                  n.trees = 10000)

# Generate predictions on the test set (scale to response)
preds2 <- predict(object = moose_gbm, 
                  newdata = moose_test,
                  n.trees = 10000,
                  type = "response")

# Compare the range of the two sets of predictions
range(preds1)
range(preds2)
#does not produce logical results because the GBM model did not identify any relevant predictor variables

####-----------Data Pre-processing for ML (from datacamp ML Toolbox Course "preprocessing your data chapter")
#median imputation; caret package calculates the median based on each bootstrapped sample to imporve accuracy

#create matrix of independent variables
moose_x <- ct_in %>%
  select(-severity, -doy) %>%
  as.matrix()
#create factor of dependent variable
moose_y <- ct_in$severity
moose_doy <- ct_in$doy 

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Apply median imputation: model
model1 <- train(
  x = moose_x, y = moose_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

model1
#severity issue: did not identify any relevant parameters and model did not converge
#doy issue: myControl is not setup properly for a regression model; may be other issues

#KNN (K nearest neighbour) imputation 
#imputes based on "similar" non-missing rows
#valuable if missing values are not random ex. if missing values were at high flow years (not the case)
#NOTE: should try both and see which produces a better result

# Apply KNN imputation: model2
model2 <- train(
  x = moose_x, y = moose_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print model to console
model2
#severity issue: did not identify any relevant parameters

#preprocessing steps:
#always use median imputation first and compare to KNN
#always centre and scale for GLM's
#worth trying PCA and spatial sign transformations for linear models
#NOTE: tree based models usually OK with just median imputation

#No (or low) variance variables
#nearly constant variables can cause issues with models 

# Identify near zero variance predictors: remove_cols

#create a df with just predictor variables
var_chk <- ct_in %>%
  select(-severity)

# Identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(var_chk, freqCut = 2, uniqueCut = 20)
#does not find any near zero variance columns (becauase of NA values?)

# Get all column names from var_chk: all_cols
all_cols <- names(var_chk)

# Remove from data: bloodbrain_x_small
moose_x_small <- moose_x[ , setdiff(all_cols, remove_cols)]

####------PCA as a pre-processing step which reduces low variance predictors and collinear predictors into Principal Components---------####
#the pca option in the preProcess argument will center and scale your data, combine low variance variables, and ensure that all of your predictors are orthogonal. 
#This creates an ideal dataset for linear regression modeling, and can often improve the accuracy of your models.


# Fit glm model using PCA: model
model <- train(
  x = moose_x, y = moose_y,
  method = "glm", preProcess = "pca"
)

# Print model to console
model


####------Building and Comparing Models (ML Toolbox Ch 5)

# Create custom indices: myFolds
myFolds <- createFolds(moose_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

#suggest always starting with GLM net because it is simple, fast and has LM coefficients that canbe used to help interperet data

# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = moose_x, y = moose_y,
  metric = "ROC",
  method = "glmnet",
  preProcess = "medianImpute",
  trControl = myControl
)

#try a random forest model
#slower and less interperable than glmnet
#BUT often more accurate and easier to tune (no need to transform data), also capture thresholds and variable interactions
#NOTE: example below uses the "ranger" package which is faster, more stable and uses less memory than "randomForest" package

# Fit random forest: model_rf
model_rf <- train(
  x = moose_x, y = moose_y,
  metric = "ROC",
  method = "ranger",
  preProcess = "medianImpute",
  trControl = myControl
)

##--------Compare Models----------##
#ensure models were fit using the same data
#selection criteria:
##highest average AUC and lowest SD in AUC
#can use resamples() function in caret package to compare models provided they have the same training data and same trainControl object

# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_rf)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

#plots of resamples so help identify stregths and weaknesses of each model
#In general, you want the model with the higher median AUC, as well as a smaller range between min and max AUC.

# Create bwplot
bwplot(resamples, metric = "ROC")
bwplot(resamples)

dotplot(resamples, metric = "ROC")
densityplot(resamples, metric = "ROC") #kernel density plot
xyplot(resamples, metric = "ROC") #shows the ROC per fold of cross-validation; if most points are above or below the 1:1 line, then it is a good indication of comaprative model performance

#create an Ensemble of models: Note: this is from a "teaser" excercise at the end of the course and does not provide much instruction
#code below does not work as there is a specific way to generate the model list using the caretEnsemble package that is not provided

# Create ensemble model: stack
stack <- caretStack(model_list, method = "glm")

# Look at summary
summary(stack)


####------Test methods using credit dataset------####

#test rpart with credit data:

credit_data_path <- here::here("import", "index.csv")
credit <- read.csv(credit_data_path)

credit_ct <- rpart(formula = Creditability ~ ., 
                   data = credit, 
                   method = "class")

# Display the results
rpart.plot(x = credit_ct, yesno = 2, type = 0, extra = 0)

