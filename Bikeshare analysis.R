library(vroom)
library(rpart)
library(tidymodels)
library(tidyverse)

# Load data
trainData <- vroom("train.csv")
testData <- vroom("test.csv")

#### Cleaning ####
trainData <- trainData|>
  select(-casual, -registered)|>
  mutate(count = log(count))
#### Feature Engineering ####
my_recipe <- recipe(count ~., data = trainData)|>
  step_mutate(weather = factor(ifelse(weather == 4, 3, weather)))|> # combines weather labeled 4 in with 3, and turns weather into a factor   
  step_mutate(season = factor(season))|> # turns season into a factor
  step_time(datetime, features = "hour")|> # takes in hour of the day in 24-hour format in "datetime" and creates a new column with just the hour
  step_date(datetime, features = c("dow","month","year"))|> # makes dow, month, and year columns from the "datetime" columns. 
  step_rm(datetime)|># removes the column datetime
  step_dummy(all_nominal_predictors())|>
  step_normalize(all_numeric_predictors())|>
  step_zv(all_predictors()) # removes predictors with no variance 

prepped_recipe <- prep(my_recipe)

baked_train <- prepped_recipe|>
  bake(new_data = NULL)

baked_test <- prepped_recipe|>
  bake(new_data = testData)
    
glimpse(baked_train)
head(baked_train)

vroom_write(x = baked_train, file = "./backed_train.csv", delim = ",")
vroom_write(x = baked_test, file = "./baked_test.csv", delim = ",")
#### Linear Regression ####

# Defining the model 

my_linear_model <- linear_reg() |> 
  set_engine("lm")|>
  set_mode("regression") 

# workflow and fit 

my_workflow <- workflow()|>
  add_recipe(my_recipe)|>
  add_model(my_linear_model)|>
  fit(data = trainData)


# predictions and Kaggle submission 
# Generate predictions on test set

lin_preds <- predict(my_workflow, new_data = testData)

# Format predictions for Kaggle submission
kaggle_submission <- lin_preds |> 
  bind_cols(testData) |>  # Bind predictions with test data
  select(datetime, .pred) |> 
  rename(count = .pred) |> 
  mutate(count = exp(count),        # Back-transform predictions
         count = pmax(0, count),    # Ensure non-negative
         datetime = as.character(format(datetime)))  # Proper datetime format

# Write submission file
vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")

#### penalized regression ####
preg_model <- linear_reg(penalty = tune(), mixture = tune())|> # .01, 1 = 1.006
  set_engine("glmnet")

preg_workflow <- workflow()|>
  add_recipe(my_recipe)|>
  add_model(preg_model)

# Grid of values to tune over

grid_of_tuning_params <- grid_regular(penalty(),
                                     mixture(),
                                     levels = 4)

# spiting up the data for CV
folds <- vfold_cv(trainData, v = 5, repeats = 1)

# running the CV
CV_results <- preg_workflow|>
  tune_grid(resamples = folds, 
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse,mae))

# plot of the results 
collect_metrics(CV_results)|>
  filter(.metric == "rmse")|>
  ggplot(aes(x = penalty, y = mean, color = factor(mixture)))+
  geom_line()

# find best tuning parameters 
bestTune <- CV_reults|>
  select_best(metric = "rmse")
bestTune

# predictions
lin_preds_pen <- predict(preg_workflow, new_data = testData)

# formatting for kaggle submission
kaggle_submission <- lin_preds_pen |> 
  bind_cols(testData) |>  # Bind predictions with test data
  select(datetime, .pred) |> 
  rename(count = .pred) |> 
  mutate(count = exp(count),        # Back-transform predictions
         count = pmax(0, count),    # Ensure non-negative
         datetime = as.character(format(datetime)))  # Proper datetime format

# Write submission file
vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")

#### Regression Trees ####
my_tree_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n = tune())|>
  set_engine("rpart")|>
  set_mode("regression")

# workflow 
tree_workflow <- workflow()|>
  add_recipe(my_recipe)|>
  add_model(my_tree_mod)
# Grid of values to tune over
grid_of_tuning_params <- grid_regular(tree_depth(),
                                      cost_complexity(),
                                      min_n(),
                                      levels = 4)
# spiting up the data for CV
folds <- vfold_cv(trainData, v = 5, repeats = 1)

# running the CV
CV_results <- tree_workflow|>
  tune_grid(resamples = folds, 
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse,mae))

# find best tuning parameters 
bestTune <- CV_results|>
  select_best(metric = "rmse")
bestTune

final_wf<- tree_workflow|>
  finalize_workflow(bestTune)|>
  fit(data = trainData)

# prediction
 tree_predict <- final_wf|>
  predict(new_data = testData)

# formatting for kaggle submission
kaggle_submission <- tree_predict |> 
  bind_cols(testData) |>  # Bind predictions with test data
  select(datetime, .pred) |> 
  rename(count = .pred) |> 
  mutate(count = exp(count),        # Back-transform predictions
         count = pmax(0, count),    # Ensure non-negative
         datetime = as.character(format(datetime)))  # Proper datetime format

vroom_write(x = kaggle_submission, file = "./treePreds.csv", delim = ",")

#### Random forests #### 
my_forest_mod <- rand_forest(mtry = tune(),
                             min_n = tune(),
                             trees = 500)|>
  set_engine("ranger")|>
  set_mode("regression")

# workflow
forest_workflow <- workflow()|>
  add_recipe(my_recipe)|>
  add_model(my_forest_mod)

mygrid <- grid_regular(mtry(range = c(1, 30)),
                       min_n(range = c(2,10)),
                       levels = 5)
# spiting up the data for CV
folds <- vfold_cv(trainData, v = 5, repeats = 1)

# running the CV
CV_results <- forest_workflow|>
  tune_grid(resamples = folds, 
            grid = mygrid,
            metrics = metric_set(rmse,mae))

# find best tuning parameters 
bestTune <- CV_results|>
  select_best(metric = "rmse")
bestTune

final_wf<- forest_workflow|>
  finalize_workflow(bestTune)|>
  fit(data = trainData)

# predictions
forest_predict <- final_wf|>
  predict(new_data = testData)

# formatting for kaggle submission
Kaggle_submission <- forest_predict |> 
  bind_cols(testData) |>  # Bind predictions with test data
  select(datetime, .pred) |> 
  rename(count = .pred) |> 
  mutate(count = exp(count),        # Back-transform predictions
         count = pmax(0, count),    # Ensure non-negative
         datetime = as.character(format(datetime)))  # Proper datetime format

vroom_write(x = Kaggle_submission, file = "./forestPreds.csv", delim = ",")

