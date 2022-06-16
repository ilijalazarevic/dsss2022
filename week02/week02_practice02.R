# load libraries
library(tidyverse)
library(glmnet)
library(car)

# define constants
WORK_DIR <- getwd()
DATA_DIR <- paste0(WORK_DIR, '/_data')
DATA_FILE <- paste0(DATA_DIR, '/kc_house_data.csv')

# load data
data.csv <- read.csv(DATA_FILE)
data.df <- as.data.frame(data.csv)

# instpect the data
str(data.df)
summary(data.df)

ggplot(data=data.df, mapping=aes(x=price)) +
  geom_histogram() + 
  ggtitle('Histogram of property price values.')

# pairs(select_if(data.df, is.numeric))
# select_if(data.df, is.numeric) %>% select(-all_of('id')) %>% pairs

data.df.numericals <- select_if(data.df, is.numeric)

cor.df <- data.df.numericals %>% 
  select(-price) %>% 
  cor %>% 
  as.data.frame() %>%
  rownames_to_column('col')

pivot_longer(cor.df, cols=-all_of(c("col")), names_to = 'col_1', values_to = 'val') %>%
  ggplot(mapping = aes(x=col, y=col_1, fill=val)) +
  geom_tile()

ggplot(data=data.df, mapping=aes(x=log(price))) +
  geom_histogram() + 
  ggtitle('Histogram of log2 property price values.')

# TRAIN MODELS

# Prepare data for training
model_set <- data.df %>% 
  select(-all_of(c("id", "lat", "long", "date",
                   "zipcode", "yr_renovated", "waterfront", 
                   "view", "sqft_basement")))

# train linear model
linear_model <- lm(price ~ ., data = model_set)
# get summary
summary(linear_model)
# get variation inflation for each predictor
car::vif(linear_model) %>% as.data.frame()

# train Ridge regularized linear model
X <- model_set %>% select(-price)
y <- model_set$price

glmnet_mlr <- glmnet(x = X, y = y, 
                     alpha = 0, 
                     family = "gaussian")
plot(glmnet_mlr, label = TRUE)

# get all values of lambda
glmnet_mlr$lambda
# get value for deviance
deviance(glmnet_mlr)

best_lambda_index <- which.min(deviance(glmnet_mlr))
best_lambda <- glmnet_mlr$lambda[best_lambda_index]

# train model with best lambda chosen
X <- model_set %>% select(-price)
y <- model_set$price
glmnet_mlr_best <- glmnet(x = X,
                          y = y,
                          alpha = 0,
                          lambda = best_lambda,
                          family = "gaussian")

coeffs <- as.data.frame(as.matrix(coefficients(glmnet_mlr_best)))
print(coeffs)

# play a bit with cv and check how it affects best lambda that is chosen
set.seed(50)
glmnet_mlr_cv <- cv.glmnet(x = as.matrix(X), 
                           y = y,
                           alpha = 0,
                           nfolds = 3,
                           family = "gaussian")
plot(glmnet_mlr_cv)
glmnet_mlr_cv$lambda.min
glmnet_mlr_cv$lambda.1se
# conclusion: apparently lambda.min doesn't change at all through the runs
# on the other side, there is a change in lambda.1se
# what is the difference between those two?

# train the model and take the best lambda that is found by CV
glmnet_mlr_best <- glmnet(x = X, 
                          y = y,
                          alpha = 0,
                          lambda = glmnet_mlr_cv$lambda.min,
                          family = "gaussian")
coeffs <- as.data.frame(as.matrix(coefficients(glmnet_mlr_best)))
print(coeffs)

predictions <- predict(glmnet_mlr_cv, 
                       newx = as.matrix(X), 
                       s = glmnet_mlr_cv$lambda.min)
r2 <- as.numeric(cor(predictions, y)^2)
print(r2)

# train the LASSO regularized linear model
glmnet_mlr <- glmnet(x = X, 
                     # y = log(y),
                     y = y,
                     alpha = 1, 
                     family = "gaussian")
summary(glmnet_mlr)
plot(glmnet_mlr, xvar='dev', label = TRUE)
plot(glmnet_mlr, xvar='norm', label = TRUE)
plot(glmnet_mlr, xvar='lambda', label = TRUE)

best_lambda_index <- which.min(deviance(glmnet_mlr))
best_lambda <- glmnet_mlr$lambda[best_lambda_index]

# get coeffients for best lambda Lasso reg. lin. model
glmnet_mlr <- glmnet(x = X, 
                     # y = log(y),
                     y = y,
                     lambda = best_lambda,
                     alpha = 1, 
                     family = "gaussian")

coeffs <- as.data.frame(as.matrix(coefficients(glmnet_mlr)))
print(coeffs)

# get best lambda from CV 
glmnet_mlr_cv <- cv.glmnet(x = as.matrix(X), 
                           y = y,
                           alpha = 1,
                           nfolds = 10,
                           family = "gaussian")
plot(glmnet_mlr_cv)
glmnet_mlr_cv$lambda.min
glmnet_mlr_cv$lambda.1se

# train the model and take the best lambda that is found by CV
glmnet_mlr_best <- glmnet(x = X, 
                          y = y,
                          alpha = 1,
                          lambda = glmnet_mlr_cv$lambda.min,
                          family = "gaussian")
coeffs <- as.data.frame(as.matrix(coefficients(glmnet_mlr_best)))
print(coeffs)

predictions <- predict(glmnet_mlr_cv, 
                       newx = as.matrix(X), 
                       s = glmnet_mlr_cv$lambda.min)
r2 <- as.numeric(cor(predictions, y)^2)
print(r2)

# conclusion: LASSO increased prediction accuracy from ~0.616 to ~0.619
# grade is the most influential on price prediction
# lets plot it in regards to price and see what is happening
ggplot(data=data.df, mapping=aes(x=grade, y=price)) +
   geom_point()
# how about price logged, since there was a clue that 
# there is exponential relation, not strictly linear
ggplot(data=data.df, mapping=aes(x=grade, y=log(price))) +
  geom_point()
# interesting. maybe try lasso with predicted variable logged?

# get best lambda from CV 
glmnet_mlr_cv <- cv.glmnet(x = as.matrix(X), 
                           y = log(y),
                           alpha = 1,
                           nfolds = 10,
                           family = "gaussian")
plot(glmnet_mlr_cv)
glmnet_mlr_cv$lambda.min
glmnet_mlr_cv$lambda.1se

# train the model and take the best lambda that is found by CV
glmnet_mlr_best <- glmnet(x = X, 
                          y = log(y),
                          alpha = 1,
                          lambda = glmnet_mlr_cv$lambda.min,
                          family = "gaussian")
coeffs <- as.data.frame(as.matrix(coefficients(glmnet_mlr_best)))
print(coeffs)

predictions <- predict(glmnet_mlr_cv, 
                       newx = as.matrix(X), 
                       s = glmnet_mlr_cv$lambda.min)

r2 <- as.numeric(cor(exp(predictions), y)^2)
print(r2)

# here we have 0.62 accuracy, which is even better than without logged 
# predicted variable. now, why is that?