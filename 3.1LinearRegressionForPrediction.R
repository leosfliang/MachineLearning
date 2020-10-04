library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#our prediction would be to use the average as a guess
avg <- mean(train_set$son)
avg

#squared loss is high
mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coefficients

y_hat <- fit$coefficients[1] + fit$coefficients[2]*test_set$father
#squared loss is lower
mean((y_hat - test_set$son)^2)

## THE Predict Func####
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# read help files
?predict.lm
?predict.glm

### Comprehension Check: Linear Regression ###
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
RMSE <- replicate(n, {test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
                  train_set <- dat %>% slice(-test_index)
                  test_set <- dat %>% slice(test_index)
                  fit <- lm(y~x,data = train_set)
                  y_hat <- predict(fit, test_set)
                  sqrt(mean((y_hat-test_set$y)^2))
                  })
mean(RMSE)
sd(RMSE)

set.seed(1, sample.kind="Rounding")
nRMSE <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x,data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  })
  c(n,mean(rmse), sd(rmse))
}
set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
sapply(n,nRMSE)

#Q3
# making corr between x y larger
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
RMSE <- replicate(n, {test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y~x,data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat-test_set$y)^2))
})
mean(RMSE)
sd(RMSE)

#Q6
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding")
RMSE <- replicate(1, {test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fitx1 <- lm(y~x_1,data = train_set)
y_hatx1 <- predict(fitx1, test_set)
fitx2 <- lm(y~x_2,data = train_set)
y_hatx2 <- predict(fitx2, test_set)
fitx1x2 <- lm(y~x_1+x_2,data = train_set)
y_hatx1x2 <- predict(fitx1x2, test_set)
c(sqrt(mean((y_hatx1-test_set$y)^2)),
sqrt(mean((y_hatx2-test_set$y)^2)),
sqrt(mean((y_hatx1x2-test_set$y)^2)))
})
RMSE

#Q8
#x_1 x_2 highly correlated
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding")
RMSE <- replicate(1, {test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fitx1 <- lm(y~x_1,data = train_set)
y_hatx1 <- predict(fitx1, test_set)
fitx2 <- lm(y~x_2,data = train_set)
y_hatx2 <- predict(fitx2, test_set)
fitx1x2 <- lm(y~x_1+x_2,data = train_set)
y_hatx1x2 <- predict(fitx1x2, test_set)
c(sqrt(mean((y_hatx1-test_set$y)^2)),
  sqrt(mean((y_hatx2-test_set$y)^2)),
  sqrt(mean((y_hatx1x2-test_set$y)^2)))
})
RMSE


#### Regression for a Categorical Outcome ####
library(dslabs)
library(caret)
library(dplyr)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#simple estimation
train_set %>%
  filter(round(height) == 66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x,prop)) +
  geom_point()

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)  #conditional probability
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]


### logistic Regression ###
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

tmp <- heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female"))
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>%
  ggplot(aes(x,prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

#better prediction
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]


### Case Study: 2 or 7 ###
mnist <- read_mnist()
#smallest and largest of x_1 which is the prop of black in upper left quadrant
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
#smallest and largest of x_2 which is the prop of black in lower right quadrant
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

#using logistic regression
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()
#boundry that divides the values of x1 and x2 is non linear in the true form
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

# logit regress cant capture this nonlinear nature
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

#logit will miss several points that cant be captured by this shape as it forces out estimates to be a plane and boundry to be a line
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

#### Assesment ####
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat$train %>% ggplot(aes(x, color = y)) + geom_density()
dat <- make_data()

set.seed(1, sample.kind="Rounding") 
do <- function(mu_1) {
  dat = make_data(mu_1 = mu_1)
  glm_fit <- dat$train %>% glm(y~x,family = "binomial", data = . ) 
  p_hat <- predict(glm_fit, newdata = dat$test)
  y_hat <- ifelse(p_hat > 0.5,1,0)
  confusionMatrix(data = factor(y_hat), reference = factor(dat$test$y))$overall["Accuracy"]
}
set.seed(1, sample.kind="Rounding") 
mu_1 <- seq(0,3, len=25)
accuracy <- sapply(mu_1, do, simplify = "array")
mu_acc <- data.frame(mu_1, res = accuracy)
mu_acc %>% ggplot(aes(mu_1,res))+
  geom_point()
