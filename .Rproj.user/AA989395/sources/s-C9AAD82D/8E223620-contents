library(tidyverse)
library(caret)
library(dslabs)
data(heights)


# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p=0.5, list = FALSE) # used to generate indexes for randomly splitting data
# t = how many random samples of indexes
# p =proportion of the index represented
# list = whether index returned is in a list
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# simplest ML algo: guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
#males generally taller
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
#predict male if height is 2 sd from the avg male
y_hat <- ifelse(x > 62, "Male","Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
#pick best from training set not test set which is only for evaluation
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){   #applies function to each element in th list "cutoff" returning an object of the same length
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
#make plot
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# test best cutoff on test set to not cause overfitting
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#assesment
mnist <- read_mnist()
labels(mnist)
labels(mnist$train)

#### CONFUSTION MATRIX ####
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
nrow(mnist$train$images)

#### confusion Matrix ####
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)

test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))
# most subjects are male
prev <- mean(y == "Male")

# confustion matrix
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(data = y_hat, reference = test_set$sex)

# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>%
  ggplot(aes(cutoff, F_1)) +
  geom_point() +
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

#### ROC and precision-recall curves ####
#guessing male 90% of the time gives high probability due to the bias in the sample
#gives low sensitivity
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

#adding cutoff values to point
library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall to account for prevalence
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


#### assessment ####
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

str(dat)
mytable <- table(sex = dat$sex, type = dat$type)
proptable <- prop.table(mytable,2) # column percentages

#2
y_hat <- ifelse(dat$type == "inclass", "Female","Male") %>%
  factor(levels = c("Female", "Male"))
mean(y_hat == y)

table(y_hat,y)
sensitivity(y_hat, reference = y)
specificity(y_hat, reference = y)
prevalence <- mean(y == "Female")
cat(prevalence)

#part 2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y, times = 1, p =0.5, list = F)
test <- iris[test_index,]
train <- iris[-test_index,]

cutoffSL <- seq(min(train$Sepal.Length), max(train$Sepal.Length), .1)
cutoffSW <- seq(min(train$Sepal.Width), max(train$Sepal.Width), .1)
cutoffPL <- seq(min(train$Petal.Length), max(train$Petal.Length), .1)
cutoffPW <- seq(min(train$Petal.Width), max(train$Petal.Width), .1)

levels(train$Species)
cutoffavgSL <- map_dbl(cutoffSL,function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})


cutoffavgSW <- map_dbl(cutoffSW,function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})


cutoffavgPL <- map_dbl(cutoffPL,function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})


cutoffavgPW <- map_dbl(cutoffPW,function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(SL = max(cutoffavgSL),SW = max(cutoffavgSW),PL = max(cutoffavgPL), PW = max(cutoffavgPW))

smartcutoff <- cutoffPL[which.max(cutoffavgPL)]
y_hat <- ifelse(test$Petal.Length > smartcutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)
#10
tcutoffavgSL <- map_dbl(cutoffSL,function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})


tcutoffavgSW <- map_dbl(cutoffSW,function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})


tcutoffavgPL <- map_dbl(cutoffPL,function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})


tcutoffavgPW <- map_dbl(cutoffPW,function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})
data.frame(SL = max(tcutoffavgSL),SW = max(tcutoffavgSW),PL = max(tcutoffavgPL), PW = max(tcutoffavgPW))
#11
plot(iris,pch=21,bg=iris$Species)
smartcutPL <- cutoffPL[which.max(cutoffavgPL)]
smartcutPW <- cutoffPW[which.max(cutoffavgPW)]
y_hat <- ifelse(test$Petal.Length > smartcutPL | test$Petal.Width > smartcutPW, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

# ASSESSMENT 2.2
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
#test for no disease
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
#test for disease
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

pPositive <- (sum(test[disease==0]==1) + sum(test[disease==1]==1)) / length(disease)
pNegative <- (sum(test[disease==0]==0) + sum(test[disease==1]==0)) / length(disease)
pDisease <- sum(disease == 1)/length(disease)
pHealthy <- 1 - pDisease
pNgivenD <- sum(test[disease==1]==0) / length(test[disease==1])
pPgivenD <- sum(test[disease==1]==1) / length(test[disease==1])
pDgivenN <- pNgivenD*pDisease/pNegative
pDgivenP <- pPgivenD*pDisease/pPositive
pDgivenP/pDisease

#Q6
#pr(male|height = x)
library(dslabs)
data("heights")
heights %>%
  mutate(heightr = round(height)) %>%
  group_by(heightr) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(heightr, p, data =.)

#Q7
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height,ps), include.lowest = TRUE)) %>% #assure each group has the same number of points
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#Q8
#generate data from a bivariate normal distrubution using the MASS package
Sigma <- 9*matrix(c(1,0.5,0.5,1),2 , 2)
dat <- MASS::mvrnorm(n = 10000, c(69,69), Sigma = Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x,quantile(x,ps),include.lowest = TRUE))%>%
  group_by(g) %>%
  summarise(x = mean(x),y=mean(y)) %>%
  qplot(x, y, data =.)
  