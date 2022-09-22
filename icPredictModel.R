##############

library(tidyverse)
library(caret)
library(janitor)
library(tidymodels)

dat1 <- read.csv("clean.csv") %>% clean_names()

attach(dat1)

dat1$gross_margin <- NULL

#Split data
set.seed(123)

dat_split <- initial_split(dat1, prop = .80)

# Build training data set
dat_training <- dat_split %>% 
  training()

# Build testing data set
dat_test <- dat_split %>% 
  testing()

#Set cross validation for rf model

tc <- trainControl(method = "cv", number = 5)

#Train model


lm1 <- train(total_collected ~., data = dat_test, method = "glm")

rf1 <- train(total_collected ~., data = dat_test, method = "rf", trControl = tc)

xgb <- train(total_collected ~., data = dat_test, method = "XgbLinear", trControl = tc)


#Predict

pred <- predict(lm1, dat_test[-3])






#Chart

ggplot(dat_test, aes(x = pred, y = total_collected)) +
  geom_point() +
  geom_abline(color = "darkblue")


par(mfrow=c(2,2)) # plot all 4 plots in one

plot(lm1$finalModel, 
     pch = 16,    # optional parameters to make points blue
     col = '#006EA1')

roc_score <- roc(dat_test[,3], pred)

plot(roc_score)


