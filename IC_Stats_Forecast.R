################

library(janitor)
library(DataExplorer)
library(correlationfunnel)
library(tidyverse)
library(psych)
library(parsnip)
library(tidymodels)

dat1 <- read.csv("dataForStats.csv") %>% clean_names()

attach(dat1)

introduce(dat1)

plot_intro(dat1)

colSums(is.na(dat1))

dat1$x <- NULL 
dat1$x0 <- NULL 

describeBy(dat1)

plot_bar(dat1)

boxplot(dat1$gp, dat1$actual_cost)

plot(dat1$actual_cost, dat1$gp)

dat1$date_closed <- as.Date(dat1$date_closed)

plot_correlation(na.omit(dat1))

#Correlation Plot

dat_cor <- subset(dat1, select = -c(date_closed))

dat_cor <- na.omit(dat_cor) %>% binarize(n_bins = 5, thresh_infreq = 0.01, name_infreq = "OTHER", one_hot = TRUE)
dat_cor

dat_cor_plot <- dat_cor %>% correlate(dat_cor$estimator__Trevor_Abbott) 

corrplot(dat_cor)

#Regression
set.seed(123)

dat_split <- initial_split(dat1, prop = .80)

# Build training data set
dat_training <- dat_split %>% 
  training()

# Build testing data set
dat_test <- dat_split %>% 
  testing()


lm_model <- linear_reg() %>% 
  set_engine('lm') %>% # adds lm implementation of linear regression
  set_mode('regression')

lm_fit <- lm_model %>% 
  fit(dat1$total_collected ~ dat1$job_code, data = dat_training)
lm_fit

summary(lm_fit$fit)

par(mfrow=c(2,2)) # plot all 4 plots in one

plot(lm_fit$fit, 
     pch = 16,    # optional parameters to make points blue
     col = '#006EA1')

# Data frame of estimated coefficients
tidy(lm_fit)

# Performance metrics on training data
glance(lm_fit)

vip(lm_fit)
