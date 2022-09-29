##########load###########
library(forecast)
library(janitor)

dat1 <- read.csv("clean_forcast.csv", header = TRUE) %>% clean_names()

attach(dat1)

options(scipen=999)

#Data formatting

dat1$date <- ymd(dat1$date)

x <- as.Date(dat1$date)

year <- as.numeric(format(x, '%Y'))

month <- as.numeric(format(x, '%m'))

day <- as.numeric(format(x, '%d'))

dat1 <- cbind(dat1, year, month, day)

#zoom in on date

dat1_zoom <- subset(dat1, subset = dat1$date >= '2019-01-01' & dat1$date <= '2019-12-31')

#Plot data
plot(dat1$year, dat1$revenue, type = "l")


plot(dat1[,2], type = "l")

plot(dat1[,3:2], xlab = "year", ylab = "revenue",
     col = "red",
     main = "Limit Chart",
     type = "l", 
     lwd=2, 
     xlim = c(2021,2022),
     ylim = c(0,12000))

par(mfrow =c(2,1))


#Chart with min and max
xmin <- min(dat1[,2])
xmax <- max(dat1[,2])

plot(dat1[,1:2], xlab = "year", ylab = "revenue",
     type = "l",  
     lwd = 3,
     col = "blue")
     grid()
     abline(h=c(15000,30000), col = "red", lwd = 2)
     abline(v=seq(xmin, xmax, 26), col = "black", lwd = 3)



plot(dat1[,3:2], xlab = "year", ylab = "revenue",  type = "l", lwd = 3, col = "blue", 
     main = " IC Chart", 
     xlim = c())

#boxplot
boxplot(dat1$revenue ~ dat1$year, las = 1)
grid()

boxplot(dat1$revenue ~ dat1$month,
        las = 1,
        col ="blue")
        grid()
  
boxplot(dat1$revenue ~ dat1$day,
                las = 1,
                col ="green")
        grid()
        
#View where NAs are
colSums(is.na(dat1))        
        
view_na <- dat1[which(is.na(dat1$revenue))]
view_na

#fill data with previous cell data
library(tidyr)
fill(dat1$revenue)

#Trend compute rolling average:
library(zoo)
library(dplyr)

roll_m <- dat1 %>% 
  arrange(desc(date)) %>% 
  group_by(year) %>% 
  mutate(day7 = rollmean(revenue, k = 7, fill = NA),
                                  rollavg_365 = rollmean(revenue, k = 365, fill = NA)) %>% ungroup()
#Filter rolling average
roll_m

roll_m %>% arrange(year) %>% 
  filter(year == 2021) %>% 
  select(date,revenue) %>% 
  head(10)

roll_2021 <- roll_m %>% arrange(year) %>% 
  filter(year == 2021) %>% 
  select(date,revenue) %>% 
  head(10)

#plot rolling average
roll_mean <- mean(na.omit(roll_m$day7))

plot(roll_m$date, roll_m$rollavg_365,  xlab = "year", ylab = "revenue",  type = "l", lwd = 3, col = "blue", 
     main = " 7 Day Rolling Average", 
     xlim = c())
abline(h =c(roll_mean), lwd = 2, col = "darkred")
points(roll_m$day7, type = "l", col = "orange")
legend(2022,6000,legend = c("1"))
grid()



#Time series############################################
library(fpp3)

dat1 <- read.csv("clean_forcast.csv", header = TRUE) %>% clean_names()

attach(dat1)

dat1$month <- NULL

dat1$date <- ymd(dat1$date)

#declare time series

time_s <- ts(dat1$revenue, start = c(2019), end = c(2022,9), frequency = 12)

########################################################

#time plot
autoplot(time_s)


#trend is stationary so we dont need to use diff  
dat_diff <- diff(time_s)

plot(dat_diff)

#check seasonality
ggseasonplot(time_s)

ggseasonplot(dat_diff)

ggsubseriesplot(time_s)

ggsubseriesplot(dat_diff)

#Forecast#######################
#Benchmark: seasonal naive method y_t = y-ts + e

fit_nai <- snaive(time_s) #resid = $172782.8152
print(summary(fit_nai))
checkresiduals(fit_nai)

fit_nai_diff <- snaive(dat_diff) #resid = $183066.1944 
print(summary(fit_nai_diff))
checkresiduals(fit_nai_diff)


#Fit Exponential model
fit_exp_times <- ets(time_s) # sigma(same as residuals) $111667.9
print(fit_exp_times)
checkresiduals(fit_exp_times)

fit_exp <- ets(dat_diff) #resid $ 129900
print(fit_exp)
checkresiduals(fit_exp)


#Fit ARIMA model
fit_arima_times <- auto.arima(time_s) #$109361.7

#if we had trend and seasonality(d=1, D=1)
print(fit_arima_times)
checkresiduals(fit_arima_times)

fit_arima <- auto.arima(dat_diff, trace = TRUE) #resid(sqrt sigma that is squared) $167402.2
     #if we had trend and seasonality(d=1, D=1)
print(fit_arima)
checkresiduals(fit_arima)

###################################
#Forescast

for_cat_ar <- forecast(fit_arima_times, h=12)
plot(for_cat_ar, type = "l", lwd = 2, col = "black",
     main = "12 Monthy Revenue per Day Prediction", 
     las = 1)
     axis(3 , at=axTicks(2), labels = sprintf("$%s", axTicks(2)), las = 1)
abline(h = 0, lwd = 1)
grid()

for_cat_ex <- forecast(fit_exp_times, h = 12)
autoplot(for_cat_ex)

for_cat_nai <- forecast(fit_nai_diff, h = 12)
autoplot(fit_nai, ylab = "Revenue" , xlab = "Date") +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(title = "12 Month Forecast of Revenue per Month") + scale_x_date(date_labels = "%b-%d-%Y")


