##########load###########
library(forecast)

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

plot(dat1[,1:2], xlab = "year", ylab = "revenue", type = "l",  lwd = 3, col = "blue")

plot(dat1[,3:2], xlab = "year", ylab = "revenue",  type = "l", lwd = 3, col = "blue", 
     main = " IC Chart", 
     xlim = c())

#Time series
time_s <- ts(dat1, frequency= 12 , start = 2021, end = 2023)

hw(dat1, seasonal = "additive")

plot(time_s)


time_fit <- HoltWinters(time_s)

accuracy(time_fit)

forecast(
  time_fit$x, 5
)
forecast( time_fit$x, h = 5, level = c(80, 95), fan = FALSE, 
          lambda = NULL, biasadj = NULL)
