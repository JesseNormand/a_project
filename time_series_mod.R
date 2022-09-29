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
