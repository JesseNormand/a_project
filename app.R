######################################
#Create API
#Using IC time series r script I wrote
######################################

library(forecast)
library(janitor)
library(fpp3)
library(plumber)
library(yaml)
library(jsonlite)
library(ggplot2)
library(readr)
library(rapidoc)

#We will use post method to send JSON data #

##########################################
#Load Model
#########################################
model <- readRDS(("IC_arima_model.rds"))

#*Test Connection
#* @get /connection-status

function(){
  list(status = "Connection Successful",
       time = Sys.time(),
       username = Sys.getenv("USERNAME"))
}

#* Run model
#* @serializer png 
#* @get /ggplot
#*  
function() {
  mod <- forecast(model)
  print(autoplot(mod, ylab = "Revenue") +
    scale_y_continuous(labels=scales::dollar_format()) +
    labs(title = "24 Month Forecast of Revenue per Month"))
}

