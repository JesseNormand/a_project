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

#We will use post method to send JSON data 

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
#* @serializer htmlwidget
#* @get /ggplot
function() {
  plot(model)
}

