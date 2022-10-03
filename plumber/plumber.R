
library(forecast)
library(plumber)
library(jsonlite)
library(ggplot2)
library(readr)

options(scipen=999)

model <- readRDS(("fit_nai.rds"))



#* @get /connection-status

function(){
  list(status = "Connection Successful",
       time = Sys.time(),
       username = Sys.getenv("USERNAME"))
}


#* Run model - Print Time Series Chart
#* @serializer png
#* @get  /prediction

function(req, res){
  
  mod <-forecast::forecast(model)
  
  print(autoplot(mod)+ labs(title = "Forecasted Revenue per Month",
                            x="Year",
                            y = "Revenue") + scale_y_continuous(labels=scales::dollar_format()))
  

}

#* Pint Model Data
#* @serializer json
#* @get /print

function(){
  mod <-forecast::forecast(model)
  print(mod)
  
  
}

