
library(forecast)




#* @get /connection-status

function(){
  list(status = "Connection Successful",
       time = Sys.time(),
       username = Sys.getenv("USERNAME"))
}


model <- readRDS(("IC_arima_model2.rds"))

#* Run model
#* @serializer json
#* @get  /prediction

function(req, res){
  
  
  mod <- forecast(model)
  print(mod)
  
 
}