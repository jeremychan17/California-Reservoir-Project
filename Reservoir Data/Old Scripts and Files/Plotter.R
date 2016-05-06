forecastplot = function(waterObject)
{
  model = fit.model(waterObject)
  forecast = produce_forecasts(model)
  data = waterObject$capacity
  fore_2016 = forecast[((length(forecast))-12):length(forecast)]
  
  # Finds first january and last december
  lastobs = length(waterObject$capacity)
  
  # Pulls dates
  d1 = substr(strptime(waterObject[1,]$datetime, format = "%F"), 0, 10)
  d2 = substr(strptime(waterObject[length(waterObject$cap),]$datetime, format = "%F"), 0, 10)
  
  # Creates Ranges
  start_range = c(as.numeric(substr(d1, 0, 4)), as.numeric(substr(d1, 6,7)))
  end_range = c(as.numeric(substr(d2, 0, 4)), as.numeric(substr(d2, 6,7)))
  forecast_start = length(temp_series) - as.numeric(substr(d2, 6, 7))
  
  # Creates Time Series
  temp_series = ts(waterObject$cap, start = start_range, end = end_range, frequency = 12)
  
  # Plots
  plot(1:length(temp_series), temp_series, col= "dark grey", main = waterObject, type = "o", ylim = c(0, 100), 
       xlim = c(length(temp_series)-20, length(temp_series)+10))
  lines(forecast_start:(forecast_start+13), c(temp_series[length(temp_series)-end_range[2]], fore_2016), type = "o", col = "red")
}

