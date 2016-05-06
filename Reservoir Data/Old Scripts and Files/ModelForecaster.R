produce_forecasts = function(model.fit)
{
  # Forecast noise
  fc = forecast(model.fit$model, h=12, level = 0.95)
  
  # Forecast seasonality
  season.fc = model.fit$seasonal.values[1:12]
  
  # Create combined seasonality and noise forecast
  y.fc = c(model.fit$resids, season.fc+fc$mean)
  
  # Differencing Inverse to return to original values
  fc.all = diffinv(y.fc, difference=1, xi =  x[1])
  
  return(fc.all)
}