forecast.all = function(waterObject)
{
  library(sharpshootR)
  library(XML)
  
  # Finds first january and last december
  firstobs = grep("January", waterObject$month)[1]
  lastobs = grep("December", waterObject$month)[length(grep("December", waterObject$month))]
  
  # Pulls dates
  d1 = substr(strptime(waterObject[firstobs,]$datetime, format = "%F"), 0, 10)
  d2 = substr(strptime(waterObject[lastobs,]$datetime, format = "%F"), 0, 10)
  
  # Creates Ranges
  start_range = c(as.numeric(substr(d1, 0, 4)), 1)
  end_range = c(as.numeric(substr(d2, 0, 4)), 12)
  
  # Creates Time Series
  temp_series = ts(waterObject$cap, start = start_range, end = end_range, frequency = 12)
  
  # Set up variables
  x = as.vector(temp_series)
  n = length(data)
  t = 1:n
  
  # Impute any missing values
  y = sapply(1:length(x), function(i){
    if(is.na(x[i])==T)
    {
      x[i] = (x[i-1]+x[i+1])/2
    }
    else x[i] = x[i]
  })
  
  # Remove trend through differencing
  y = diff(y)
  
  # Removal seasonal component
  
  # Rescale t
  n = length(t)
  t = 1:length(y)
  t = (t)/n
  
  # Matrix of harmonics
  d = 12
  n.harm = d/2
  harm = matrix(nrow=length(t), ncol=2*n.harm)
  for(i in 1:n.harm){
    harm[,i*2-1] = sin(n/d * i *2*pi*t)
    harm[,i*2] = cos(n/d * i *2*pi*t)
  }
  colnames(harm)= 
    paste0(c("sin", "cos"), rep(1:n.harm, each = 2))
  
  # Fit on all of the sines and cosines
  dat = data.frame(y, harm)
  fit = lm(y~., data=dat)
  
  # Setup the full model and the model with only an intercept
  full = lm(y~.,data=dat)
  reduced = lm(y~1, data=dat)
  
  # Stepwise regression starting with the full model
  fit.back = step(full, scope = formula(reduced), direction = "both", trace = F)
  resid = residuals(fit.back)
  
  # Get back the original t so that we can plot over this range
  t = 1:length(y)
  
  # Fitting a model
  library(forecast)
  fit.y = auto.arima(resid, allowmean = F, trace = F, stepwise = F)
  wn = resid(fit.y)
  
  # Tests for stationarity of resulting series
  library(tseries)
  if(kpss.test(wn)$p.value < .05)
  {
    print("Warning: Time Series is not stationary by kpss test")
  }
  
  if(adf.test(wn)$p.value > .05)
  {
    print("Warning: Time series is not stationary by adf test")
  }
  
  # Test for residual independence
  if(Box.test(wn)$p.value < .05){
    print("Warning: Residuals are not independent.")
  }
  
  ## BREAK ##
  
  # Forecast noise
  fc = forecast(fit.y, h=12, level = 0.95)
  
  # Forecast seasonality
  season.fc = fit.back$fitted.values[1:12]
  
  # Create combined seasonality and noise forecast
  y.fc = c(y, season.fc+fc$mean)
#   y.upper = c(season.fc+fc$upper)
#   y.lower = c(season.fc+fc$lower)
  bound = (fc$upper - fc$lower)/2
  
  # Differencing Inverse to return to original values
  fc.all = diffinv(y.fc, difference=1, xi =  x[1])
  fc.upper = fc.all[649:660]+bound
  fc.lower = fc.all[649:660]-bound
  
  #TESTING works, but need to make modular
  plot(fc.all, type = "l", col = "gray", xlim = c(640, 700))
  lines(648:660, c(fc.all[648], fc.upper), col = "red", type = "l")
  lines(648:660, c(fc.all[648], fc.lower), col = "red", type = "l")
  
  model = fit.y
  forecast = fc.all
  data = waterObject$capacity
  fore_2016 = forecast[((length(forecast))-11):length(forecast)]
  
  # Finds first january and last december
  lastobs = length(waterObject$capacity)
  
  # Pulls dates
  d1 = substr(strptime(waterObject[1,]$datetime, format = "%F"), 0, 10)
  d2 = substr(strptime(waterObject[length(waterObject$cap),]$datetime, format = "%F"), 0, 10)
  
  # Creates Ranges
  start_range = c(as.numeric(substr(d1, 0, 4)), as.numeric(substr(d1, 6,7)))
  end_range = c(as.numeric(substr(d2, 0, 4)), as.numeric(substr(d2, 6,7)))
  
  # Creates Time Series
  temp_series = ts(waterObject$cap, start = start_range, end = end_range, frequency = 12)
  
  # Forecast Starting Index
  forecast_start = length(temp_series) - as.numeric(substr(d2, 6, 7))
  
  # Plots
  ID = waterObject[1,]$ID
  
  hist(wn, main = paste("Histogram of Residuals of", ID))
  
  plot(1:length(temp_series), temp_series, main = paste("Twelve Month Forecast of", ID, " Reservoir Capacity"), 
       col= "dark grey", type = "o", xlim = c(length(temp_series)-20, length(temp_series)+10), 
       ylab = "Reservoir Capacity (Percentage)", ylim = c(0, 100))
  lines(forecast_start:(forecast_start+12), c(temp_series[length(temp_series)-end_range[2]], fore_2016), type = "o", col = "red")
  if(shapiro.test(wn)$p.value>.05)
  { 
    lines(forecast_start:(forecast_start+12), c(temp_series[length(temp_series)-end_range[2]], fore_2016+fc$upper), col="red", lty=2)
    lines(forecast_start:(forecast_start+12), c(temp_series[length(temp_series)-end_range[2]], fore_2016-fc$upper), col="red", lty=2)
  }
  else{print("Warning: Residuals are not normal, prediction interval not included.")}
 
  
  
  returnList = list(model = fit.y, forecast = fc.all, residuals = wn)
}