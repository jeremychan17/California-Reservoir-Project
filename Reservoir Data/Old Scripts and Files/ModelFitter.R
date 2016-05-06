# Function Writing

fit.model = function(waterObject)
{
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
  fit.y = auto.arima(resid, allowmean = F, trace = F, stepwise = F, d = 1) # Do I need this d?
  # wn = resid(fit.y)
  
  # Create noise forecast
  fc = forecast(fit.y, h=12, level = 0.95)
  
  # Forecast seasonality
  season.fc = fit.back$fitted.values[1:12]
  
  # Create combined seasonality and noise forecast
  y.fc = c(y, season.fc+fc$mean)
  
  # Differencing Inverse to return to original values
  fc.all = diffinv(y.fc, difference=1, xi =  x[1])
  
  
  #returnList = list(model = fit.y, seasonal.values = fit.back$fitted.values[1:12])
  returnList = list(model = fit.y, forecast = fc.all, resids = y, seasonal.values = fit.back$fitted.values[1:12])
  return (returnList)
}