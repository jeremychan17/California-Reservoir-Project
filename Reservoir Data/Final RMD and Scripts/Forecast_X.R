# The purpose of this function is to create a twelve month 95% confidence forecast for a given 
# California reservoir in a given year. 

# Expected Input: The function takes a "reservoir" object. The object is created using the
# R package "sharpshootR." See the associated documentation for that package for more information
# on retrieving the necessary information. A sample call to the necessary function as well
# as a call for this function is included below.

# Sample Input: 
# temp = CDECquery(x, 15, interval = "M", "1900-01-01", Sys.Date())
# forcast.all(temp)

# Expected Output: forecast.all has several outputs. The first output is three plots: The first 
# plot is a histogram of the time series model residuals, used to check for normality of the
# residuals. The second plot is the entire time series plus the forecast and prediction interval.
# The forecast and prediction interval are all in red lines, with the prediction interval denoted
# by a dotted red line. The third plot is a zoomed-in plot of the prediction, including the 
# year being predicted and the previous year for reference. Again, the point forecast is indicated
# by a red line, but this time each month is denoted by a circle. The prediction interval 
# remains as a dotted red line. Additionally, the function returns several objects shown below:
# $model returns the time series model which was fitted to the data
# $forecast.list returns the point forecast as well as the upper and lower 95% prediction interval
# $point.forecast returns only the point forecast
# $upper.forecast returns only the upper 95% prediction interval bound
# $lower.forecast returns only the lower 95% prediction interval bound
# $residuals returns the residuals from the fitted time series 

forecast.201X = function(waterObject, year)
{
  # Finds first january and last december
  firstjan = grep("January", waterObject$month)[1]
  lastdec = grep("December", waterObject$month)[length(grep("December", waterObject$month))]
  
  # Pulls dates
  first_jan = substr(strptime(waterObject[firstjan,]$datetime, format = "%F"), 0, 10)
  last_dec = substr(strptime(waterObject[lastdec,]$datetime, format = "%F"), 0, 10)
  
  # Creates Ranges
  start_range = c(as.numeric(substr(first_jan, 0, 4)), 1)
  end_range = c(as.numeric(year-1), 12)
  
  # Creates Time Series
  year_series = ts(waterObject$cap[firstjan:lastdec], start = start_range, end = end_range, frequency = 12)
  
  # Set up variables
  x = as.vector(year_series)
  n = length(data)
  t = 1:n
  
  # Impute any missing values
  y = sapply(1:length(x), function(i){
    if(is.na(x[i])==T)
    {
      if(i == 2)
      {
        x[i] = (x[i-1]+x[i+1])/2
      }
      else {x[i] = (x[i-2]+x[i-1]+x[i+1]+x[i+2])/4}
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
  if(Box.test(wn, type = "Ljung-Box", lag = 24)$p.value < .05){
    print("Warning: Residuals are not independent.")
  }
  
  # Test for residual normality
  if(shapiro.test(wn)$p.value < .05){
    print("Warning: Residuals are not normal by Shapiro-Wilk Test.")
  }
  
  ## Forecast ##
  
  # Forecast noise
  fc = forecast(fit.y, h=12, level = 0.95)
  
  # Forecast seasonality
  season.fc = fit.back$fitted.values[1:12]
  
  # Create combined seasonality and noise forecast
  y.fc = c(y, season.fc+fc$mean)
  bound = (fc$upper - fc$lower)/2
  
  # Differencing Inverse to return to original values
  fc.all = diffinv(y.fc, difference=1, xi =  x[1])
  fc.upper = fc.all[(length(x)+1):(length(x)+12)]+bound
  fc.lower = fc.all[(length(x)+1):(length(x)+12)]-bound
  
  forecast.list = list("point.forecast" = fc.all[(length(x)+1):(length(x)+12)], 
                       "upper.forecast" = fc.upper, "lower.forecast" = fc.lower)
  
  # Objects to return
  model = fit.y
  point.forecast = forecast.list$point.forecast
  upper.forecast = forecast.list$upper.forecast
  lower.forecast = forecast.list$lower.forecast
  
  # Finds first january and last december
  lastobs = length(waterObject$cap)
  
  # Pulls dates
  firstobs = substr(strptime(waterObject[1,]$datetime, format = "%F"), 0, 10)
  lastobs = substr(strptime(waterObject[length(waterObject$cap),]$datetime, format = "%F"), 0, 10)
  
  # Creates Ranges
  full_range_start = c(as.numeric(substr(firstobs, 0, 4)), as.numeric(substr(firstobs, 6,7)))
  full_range_end = c(year, 12)
  
  forecast_start = c(year, 1)
  forecast_end = c(year, 12)
  
  point_joined = c(year_series[length(year_series)], point.forecast)
  upper_joined = c(year_series[length(year_series)], upper.forecast)
  lower_joined = c(year_series[length(year_series)], lower.forecast)
  
  # Creates Time Series
  full_series = ts(waterObject$cap, start = full_range_start, end = full_range_end, frequency = 12)
  point_series = ts(point_joined, start = end_range, end = forecast_end, frequency = 12)
  upper_series = ts(upper_joined, start = end_range, end = forecast_end, frequency = 12)
  lower_series = ts(lower_joined, start = end_range, end = forecast_end, frequency = 12)
  
  # Create Plots
  
  # Plot Raw Data
  ID = waterObject[1,]$ID
  index_2015 = grep(2015, waterObject$year)[1]
  raw_data = ts(waterObject$cap[0:index_2015], start = full_range_start, end = full_range_end, frequency = 12)
  plot(raw_data, type = "l", main = paste(ID, "Capacity Levels (Raw)"), ylab = 
         "Capacity (Percentage)", ylim = c(0, 100), xlab = "Year")
  
  # Plot Cleaned Data
  plot(full_series, type = "l", main = paste(ID, "Capacity Levels (Cleaned)"), ylab = 
         "Capacity (Percentage)", ylim = c(0, 100), xlab = "Year")
  
  # Residuals Plot
  hist(wn, main = paste("Residuals of 2014", ID, "Model"))
  
  # Forecast Plot
  
  # Zoomed out
  plot(full_series, type = "l", main = 
         paste("2014 Forecast of", ID, "\n Reservoir Capacity"), ylab = 
         "Capacity (Percentage)", ylim = c(0, 100), xlab = "Year")
  lines(point_series, col = "red", type = "l")
  lines(upper_series, col = "red", lty = 2)
  lines(lower_series, col = "red", lty = 2)
  
  # Zoomed In
  # year = (as.numeric(substr(lastobs, 0, 4)))
  plot(full_series, xlim = c(year-1, year+1), type = "o", main = 
         paste(year, "Forecast of", ID, "\n Reservoir Capacity"), ylab = 
         "Capacity (Percentage)", ylim = c(0, 100), xlab = "Year")
  lines(point_series, col = "red", type = "o")
  lines(upper_series, col = "red", lty = 2)
  lines(lower_series, col = "red", lty = 2)
  
  # Objects to return
  model = fit.y
  point.forecast = forecast.list$point.forecast
  upper.forecast = forecast.list$upper.forecast
  lower.forecast = forecast.list$lower.forecast
  
  # Return Call
  returnList = list(model = fit.y, forecast.list = forecast.list,
                    point.forecast = point.forecast, upper.forecast = upper.forecast, 
                    lower.forecast = lower.forecast, residuals = wn) 
}