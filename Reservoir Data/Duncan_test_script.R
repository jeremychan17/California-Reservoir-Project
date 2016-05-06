# Sample Time Series Graph Code

# Loads library for retrieving data
library(sharpshootR)
waterObject = CDECquery("SHA", 15, interval = "M", "1900-01-01", Sys.Date()) # Pulls data
waterObject$cap = waterObject$value/4552000 * 100 # Creates Capacity vector

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
# Forecast Starting Index
forecast_start = length(temp_series) - as.numeric(substr(d2, 6, 7))

ID = waterObject[1,]$ID

# I think the problem which doesn't properly apply the x labels is in the 1:length(temp_series) argument. However, 
# I use that because otherwise, my next line of code adds the prediction using "lines." Without that argument, the "line" 
# function call does not work. 
plot(1:length(temp_series), temp_series, main = paste("Twelve Month Forecast of ", ID, " Reservoir Capacity"), 
     col= "dark grey", type = "o", xlim = c(650, 750), 
     ylab = "Reservoir Capacity (Percentage)", ylim = c(0, 100))
temp = rep(50, 12)
lines(forecast_start:(forecast_start+12), c(temp_series[length(temp_series)-end_range[2]], temp), type = "o", col = "red")

