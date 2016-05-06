library(sharpshootR)
library(ggplot2)
library(XML)

# List of major reservoirs
# Trinity Lake | CLE
# Lake Shasta | SHA
# Lake Oroville | ORO
# New Melones | NML
# Folsom Lake | FOL
# Don Pedro | DNP
# Exchequer | EXC
# San Luis | SNL
# Millerton Lake | MIL
# Pine Flat | PNF
# Castaic Lake | CAS
# Lake Perris | PRR

#pulling data

# Pulls Reservoir Information

# List of reservoirs of interest, daily intervals
resList = c("CLE", "SHA", "ORO", "NML", "FOL", "DNP", "EXC", "SNL", "MIL", "PNF", "CAS", "PRR")
resData = lapply(resList, function(x) CDECquery(x, 15, interval = "D", "1900-01-01", "2015-11-17"))
resData = lapply(resList, function(x) CDECquery(x, 15, interval = "D", "1900-01-01", "2015-11-17"))

# Capacity table
u = "http://cdec.water.ca.gov/misc/resinfo.html" # Reservoir URL
table = readHTMLTable(u) # Pulls Reservoir Table 
table = table[[1]] # Takes first Column
names(table) = c("ID", "Dam", "Lake", "Stream", "Capacity") # Renames Columns of Table
capacityList = table$Capacity # Pulls List of all Capacities
positions = sapply(1:length(resList), function(x) grep(resList[x], table$ID)) # Obtains positions of all reservoirs of interest
finalCapList = capacityList[c(positions)] # List of capacities from reservoirs of interest
finalCapList = as.numeric(gsub(",", "", finalCapList)) # Turns list into numerics

# Adds Capacity and computes capacity for each reservoir
for(i in 1:12)
{
  resData[[i]]$ID = resList[[i]] #adds ID Column
  resData[[i]]$Capacity = (resData[[i]]$value/finalCapList[[i]])*100 #Computes capacity %
  # resData[[i]]$day = substr(resData[[i]]$datetime, 6, 10) #column of just month/day
  # resData[[i]]$PercentHistMean = dayDiff(resData[[i]])
}

# Plots
plot(resData[[1]]$datetime, resData[[1]]$cap, type = "l", main = "Trinity Lake (CLE)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[1]]$datetime, resData[[1]]$PercentHistMean, type = "l", 
     main = "Trinity Lake (CLE)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[1]]$Capacity ~ resData[[1]]$day)
plot(resData[[1]]$Capacity ~ unique(resData[[1]]$year), type = "l")
plot(split(resData$Capacity, resData$day))

plot(resData[[2]]$datetime, resData[[2]]$cap, type = "l", main = "Lake Shasta (SHA)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[2]]$datetime, resData[[2]]$PercentHistMean, type = "l", 
     main = "Lake Shasta (SHA)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[2]]$Capacity ~ resData[[2]]$day)


plot(resData[[3]]$datetime, resData[[3]]$cap, type = "l", main = "Lake Oroville (ORO)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[3]]$datetime, resData[[3]]$PercentHistMean, type = "l", 
     main = "Lake Oroville (ORO)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[3]]$Capacity ~ resData[[3]]$day)


plot(resData[[4]]$datetime, resData[[4]]$cap, type = "l", main = "New Melones (NML)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[4]]$datetime, resData[[4]]$PercentHistMean, type = "l", 
     main = "New Melones (NML)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[4]]$Capacity ~ resData[[4]]$day)


plot(resData[[5]]$datetime, resData[[5]]$cap, type = "l", main = "Folsom Lake (FOL)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[5]]$datetime, resData[[5]]$PercentHistMean, type = "l", 
     main = "Folsom Lake (FOL)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[5]]$Capacity ~ resData[[5]]$day)


plot(resData[[6]]$datetime, resData[[6]]$cap, type = "l", main = "Don Pedro (DNP)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[6]]$datetime, resData[[6]]$PercentHistMean, type = "l", 
     main = "Don Pedro (DNP)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[6]]$Capacity ~ resData[[6]]$day)


# plot(resData[[7]]$datetime, resData[[7]]$Capacity, type = "l", main = "Exchequer (EXC)", 
#      ylab = "Capacity (Percentage)", xlab = "Date (Year)")
# plot(resData[[7]]$datetime, resData[[7]]$PercentHistMean, type = "l", 
#      main = "Exchequer (EXC)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
# boxplot(resData[[7]]$Capacity ~ resData[[7]]$day)

plot(resData[[7]]$datetime, resData[[7]]$cap, type = "l", main = "San Luis (SNL)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[7]]$datetime, resData[[7]]$PercentHistMean, type = "l", 
     main = "San Luis (SNL)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[7]]$Capacity ~ resData[[7]]$day)


plot(resData[[8]]$datetime, resData[[8]]$cap, type = "l", main = "Millerton Lake (MIL)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[8]]$datetime, resData[[8]]$PercentHistMean, type = "l", 
     main = "Millerton Lake (MIL)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[8]]$Capacity ~ resData[[8]]$day)

plot(resData[[9]]$datetime, resData[[9]]$cap, type = "l", main = "Pine Flat (PNF)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[9]]$datetime, resData[[9]]$PercentHistMean, type = "l", 
     main = "Lake Oroville (ORO)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[9]]$Capacity ~ resData[[9]]$day)

plot(resData[[10]]$datetime, resData[[10]]$cap, type = "l", main = "Castaic Lake (CAS)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[10]]$datetime, resData[[10]]$PercentHistMean, type = "l", 
     main = "Castaic Lake (CAS)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[10]]$Capacity ~ resData[[10]]$day)

plot(resData[[11]]$datetime, resData[[11]]$cap, type = "l", main = "Lake Perris (PRR)", 
     ylab = "Capacity (Percentage)", xlab = "Date (Year)")
plot(resData[[11]]$datetime, resData[[11]]$PercentHistMean, type = "l", 
     main = "Lake Perris (PRR)", ylab = "Historical Mean (Percentage)", xlab = "Date (Year)")
boxplot(resData[[11]]$Capacity ~ resData[[11]]$day)




# Investigating Trinity
summary(subset(resData[[1]], resData[[1]]$year >= 1975 & resData[[1]]$year <= 1980))

# Drop from 1976-1979 (Trinity)
plot(subset(resData[[1]], resData[[1]]$year >= 1976 & resData[[1]]$year < 1979)$datetime, 
     subset(resData[[1]], resData[[1]]$year >= 1976 & resData[[1]]$year < 1979)$Capacity, 
     type = "l", xlab = "Year", ylab = "Capacity (Percentage)")
# Corresponds with California drought


# Drop/Missing from 1985-1986 (Trinity)
plot(subset(resData[[1]], resData[[1]]$year >= 1984 & resData[[1]]$year < 1990)$datetime, 
     subset(resData[[1]], resData[[1]]$year >= 1984 & resData[[1]]$year < 1990)$Capacity, 
     type = "l", xlab = "Year", ylab = "Capacity (Percentage)")

# incorrect reporting for several instances, missing values for 1986-1988

# Sharp increase in 1993-1995 (Trinity)
plot(subset(resData[[1]], resData[[1]]$year >= 1992 & resData[[1]]$year < 1995)$datetime, 
     subset(resData[[1]], resData[[1]]$year >= 1992 & resData[[1]]$year < 1995)$Capacity, 
     type = "l", xlab = "Year", ylab = "Capacity (Percentage)")

# Rainfall 12.3 inches above 138 year average? End of drought, return to normal water levels

# Current drought 
plot(subset(resData[[1]], resData[[1]]$year >= 2009 & resData[[1]]$year < 2015)$datetime, 
     subset(resData[[1]], resData[[1]]$year >= 2009 & resData[[1]]$year < 2015)$Capacity, 
     type = "l", xlab = "Year", ylab = "Capacity (Percentage)")

plot(subset(resData[[1]], resData[[1]]$year==1985)$datetime, 
     subset(resData[[1]], resData[[1]]$year==1985)$Capacity, 
     type = "l", xlab = "Year", ylab = "Capacity (Percentage)")

# Investigating Lake Shasta



# San Luis

plot(subset(resData[[8]], resData[[8]]$year==1991)$datetime, 
     subset(resData[[8]], resData[[8]]$year==1991)$Capacity, 
     type = "l", xlab = "Year", ylab = "Capacity (Percentage)")

plot(subset(resData[[8]], resData[[8]]$year >= 1989 & resData[[8]]$year < 1990)$datetime, 
     subset(resData[[8]], resData[[8]]$year >= 1989 & resData[[8]]$year < 1990)$Capacity, 
     type = "l", xlab = "Month", ylab = "Capacity (Percentage)")

plot(subset(resData[[8]], resData[[8]]$month == "February" & resData[[8]]$year == 1990)$datetime, 
     subset(resData[[8]], resData[[8]]$month == "February" & resData[[8]]$year == 1990)$Capacity, 
     type = "l", xlab = "Year", ylab = "Capacity (Percentage)")

# examining feb & march data
subset(resData[[8]], resData[[8]]$month == "February" & resData[[8]]$year == 1990)
subset(resData[[8]], resData[[8]]$month == "March" & resData[[8]]$year == 1990)
subset(resData[[8]], resData[[8]]$month == "April" & resData[[8]]$year == 1990)

# adjusting SNL values
for(i in 1:nrow(resData[[8]]))
{
  if(resData[[8]][i,]$Capacity > 150 & is.na(resData[[8]][i,]$Capacity) == F)
  {
    resData[[8]][i,]$Capacity = resData[[8]][i,]$Capacity/100
  }
  else{}
}

# Calculate difference from historical average
# dayDiff calculates the historic mean for any given day, calculates the percentage
# of that historic mean that a certain day's capacity measures at, and then loops 
# them all into a list to be added to a reservoir's data
dayDiff = function(resData)
{
  reservoirData.individual = resData
  dayVector = levels(as.factor(reservoirData.individual$day))
  histMeanVector = 0
  for(i in 1:length(dayVector))
  {
    histMeanVector[i] = mean(subset(reservoirData.individual, 
                                    day == dayVector[i])$Capacity, na.rm = TRUE)
  }
  
  histMeanList = list(dayVector, histMeanVector)
  names(histMeanList)= c("Day", "HistoricMean")
  histCapPercentage = sapply(1:length(reservoirData.individual$day), function(x) 
    {
    tempIndex = which(histMeanList$Day == reservoirData.individual[x,]$day)
    reservoirData.individual[x,]$Capacity/histMeanList$HistoricMean[tempIndex]*100}
  )
  return(histCapPercentage)
}


# 



# test queries
CDECquery(id, sensor, interval = "D", start, end)
CDECquery("CLE", 15, interval = "D", 2015-01-01, 2015-01-02)
CDECquery("CLE", 15, interval = "D", "2015-01-01", "2015-01-10")

trinityTest = CDECquery("CLE", 15, interval = "D", "1900-01-01", "2015-11-02")
trinityHourly = CDECquery("CLE", 15, interval = "H", "1900-01-01", "2015-11-02")

trinityTest$capacity = trinityTest$value/2447650 * 100
qplot(x = year, y = capacity, data = trinityTest, geom = "point")
ggplot(trinityTest, aes(capacity) + geom_area(aes(year,capacity0)))
plot(trinityTest$datetime, trinityTest$capacity, type = "l")


shastaTest = CDECquery("SHA", 15, interval = "D", "1962-11-01", "2015-11-02")

test = lapply(resList, function(x) CDECquery(x, 15, interval = "D", "1962-11-01", "2015-11-02"))
