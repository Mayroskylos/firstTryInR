library("lubridate")
##  Function
# name : fill_timeseries
# brief : takes a time series and fills the missing values
# params :  
# @ dataName : the name of the time series
# return : void
##
fill_timeseries  <- function (dataName){
  
  # Read the time series of power data
  powerData = read.csv(dataName)
  
  # Split the time data from time series 
  oldPowerT = powerData[,1]/1000  # for milliseconds to seconds
  
  # Split the power data from time series
  oldPower = powerData[,2]
  
  # Calculate the seconds in a day
  daySec = 24 * 3600
  
  # Find the first date in time data
  firstDate = as.POSIXct(oldPowerT[1], origin = "1970-01-01", tz = "Europe/Amsterdam")
  # firstDate = as.POSIXct(oldPowerT[1], origin = "1970-01-01", tz = "UTC")  # In case of problem not solved
  print(dataName)
  print(firstDate)
  # firstDate = strftime(firstDate, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  sec = second(firstDate)
  minut = minute(firstDate)
  hours = hour(firstDate)
  
  # Shift the first date to 00:00:00 of that date and then 3 before
  if ( hours != 21 || minut != 0 || sec != 0){
    firstDate = firstDate - 3600 * hours - 60 * minut - sec - 3 * 3600
  }
  firstDateInSec = as.numeric(firstDate) 
  
  # Find the last date in time data
  lastDate = as.POSIXct(oldPowerT[length(oldPowerT)], origin = "1970-01-01", tz = "Europe/Amsterdam")
  sec = second(lastDate)
  minut = minute(lastDate)
  hours = hour(lastDate)
  print(lastDate)
  # Shift the last date to 23:59:59 of that date
  lastDate = lastDate - 3600 * hours - 60 * minut - sec + daySec - 1
  lastDateInSec = as.numeric(lastDate) 
  
  dt = lastDateInSec - firstDateInSec
  
  # If the time series is already full stop
  if ( length(oldPowerT) >= dt){
    print('I was ready!')
    return()
  }
  
  # Create the arrays of the new time series
  newPowerT = firstDateInSec : lastDateInSec     # the three hours before
  newPower = vector(mode = 'integer', length = length(newPowerT)) - 1
  
  # Copy the power that already exists
  newPower[oldPowerT %% firstDateInSec + 1] = oldPower
  
  # If there is not a value assigned yet then give the previous one
  if ( newPower[1] == -1){
    newPower[1] = 0
  }
  for ( i in 1 : length(newPower)){
    if ( newPower[i] == -1){
      newPower[i] = newPower[i-1]
    }
  }
  
  # Create the header for the time series
  header = c('DateTime', 'Measurements')
  
  # Change from seconds to millisends again
  newPowerT  = newPowerT * 1000
  
  # Rewrite the time series
  dfTime <- data.frame(newPowerT)
  dfWatt <- data.frame(newPower)
  write.table(c(dfTime,dfWatt),dataName, row.names = FALSE, col.names = header, sep = ',', na="")
  
  return()
  
}