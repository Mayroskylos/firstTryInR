source("median_filter.R")
##  Function
# name : extract_data_from_timeseries
# brief : calculates and extract the necessary data
# params :  
# @ dataName : the name of the file that has the time series
# @ time : a matrix that has the starting and stopping time of the appliance event
# @ hours : the hours inside the report (usually a day -> 24)
# @ eEnergy : the estimated usage of the appliance in regards to report data
# @ date : the date of the report
# @ name : the name of the appliance
# return : ndata -> an array that holds the data for the real energy in regards to estimated energy, the estimated
#                   energy, the times the appliance was on, the correct estimated times the appliance was on, and
#                   the falsely estimated time the appliance was on
##
extract_data_from_timeseries <- function (dataName, time, hours, eEnergy, date, name){
  # Initialise the variables that will be returned to zero
  realEnergy = 0
  totalRealEnergy = 0
  numberEvents = 0
  numberEstimatedEvents = 0
  numberFalseEvents = 0
  
  # Day in seconds
  lenDayBefore = 3*3600
  dayInSec = 86399 + lenDayBefore # the three hours before
  
  # Read the data of the time series
  powerData = read.csv(dataName)
  
  # Split the time data from time series
  powerT = powerData[,1]/1000
  # totalTime = (powerT[length(powerT)] - powerT[1] )
  # extraSec = totalTime - hours * 3600;
  lenTime = 1:length(powerT)
  
  # Calculate the date in seconds
  dateInSec = ymd(date)
  dateInSec = strftime(dateInSec, format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  dateInSec = as.POSIXct(dateInSec, origin = "1970-01-01", tz = "GMT")
  dateInSec = force_tz(dateInSec, "Europe/Amsterdam")
  dateInSec = as.numeric(dateInSec) - lenDayBefore   # the three hours before
  
  # Find the position of day in the time data
  dateInDataBase = (powerT == dateInSec)
  dateInDataBase = lenTime[dateInDataBase]
  
  # Split the power data from time series
  power = powerData[,2]
  dayPower = power[ 0 : (dayInSec-1) + dateInDataBase] # the power data of that day plus 3 hours before
  
  # Start to save a visual image of the time series of the appliance 
  jpeg(paste('./visualisation/', toString(date), ' ', toString(name), '.jpg', sep = ""))
  ylimit = c(0, max(dayPower, 400) + 10)
  plot(1 : dayInSec, dayPower, ylim = ylimit, type = 'l')
  
  # Create a fake pulse for the image
  dayBeforePower = vector(mode = 'integer', length = (lenDayBefore+2))
  dayBeforePower[2:(lenDayBefore+1)] = 400
  
  # Highlight the day before part
  lines(1:(lenDayBefore+2), dayBeforePower, col = 'red')
  
  ## Calculate the variables to be returned
  if ( length(time) > 0) {         # If there is at least one appearance of this appliance in the report file
    dt = time[1,2] - time[1,1] 
    
    # Differentiate the time based appliance from the event Based
    if (dt == (hours * 3600 - 1)){  # Time based appliances
      
      # Find the start and end time of the event in the time data
      tstart = (powerT == (time[1]))
      tstart = lenTime[tstart]
      tend = (powerT == (time[2]))
      tend = lenTime[tend]
      
      # Keep only the power of the day
      dayPower = power[tstart:tend]
      
      if ( name == 'alwaysOn'){
        dayPower = median_filter(dayPower)
        realEnergy = min(dayPower) * hours / 1000
        totalRealEnergy = realEnergy
        lines(1 : dayInSec, c(vector(mode = 'integer', length = lenDayBefore-1), dayPower), col = 'red')
        
      } else {
        # Keep only the power over 5 due to disturbances
        times = dayPower > 5
        
        # Calculate the real energy 
        realEnergy = sum(dayPower[times]) / (3600 * 1000)  # W/ s -> KWh
        totalRealEnergy = realEnergy
      }

      
      # Close the save image process
      dev.off()
      
      # Fill the array with the variables to be returned
      ndata <- c(totalRealEnergy, realEnergy, eEnergy, numberEvents, numberEstimatedEvents, numberFalseEvents)
      
      return(ndata)
      # plot(1:length(powerT), power)
    } else {                        # Event based appliances
      
      ## Cacluate the events of the appliance in that day
      # Split an hour to fractions
      hoursFraction = 4
      sec = 3600 / hoursFraction
      timeStartArray = vector(mode = 'integer', length = (hoursFraction * hours))
      timeEndArray = vector(mode = 'integer', length = (hoursFraction * hours))
      timeWithEnergy = vector(mode = 'integer', length = (hoursFraction * hours))
      
      # Loop for one day
      for ( i in 1 : (hoursFraction * hours)){
        
        # Calculate the starting and ending time
        ts = (i-1) * sec + dateInDataBase + lenDayBefore
        te = i * sec - 1 + dateInDataBase + lenDayBefore
        
        # Split the power of that time fraction
        realPower = power[ts:te]
        
        # Keep only the power over 5 due to disturbances
        times = realPower > 5
        
        # Calculate the energy in that time fraction
        timeEnergy = sum(realPower[times]) / (3600 * 1000)  # W/ s -> KWh
        
        # Add to the arrays
        timeStartArray[i] = ts
        timeEndArray[i] = te
        timeWithEnergy[i] = timeEnergy
      }
      
      # Loop that checks if the appliance is on from the day before
      extraHours = lenDayBefore / 3600
      for ( i in (extraHours * hoursFraction) : 1){
        if ( timeWithEnergy[1] > 0){
          
          # Calculate the starting and ending time
          ts = (i-1) * sec + dateInDataBase
          te = i * sec - 1 + dateInDataBase
          
          # Split the power of that time fraction
          realPower = power[ts:te]
          
          # Keep only the power over 5 due to disturbances
          times = realPower > 5
          
          # Calculate the energy in that time fraction
          timeEnergy = sum(realPower[times]) / (3600 * 1000)
          
          # Add to the array
          timeStartArray = c(ts, timeStartArray)
          timeEndArray = c(te, timeEndArray)
          timeWithEnergy = c(timeEnergy, timeWithEnergy)
        } else {
          break
        }
      }
      
      # Matrix that has the starting, ending time and the energy consumption of the appliance
      applianceOnMatrix = c()      
      # applianceOnArray : Temp array that has the starting, ending time and the energy consumption of the appliance for one event
      
      # Loop to calculate the times the event based appliance is on and construct a matrix with data
      for ( i in 2 : length(timeWithEnergy)){
        # every time the pulse of energy is rising
        if (timeWithEnergy[i] > 1e-5 && timeWithEnergy[i-1] < 1e-5){
          applianceOnArray = timeStartArray[i]
          ts = i
        }
        # Add one usage every time the pulse of energy is fading
        if ( timeWithEnergy[i] < 1e-5 && timeWithEnergy[i-1] > 1e-5){
          numberEvents = numberEvents + 1
          applianceOnArray = c(applianceOnArray, timeEndArray[i-1], sum(timeWithEnergy[ts:i]))
          applianceOnMatrix = rbind(applianceOnMatrix, applianceOnArray)
        }
      }
      
      # Calculate the total energy for this appliance
      totalRealEnergy = sum(applianceOnMatrix[,3])
      
      # Loop for every use of the appliance in day
      for ( i in 1 : dim(time)[1]) {
        
        # Metric that has to do with the difference between the report and the time series
        # two = 2 * 3600
        two = 0
        
        # Find the start and end time of the event in the time data
        tstart = (powerT == (time[i,1]))
        tstart = lenTime[tstart]
        tend = (powerT == (time[i,2]))
        tend = lenTime[tend]
        # par(ask = FALSE)
        # plot(tstart:tend, power[tstart:tend+two])
        # plot(1:length(powerT), power)
        
        energy = sum(power[tstart:tend]) / (3600 * 1000) # W/s to KWh
        
        # Using energy decide between true positve ro false positive
        if (energy < 0.01){             # false positive
          
          # Update the number of false events
          numberFalseEvents = numberFalseEvents + 1
          
          # Create a fake pulse for the image
          falsePower = vector(mode = 'integer', length = (dt+3))
          falsePower[2:(dt+2)] = 200
          
          # Highlight the estimated part
          lines((tstart-1):(tend+1) - dateInDataBase, falsePower, col = 'green')
        } else {                       # true positive
          
          # Calculate the median of the time on 
          tmiddle = (tstart + tend) / 2
          
          # Loop that calcuates the energy 
          for ( j in 1 : dim(applianceOnMatrix)[1]){
            if ( tmiddle >= applianceOnMatrix[j,1] && tmiddle <= applianceOnMatrix[j,2]){
              energy = applianceOnMatrix[j,3]
            }
          }
          
          #  Update the number of estimated events and real energy
          realEnergy = realEnergy + energy
          numberEstimatedEvents = numberEstimatedEvents + 1
          
          # Highlight the estimated part
          lines(tstart:tend - dateInDataBase, dayPower[tstart:tend - dateInDataBase], col = 'green')
        }
      }
    }
  } else {   # If there is none appearance of this appliance in the report file
    ## Cacluate the events of the appliance in that day
    # Split an hour to fractions
    hoursFraction = 4
    sec = 3600 / hoursFraction
    timeStartArray = vector(mode = 'integer', length = (hoursFraction * hours))
    timeEndArray = vector(mode = 'integer', length = (hoursFraction * hours))
    timeWithEnergy = vector(mode = 'integer', length = (hoursFraction * hours))
    
    # Loop for one day
    for ( i in 1 : (hoursFraction * hours)){
      
      # Calculate the starting and ending time
      ts = (i-1) * sec + dateInDataBase + lenDayBefore
      te = i * sec - 1 + dateInDataBase + lenDayBefore
      
      # Split the power of that time fraction
      realPower = power[ts:te]
      
      # Keep only the power over 5 due to disturbances
      times = realPower > 5
      
      # Calculate the energy in that time fraction
      timeEnergy = sum(realPower[times]) / (3600 * 1000)
      
      # Add to the arrays
      timeStartArray[i] = ts
      timeEndArray[i] = te
      timeWithEnergy[i] = timeEnergy
    }
    
    # Loop that checks if the appliance is on from the day before
    extraHours = lenDayBefore / 3600
    for ( i in (extraHours * hoursFraction) : 1){
      if ( timeWithEnergy[1] > 0){
        
        # Calculate the starting and ending time
        ts = (i-1) * sec + dateInDataBase
        te = i * sec - 1 + dateInDataBase
        
        # Split the power of that time fraction
        realPower = power[ts:te]
        
        # Keep only the power over 5 due to disturbances
        times = realPower > 5
        
        # Calculate the energy in that time fraction
        timeEnergy = sum(realPower[times]) / (3600 * 1000)
        
        # Add to the array
        timeStartArray = c(ts, timeStartArray)
        timeEndArray = c(te, timeEndArray)
        timeWithEnergy = c(timeEnergy, timeWithEnergy)
      } else {
        break
      }
    }
    
    # Matrix that has the starting, ending time and the energy consumption of the appliance
    applianceOnMatrix = c()      
    # applianceOnArray : Temp array that has the starting, ending time and the energy consumption of the appliance for one event
    
    # Loop to calculate the times the event based appliance is on and construct a matrix with data
    for ( i in 2 : length(timeWithEnergy)){
      # every time the pulse of energy is rising
      if (timeWithEnergy[i] > 1e-5 && timeWithEnergy[i-1] < 1e-5){
        applianceOnArray = timeStartArray[i]
        ts = i
      }
      # Add one usage every time the pulse of energy is fading
      if ( timeWithEnergy[i] < 1e-5 && timeWithEnergy[i-1] > 1e-5){
        numberEvents = numberEvents + 1
        applianceOnArray = c(applianceOnArray, timeEndArray[i-1], sum(timeWithEnergy[ts:i]))
        applianceOnMatrix = rbind(applianceOnMatrix, applianceOnArray)
      }
    }
  }
  
  # Calculate the total energy for this appliance
  totalRealEnergy = sum(applianceOnMatrix[,3])
  
  # Close the save image process
  dev.off()
  
  # Fill the array with the variables to be returned
  ndata <- c(totalRealEnergy, realEnergy, eEnergy, numberEvents, numberEstimatedEvents, numberFalseEvents)
  
  return(ndata)
  
}