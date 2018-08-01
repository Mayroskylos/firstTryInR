rm( list = ls())
library("rjson")
library("lubridate")
source("extract_data_from_timeseries.R")
source("add_timeseries.R")

# Read all the reports in the corresponding file
reports =  list.files(path = "./reports/")

# Read the time based and event based configuration files in orrder to extract the name of the appliances
timeBasedPer <- fromJSON(file = "./configuration/time based percentages.json")
eventBasedPer <- fromJSON(file = "./configuration/event based percentages.json")

# Find all the corresponding appliances 
appliance = c()

for (i in 1 : length(timeBasedPer)){
  appliance = c(appliance, timeBasedPer[[i]]$name)
}

for (i in 1 : length(eventBasedPer)){
  appliance = c(appliance, eventBasedPer[[i]]$name)
}

lapp = length(appliance)
tic <- Sys.time()
# Use the fuction add_timeseries to get the final time series in order to be ready for the data extraction
for ( i in 1 : lapp){
  add_timeseries(appliance[i])
}
print(Sys.time() - tic)
# Array that keeps the estimated anergy of alwaysOn appliance, that will be used to calculate the real energy
alwaysOnArray = c()

# Create header for the data to be extracted
header = c('name', 'totalRealEnergy', 'realEnergy', 'EstimatedEnergy', 'number of events', 
           'number of estimated events', 'number of false positive events')

for ( rep in 1 : length(reports)){
  
  # Read the report
  json_file = paste("./reports/", reports[rep], sep = "")
  json_data <- fromJSON(file = json_file)
  
  # Extract the date from the report
  date = json_data$date
  print(date)
  
  # Separate the reported events from the json data
  spec = json_data$reportEvents
  numOfReportEvents = length(spec)
  
  # Initialize data matrix
  data = c()
  
  # Helpful time metrics  MAYBE NOT NEEDED
  for ( i in 1 : numOfReportEvents) {
    alwaysOn  = ( spec[[i]]$applianceType == 'alwaysOn' ) # multiplier here
    if (alwaysOn){
      hours = (spec[[i]]$stopTime - spec[[i]]$startTime + 1) / 3600  # the ammount of hours in json
      break
    }
  }
  
  # Loop for every appliance
  for ( i in 1 : lapp){
    
    # Initialize the metrics of appliance
    name = appliance[i]
    tRealEnergy = 0
    rEnergy = 0
    eEnergy = 0
    nEvents = 0
    nEstimatedEvents = 0
    nFalseEvents = 0
    
    # Calculate the estimated energy and the time slots of the event
    time = c()
    for ( j in 1 : numOfReportEvents){
      if (spec[[j]]$applianceType == name){
        eEnergy = eEnergy + spec[[j]]$usage
        tstart = spec[[j]]$startTime
        tend = spec[[j]]$stopTime
        time = rbind( time, c(tstart, tend))
      }
    }
    
    # Create the name of the file with the data that will be used
    dataName = paste('./power data/', name, ".csv", sep = "")
    
    if ( file.exists(dataName) ){       # Create the data if the file exists
      
      if ( name != "alwaysOn5"){         # The appliance that is not alwaysOn (if alwaysOn$ ($ = number) then for median filter)

        # Create the data using the functipn extract_data_from_timeseries
        newdata = extract_data_from_timeseries(dataName, time, hours, eEnergy, date, name)
        # print(newdata)
        
        # Add the name of the appliance to the data array
        newdata = c(name, newdata)
        
      } else {                          # AlwaysOn will be tuned differnetly 
        
        # Add the estimated energy to the array of alwaysOn
        alwaysOnArray = c(alwaysOnArray, eEnergy)
        
        # Estimate the real energy using the median of the till now estimated energy consumption of alwaysON
        rEnergy = median(alwaysOnArray)
        tRealEnergy = rEnergy
        
        # Bind the new data into one array
        newdata = c(name, tRealEnergy, rEnergy, eEnergy, nEvents, nEstimatedEvents, nFalseEvents)
      }
      
    } else {                          # Inform the user that the file is missing
      
      # Create the error message that the file is missing
      errorMessage = paste("There is no such file (name = ", dataName, ")", sep = "" )
      
      # Pirnt the error message
      print(errorMessage)
      
      # Use random generation methods to fill the data missing in order to have some results
      rEnergy = eEnergy
      tRealEnergy = rEnergy
      nEstimatedEvents = length(time) / 2
      zeros = vector(mode = 'integer', length = 99999)
      
      nEvents = nEstimatedEvents + sample(c(zeros),1)
      nFalseEvents = sample(c(zeros),1)
      
      # Bind the new data into one array
      newdata = c(name, tRealEnergy, rEnergy, eEnergy, nEvents, nEstimatedEvents, nFalseEvents)
      
    }
    
    # Bind the new data into total data matrix
    data = rbind(data, newdata)
  }
  
  # Write the data to a file
  dataName  = paste("./data created/data", toString(date), ".csv", sep = "") 
  write.table(data, dataName, row.names = FALSE, col.names = header, sep = ',', na="")
  print(data)  
  
}




