this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
# source('createResults.R')
library("rjson")
rm ( list = ls() )

# Variables that show the information about the column in energy data
nameCol = 1
totalRealEnergyCol = 2
realEnergyCol = 3
EstimatedEnergyCol = 4
TotalEventsCol = 5
EstimatedEventsCol = 6
FalsePositiveEventsCol = 7

# Read the configuration files
totalPer <- fromJSON(file = "./configuration/total percentages.json")
timeBasedPer <- fromJSON(file = "./configuration/time based percentages.json")
eventBasedPer <- fromJSON(file = "./configuration/event based percentages.json")
fpMul <- fromJSON(file = "./configuration/false positive multipliers.json")

# Read the names of the data cretead by the createResults algorithm
dataCreated =  list.files(path = "./data created/")

# Create the header for the data error
headerDataError = c("appliance", "error", "reason")

# Loop for every data created 
for ( rep in 1 : length(dataCreated)){
  
  # Read the data that has benn created
  dataName = paste("./data created/", toString(dataCreated[rep]), sep = "") 
  energyData = read.csv(dataName, stringsAsFactors=FALSE)
  ldata = 1 : dim(energyData)[1]
  
  # The matrix that keeps information about every error
  dataError = c()
  
  # The variable of the total error
  totalError = 0
  
## Calculate the time based error
  timeBasedError = 0
  lTimeBased = 1 : length(timeBasedPer)
  
  # Calculate the total proportion from the corresponding time based appliance proportions
  totalTimeBasedPropotion = 0
  for ( i in lTimeBased )
    totalTimeBasedPropotion = totalTimeBasedPropotion + as.numeric(timeBasedPer[[i]]$percentage)
  
  # Loop for every time based appliance  
  for ( i in lTimeBased ){
    
    # Find the position in data pf the appliance
    inData = (energyData$name == timeBasedPer[[i]]$name)
    inData = ldata[inData]

    # Continue to next iteration if the appliance is not in the report
    if ( length(inData) == 0 ){
      next
    }
    
    # Read the percentage from the percentage file
    per = as.numeric(timeBasedPer[[i]]$percentage) / totalTimeBasedPropotion
    
    # Calculate the energy consumption error of the appliance
    error = 0
    if ( energyData[inData, realEnergyCol] != 0 || energyData[inData, EstimatedEnergyCol] != 0){
      # |real energy - estimated energy| / real energy
      error = abs(energyData[inData, realEnergyCol] - energyData[inData, EstimatedEnergyCol]) / energyData[inData, realEnergyCol] 
    }
 
    timeBasedError = timeBasedError + per * error
    
    # Change timeBasedError from NA to Inf if it is needed
    if ( error == Inf)
      timeBasedError = error
    
    # Bind the error information of the time based appliance to the array of errors
    dataError = rbind(dataError, c(timeBasedPer[[i]]$name, paste(as.numeric(error)*100, '%', sep = ""), 'energy consumption'))
  }
  
  ## Calculate the event based error which is divided into the correct event identification 
  ## and the correct energy determination of recognized events
  eventConsumptionError = 0
  eventIdentificationError = 0
  
  lEventBased = 1 : length(eventBasedPer)
  
  # Calculate the total proportion from the corresponding event based appliance proportions
  totalEventPropotion = 0
  for ( i in lTimeBased )
    totalEventPropotion = totalEventPropotion + as.numeric(timeBasedPer[[i]]$percentage)
  
  # Loop for every event based appliance 
  for ( i in lEventBased ){
    
    # Find the position in data pf the appliance
    inData = (energyData$name == eventBasedPer[[i]]$name)
    inData = ldata[inData]

    # # Continue to next iteration if the appliance is not in the report
    # if ( length(inData) == 0 )
    #   next
    
    # Read the percentage from the percentage file
    per = as.numeric(eventBasedPer[[i]]$percentage) / totalEventPropotion
    
    # Calculate the energy consumption error of the appliance
    error = 0
    if ( energyData[inData, realEnergyCol] != 0 || energyData[inData, EstimatedEnergyCol] != 0)
      # |real energy - estimated energy| / real energy 
      error = abs(energyData[inData, realEnergyCol] - energyData[inData, EstimatedEnergyCol]) / energyData[inData, realEnergyCol] 
    
    eventConsumptionError = eventConsumptionError + per * error
    
    # Bind the error information (energy consumption) of the event based appliance to the array of errors
    dataError = rbind(dataError, c(eventBasedPer[[i]]$name, paste(as.numeric(error)*100, '%', sep = ""), 'energy consumption'))
    
    # Calculate the event identification error of the appliance
    appError = 0
    if ( energyData[inData, TotalEventsCol] != 0)
      # (1 - estimated events) / real events
      appError =  (1 - energyData[inData, EstimatedEventsCol] / energyData[inData, TotalEventsCol])
    
    eventIdentificationError = eventIdentificationError + per * appError
    
    # Bind the error information (event recognition) of the event based appliance to the array of errors
    dataError = rbind(dataError, c(eventBasedPer[[i]]$name, paste(as.numeric(appError)*100, '%', sep = ""), 'event recognition'))
  }
  
  
  # Calculate the false detection error
  
  falsePositiveError = 0
  
  lFPIdentification = 1 : length(fpMul)
  # Read the global false positive input
  for ( i in 1 : length(totalPer)) {
    FP  = ( totalPer[[i]]$name == 'fpMultiplier' ) # multiplier here
    if (FP){
      FP =  as.numeric(totalPer[[i]]$multiplier)
      break
    }
  }
  
  # Loop for every event based appliance 
  for ( i in lFPIdentification){
    
    # Find the position in data pf the appliance
    inData = (energyData$name == fpMul[[i]]$name)
    inData = ldata[inData]
    
    # # Continue to next iteration if the appliance is not in the report
    # if ( length(inData) == 0 )
    #   next
    
    # Check whether is a global false positive multiplier or otherwise use the corresponding one
    if ( FP )
      per = FP
    else
      per = fpMul[[i]]$multiplier # Read the percentage from the percentage file
    
    # Calculate the false detection error of the appliance
    error = 0
    if ( energyData[inData, FalsePositiveEventsCol] != 0){
      # (false positive events) / (false positives events + estimated events) estimated events -> true positive
      error = energyData[inData, FalsePositiveEventsCol] / (energyData[inData, FalsePositiveEventsCol] + energyData[inData, EstimatedEventsCol])
    }

    
    falsePositiveError = falsePositiveError + per * error
    
    # Bind the error information (false positive) of the event based appliance to the array of errors
    dataError = rbind(dataError, c(fpMul[[i]]$name, paste(as.numeric(error)*100, '%', sep = ""), 'false positive'))
  }
  
  # Read the final percentages
  percentages = vector(mode = 'integer', length = length(totalPer)-1)
  for ( i in 1 : ( length(totalPer) - 1) ) {
    percentages[i] = as.numeric(totalPer[[i]]$percentage) / 100
  }
  
  # Read from somewhere the impression
  impression = 0
  
  # Variables that show the information about the row in configuration file
  timeBasedErrorRow = 1
  eventConsumptionErrorRow = 2
  eventIdentificationErrorRow = 3
  impressionRow = 4
  falsePositiveErrorRow = 5
  
  # Calculate the total error using the percentages of the corresponding configuration file
  totalError = percentages[timeBasedErrorRow] * timeBasedError + percentages[eventConsumptionErrorRow] * eventConsumptionError + 
                  percentages[eventIdentificationErrorRow] * eventIdentificationError + falsePositiveError + percentages[impressionRow] * impression
  
  # Bind the total error information to the array of errors
  dataError = rbind(dataError, c('Total Error', paste(as.numeric(totalError)*100, '%', sep = ""), ' '))
  
  # Bind the accuracy information to the array of errors
  dataError = rbind(dataError, c('Accuracy', paste(as.numeric(1 - totalError)*100, '%', sep = ""), ' '))
  
  # Extract the date from the name of the data created
  date = substr(dataCreated[rep], 5, 12)
  
  # Print the accuracy to the console
  print(c('Accuracy', paste(as.numeric(1 - totalError)*100, '%', sep = ""), date))
  
  # Create the name of the file that the error information will be written
  nameDataError = paste("./error info/dataError", toString(date), ".csv", sep = "") 
  
  # Write the error information to the file
  write.table(dataError, nameDataError, row.names = FALSE, col.names = headerDataError, sep = ',', na="")
}

