source("fill_timeseries.R")
##  Function
# name : add_timeseries
# brief : find if there are more than one time series that need to be added in order to get teh final one
# params :  
# @ appName : the name of the appliance
# return : void
##
add_timeseries  <- function (appName){
  
  # Read the names of the data that have been supplied
  data = list.files('./supply data/')
  
  # Create a boolean variable for the first time the appliance name will be found
  first = TRUE
  
  # Initialise the aggregated time series
  aggregateTimeseries = c()
  
  # Loop to add the time series
  for ( i in 1 : length(data)){
    
    # If applinace name was found in the name of the supplied data
    if ( grepl(appName, data[i]) ){
      tic = Sys.time()
      # If it is the first time, write the data to aggregateTimeseries
      if ( first ){
        
        # Read the time series
        dataName = paste('./supply data/', data[i], sep = '')
        
        
        # Use the fuction fill_timeseries to fill the time series
        fill_timeseries(dataName)
        
        
        # Transfer data to aggregateTimeseries
        aggregateTimeseries = read.csv(dataName)
        
        # Update the variable first
        first = FALSE
      } else {  # If there are more than one time series with the appliance name in it
        
        # Read the time series
        dataName = paste('./supply data/', data[i], sep = '')

        # Use the fuction fill_timeseries to fill the time series
        fill_timeseries(dataName)
        
        # Add just the power
        aggregateTimeseries[,2] = aggregateTimeseries[,2] + read.csv(dataName)[,2]
      } 
      print(Sys.time() - tic)
    }
  }
  
  # No timeseries was found 
  if (first){
    return()
  }
  
  # Crete the name to be saved
  aggregatedDataName = paste('./power data/', appName, '.csv', sep = '')
  
  # Create the header for the time series
  headerAggregateTimeseries = c('DateTime', 'Measurements')
  
  # Rewrite the time series
  write.table(aggregateTimeseries, aggregatedDataName, col.names = headerAggregateTimeseries, row.names = FALSE, sep = ',', na="")
  return()
}

