rm( list = ls())
name = '3710dryer'
dataName = paste('./power data/', name, ".csv", sep = "")
dataName = 'dr.csv'
powerData = read.csv(dataName)
oldPowerT = powerData[,1]/1000
oldPower = powerData[,2]
daySec = 24*3600

firstDate = as.POSIXct(oldPowerT[1], origin = "1970-01-01", tz = "UTC")
sec = second(firstDate)
minut = minute(firstDate)
hours = hour(firstDate)
firstDate = firstDate - 3600 * hours - 60 * minut - sec
firstDateInSec = as.numeric(firstDate)

lastDate = as.POSIXct(oldPowerT[length(oldPowerT)], origin = "1970-01-01", tz = "UTC")
sec = second(lastDate)
minut = minute(lastDate)
hours = hour(lastDate)
lastDate = lastDate - 3600 * hours - 60 * minut - sec + daySec - 1
lastDateInSec = as.numeric(lastDate)

dt = lastDateInSec - firstDateInSec

if ( length(oldPowerT) > dt){
  print('already done')
}

newPowerT = 0 : dt + firstDateInSec
newPower = vector(mode = 'integer', length = length(newPowerT)) - 1
newPower[oldPowerT %% firstDateInSec + 1] = oldPower
if ( newPower[1] == -1)
  newPower[1] = 0
for ( i in 2 : length(newPower)){
  if ( newPower[i] == -1)
    newPower[i] = newPower[i-1]
}


header = c('DateTime', 'Measurements')
newPowerT  = newPowerT * 1000
df <- data.frame(newPowerT)
df1 <- data.frame(newPower)
write.table(c(df,df1),'./test/dr.csv', row.names = FALSE, col.names = header, sep = ',', na="")



