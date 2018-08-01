rm( list = ls())
time = c()
time = rbind(time, c(1528024586, 1528027454))
time = rbind(time, c(1528029621, 1528031985))
# time = rbind(time, c(1528632695, 1528635759))
# time = rbind(time, c(1528657148, 1528660103))
# time = rbind(time, c(1527804000, 1527890399))


# name = 'refrigeration'
name = 'alwaysOn'
dataName = paste('./power data/', name, ".csv", sep = "")
date = 20180603
# fill_chronoseries(dataName, hours)
powerData = read.csv(dataName)
hours = 24
eEnergy = 2.384
 
realEnergy = 0
numberEvents = 0
numberEstimatedEvents = 0
numberFalseEvents = 0

powerT = powerData[,1]/1000
totalTime = (powerT[length(powerT)] - powerT[1] )
extraSec = totalTime - hours * 3600;
lenTime = 1:length(powerT)
date = ymd(date)
# dateInSec1 = as.POSIXct(dateInSec, origin = "1970-01-01", tz = "UTC")
date = strftime(date, format="%Y-%m-%d %H:%M:%S", tz = "GMT")
dateInSec = as.POSIXct(date, origin = "1970-01-01", tz = "GMT")
dateGMT = as.numeric(dateInSec)
dateInSec = force_tz(dateInSec, "Europe/Amsterdam")
dateInSec = as.numeric(dateInSec)
diffTime = dateInSec - dateGMT
print(diffTime)
dateInDataBase = (powerT == dateInSec)
dateInDataBase = lenTime[dateInDataBase]
power = powerData[,2]
dayPower = power[ 1 : 86399 + dateInDataBase] # the power data of that day

dt = time[1,2] - time[1,1] 

if (dt == (hours * 3600 - 1)){
  times = dayPower > 5
  realEnergy = sum(dayPower[times]) / (3600 * 1000)
  jpeg(paste('./visualisation/', toString(date), ' ', toString(name), '.jpg', sep = ""))
  plot(1:dt,dayPower, type = 'l')
  dev.off()
  # plot(1:length(powerT), power)
} else {
  jpeg(paste('./visualisation/', toString(date), ' ', toString(name), '.jpg', sep = ""))
  ylimit = c(0, max(dayPower, 200) + 10)
  plot(1 : 86399, dayPower, ylim = ylimit, type = 'l')
  for ( i in 1 : dim(time)[1]) {
    # two = 2 * 3600
    two = 0
    tstart = (powerT == (time[i,1]))
    tstart = lenTime[tstart]
    tend = (powerT == (time[i,2]))
    tend = lenTime[tend]
    # par(ask = FALSE)
    # plot(tstart:tend - dateInDataBase, power[tstart:tend+two], type = 'l')

    # plot(1:length(powerT), power)
    
    energy = sum(power[tstart:tend+two]) / (3600 * 1000) # W/s to KWh
    if (energy < 0.01){
      numberFalseEvents = numberFalseEvents + 1
      falsePower = vector(mode = 'integer', length = (dt+3))
      falsePower[2:(dt+2)] = dayPower[tstart:tend - dateInDataBase] + 200
      
      lines((tstart-1):(tend+1) - dateInDataBase, falsePower, col = 'green')
    }else{
      realEnergy = realEnergy + energy
      numberEstimatedEvents = numberEstimatedEvents + 1
      lines(tstart:tend - dateInDataBase, dayPower[tstart:tend - dateInDataBase], col = 'green')
    }
    
    
  }
  dev.off()
}

hoursFraction = 4
sec = 3600 / hoursFraction
timeWithEnergy = vector(mode = 'integer', length = (hoursFraction * hours))

for ( i in 1 : (hoursFraction * hours)){
  ts = (i-1) * sec + dateInDataBase
  te = i * sec + 1 + dateInDataBase
  realPowerT = powerT[ts:te]
  realPower = power[ts:te]
  times = realPower > 5
  timeEnergy = sum(realPower[times]) / (3600 * 1000)
  timeWithEnergy[i] = timeEnergy
}
for ( i in 2 : length(timeWithEnergy)){
  if ( timeWithEnergy[i] < 1e-5 && timeWithEnergy[i-1] > 1e-5)
    numberEvents = numberEvents + 1
}

ndata <- c(realEnergy, eEnergy, numberEvents, numberEstimatedEvents, numberFalseEvents)
print(ndata)
