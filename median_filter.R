##  Function
# name : median_filter
# brief : passes the power from a median filter
# params :  
# @ power : the power data
# return : power after median filter
##
median_filter  <- function (power){
  P = 25
  windowMedian = power[1:(2*P+1)]
  nextValue = 1
  for ( i in (P+1) : (length(power)-P-1) ){
    power[i] = median(windowMedian)
    windowMedian[nextValue] = power[i+P+1]
    nextValue = nextValue + 1
    if ( nextValue > (2*P+1)){
      nextValue = 1
    }
  }
  return(power)
}