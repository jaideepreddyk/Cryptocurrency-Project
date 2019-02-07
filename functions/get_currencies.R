get_currencies = function(currencies,master_data)
{
  library("quantmod")
  temp = list()
  for(i in 1:length(currencies))
  {
    required = master_data[master_data$unique_name == currencies[i],]
    required_date = as.Date(required$Date)
    required = required[,2:7]
    required = as.xts(required,order.by = required_date)
    temp[[i]] = required
  }
  names(temp) = currencies
  return(temp)
}