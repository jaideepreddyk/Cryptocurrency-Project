get_currency = function(currency_name, master_data)
{
  required = master_data[master_data$unique_name == currency_name,]
  required_date = as.Date(required$Date)
  required = required[,2:7]
  library("quantmod")
  required = as.xts(required,order.by = required_date)
  return(required)
}