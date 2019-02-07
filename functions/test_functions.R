#initial_master_data
source("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/initial_master_data.R")
start_time = Sys.time()
master_data = initial_master_data()
end_time = Sys.time()
time_taken = end_time - start_time
time_taken

rm(list=ls()[! ls() %in% c("master_data")])
#save.image("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/master_data.RData")

#update_master_data
rm(list = ls())
load("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/master_data.RData")
source("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/update_master_data.R")
start_time = Sys.time()
master_data = update_master_data(master = master)
end_time = Sys.time()
time_taken = end_time - start_time
time_taken

rm(list=ls()[! ls() %in% c("master_data")])
#save.image("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/master_data.RData")

#currency_info
rm(list = ls())
source("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/currency_info.R")

output = currency_info(currency_name = "bitcoin",snapshot = NULL)
snapshot = output[[2]]
View(output[[1]],"bitcoin")
View(currency_info(currency_name = "ethereum",snapshot = snapshot),"ethereum")
View(currency_info(currency_name = "ripple",snapshot = snapshot),"ripple")

#get_currency
rm(list = ls())
load("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/master_data.RData")
source("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/get_currency.R")

bitcoin = get_currency(currency_name = "bitcoin",master_data = master_data)
chart_Series(bitcoin)

ethereum = get_currency(currency_name = "ethereum",master_data = master_data)
chart_Series(ethereum)

ripple = get_currency(currency_name = "ripple",master_data = master_data)
chart_Series(ripple)

chart_Series(Cl(bitcoin))
add_TA(Cl(ethereum),on=1)

#get_currencies
rm(list = ls())
load("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/master_data.RData")
source("C:/Users/Bharath/Desktop/Insofe/crypto_currency/functions/get_currencies.R")

currencies = c("bitcoin","ethereum","ripple")
data = get_currencies(currencies = currencies,master_data = master_data)
bitcoin = data[["bitcoin"]]
ethereum = data[["ethereum"]]
ripple = data[["ripple"]]

chart_Series(bitcoin)
chart_Series(ethereum)
chart_Series(ripple)
chart_Series(Cl(bitcoin))
add_TA(Cl(ethereum),on=1)
