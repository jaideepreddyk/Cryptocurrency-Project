top_25 = function()
{
  rm(list = ls())
  options(scipen  = 999)
  library(quantmod)
  library(lubridate)
  
  CryptoDir = c("C:\\Users\\Jaideep Reddy\\Desktop\\INSOFE\\Crypto Proj\\Shiny")
  setwd(CryptoDir)
  
  DestFile = paste0(CryptoDir,"masterdata.zip")
  download.file(url="http://183.82.112.85:6262/media/datasets/masterdata.zip", destfile=DestFile)
  unzip_files = unzip(zipfile=DestFile, overwrite=TRUE)
  output = readRDS(unzip_files[[1]])
  
  master_data = output[[1]]
  CoinPriceList = output[[2]]
  SummaryFile = output[[3]]
  
  SummaryFile = SummaryFile[order(SummaryFile$MarketCap,decreasing = TRUE),]
  top_25_currencies = head(SummaryFile$Name,n = 25)
  top_25 = master_data[master_data$unique_name %in% top_25_currencies,]
  
  load("C:\\Users\\Jaideep Reddy\\Desktop\\INSOFE\\Crypto Proj\\Shiny\\USDINR_historic.RData")
  getFX("USD/INR",from = (max(index(USDINR_historic)))+days(1))
  if(nrow(USDINR)>0){USDINR_historic = rbind(USDINR_historic,USDINR)}
  save(USDINR_historic,file = "USDINR_historic.RData")
  
  inr_rate = as.data.frame(coredata(USDINR_historic))
  inr_rate["date"] = index(USDINR_historic)
  inr_rate = inr_rate[inr_rate$date >= as.Date(min(master_data$Date)),]
  
  required_dates = as.data.frame(unique(master_data$Date))
  required_dates = as.data.frame(required_dates[order(required_dates[,1],decreasing = FALSE),])
  colnames(required_dates) = "date"
  
  required_dates = merge(required_dates,inr_rate,by.x = "date",by.y = "date",all = TRUE)
  required_dates = na.locf(required_dates)
  required_dates$date = as.Date(required_dates$date,format = "%Y-%m-%d")
  required_dates$USD.INR = as.numeric(required_dates$USD.INR)
  
  top_25 = merge(top_25,required_dates,by.x = "Date",by.y = "date",all = TRUE)
  top_25["Close_INR"] = top_25$Close * top_25$USD.INR
  colnames(top_25)[8] = "crypto_name"
  
  return(top_25)
}

