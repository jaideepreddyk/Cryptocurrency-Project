update_master_data = function(master = master_data)
{
  library("rvest")
  library("RCurl")
  library("XML")
  library(stringr)
  
  url1 = 'https://coinmarketcap.com/all/views/all/'
  url2<-getURL(url1)
  parsed<-htmlParse(url2)
  links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
  links = do.call(rbind.data.frame, links)
  links = as.data.frame(links[!duplicated(links[,1]),])
  links = as.data.frame(links[grepl("/currencies/",links[,1]),])
  links = as.data.frame(links[!grepl("#markets",links[,1]),])
  links = as.data.frame(links[!grepl("https://",links[,1]),])
  links["count"] = str_count(links[,1],"/")
  links = links[links$count == 3,]
  
  links[,1] = gsub("\\/currencies/","",links[,1])
  links[,1] = gsub("\\/","",links[,1])
  colnames(links)[1] = "unique_name"
  links[,2] = NULL
  
  temp = as.data.frame(master[!duplicated(master$unique_name),"unique_name"])
  temp = as.data.frame(temp[!temp[,1] %in% links$unique_name,])
  links = rbind(links,temp)

  start_date = max(master$Date)+1
  start_date = format(start_date,"%Y%m%d")
  end_date=Sys.Date()+1
  end_date = format(end_date,"%Y%m%d")
  url2 = "https://coinmarketcap.com/currencies/"
  url3 = "/historical-data/?start="
  
  master_data = data.frame()
  xpath1 = '//*[@id="historical-data"]/div/div[3]/table'
  xpath2 = "/html/body/div[3]/div/div[1]/div[3]/div[1]/h1"
  xpath3 = '/html/body/div[3]/div/div[1]/div[4]/div[2]'
  xpath4 = "/html/body/div[3]/div/div[1]/div[4]/div[1]/h1"
  xpath5 = '/html/body/div[3]/div/div[1]/div[5]/div[2]/ul'
  
  for(i in 1:length(links$unique_name))
  {
    url4 = paste(url2,links$unique_name[i],url3,start_date,"&end=",end_date, sep = "")
    currency_page = read_html(url4)
    
    historic_table = html_node(currency_page, xpath = xpath1)
    historic_table = html_table(historic_table)
    
    last_updated = Sys.time()
    
    temp1 = html_node(currency_page, xpath = xpath2)
    temp1 = html_text(temp1)
    temp1 = strsplit(temp1,"\n")
    temp1 = temp1[[1]]
    name = temp1[3]
    if(!is.na(name))
    {
      name = trimws(name, which = "left")
      symbol = temp1[2]
      symbol = trimws(symbol, which = 'left')
      symbol = gsub("\\(","",symbol)
      symbol = gsub("\\)","",symbol)
      
      temp2 = html_node(currency_page, xpath = xpath3)
      temp2 = html_text(temp2)
      temp2 = as.data.frame(as.data.frame(strsplit(temp2,"\n"))) 
      
      temp3 = which(grepl(" Rank ", temp2[,1]))
      if(length(temp3) == 0)
      {
        rank = as.character("-")
      }else
      {
        rank = as.character(temp2[temp3,])
        rank = strsplit(rank," Rank ")
        rank = rank[[1]]
        rank = as.numeric(rank[2])
      }
      
      
      temp4 = which(grepl("Coin", temp2[,1]))
      if(length(temp4) == 0)
      {
        coin_token = as.character("Token")
      }else
      {
        coin_token = as.character("Coin")
      }
      
      temp5 = which(grepl("Mineable", temp2[,1]))
      if(length(temp5) == 0)
      {
        Mineable_not = as.character("-")
      }else
      {
        Mineable_not = as.character("Mineable")
      }
      
      temp6 = which(grepl("Inactive", temp2[,1]))
      if(length(temp6) == 0)
      {
        Inactive = as.character("-")
      }else
      {
        Inactive = as.character("Inactive")
      }
      
      temp7 = which(grepl("Premined", temp2[,1]))
      if(length(temp7) == 0)
      {
        premined = as.character("-")
      }else
      {
        premined = as.character("Premined")
      }
    }else
    {
      temp1 = html_node(currency_page, xpath = xpath4)
      temp1 = html_text(temp1)
      temp1 = strsplit(temp1,"\n")
      temp1 = temp1[[1]]
      name = temp1[3]
      name = trimws(name, which = "left")
      symbol = temp1[2]
      symbol = trimws(symbol, which = 'left')
      symbol = gsub("\\(","",symbol)
      symbol = gsub("\\)","",symbol)
      
      temp2 = html_node(currency_page, xpath = xpath5)
      temp2 = html_text(temp2)
      temp2 = as.data.frame(as.data.frame(strsplit(temp2,"\n"))) 
      
      temp3 = which(grepl(" Rank ", temp2[,1]))
      if(length(temp3) == 0)
      {
        rank = as.character("-")
      }else
      {
        rank = as.character(temp2[temp3,])
        rank = strsplit(rank," Rank ")
        rank = rank[[1]]
        rank = as.numeric(rank[2])
      }
      
      
      temp4 = which(grepl("Coin", temp2[,1]))
      if(length(temp4) == 0)
      {
        coin_token = as.character("Token")
      }else
      {
        coin_token = as.character("Coin")
      }
      
      temp5 = which(grepl("Mineable", temp2[,1]))
      if(length(temp5) == 0)
      {
        Mineable_not = as.character("-")
      }else
      {
        Mineable_not = as.character("Mineable")
      }
      
      temp6 = which(grepl("Inactive", temp2[,1]))
      if(length(temp6) == 0)
      {
        Inactive = as.character("-")
      }else
      {
        Inactive = as.character("Inactive")
      }
      
      temp7 = which(grepl("Premined", temp2[,1]))
      if(length(temp7) == 0)
      {
        premined = as.character("-")
      }else
      {
        premined = as.character("Premined")
      }
      
    }
    
    historic_table["unique_name"] = links$unique_name[i]
    historic_table["currency_name"] = name
    historic_table["symbol"] = symbol
    historic_table["rank"] = rank
    historic_table["coin_token"] = coin_token
    historic_table["mineable"] = Mineable_not
    historic_table["premined"] = premined
    historic_table["inactive"] = Inactive
    historic_table["last_updated"] = last_updated
    master_data = rbind(master_data,historic_table)
    print(i)
  }
  
  master_data$Open = gsub(",","",master_data$Open)
  master_data$High = gsub(",","",master_data$High)
  master_data$Low = gsub(",","",master_data$Low)
  master_data$Close = gsub(",","",master_data$Close)
  master_data$Volume = gsub(",","",master_data$Volume)
  master_data$`Market Cap` = gsub(",","",master_data$`Market Cap`)
  
  master_data$Open = gsub("-","",master_data$Open)
  master_data$High = gsub("-","",master_data$High)
  master_data$Low = gsub("-","",master_data$Low)
  master_data$Close = gsub("-","",master_data$Close)
  master_data$Volume = gsub("-","",master_data$Volume)
  master_data$`Market Cap` = gsub("-","",master_data$`Market Cap`)
  
  
  master_data$Date = as.Date(master_data$Date, "%B%d,%Y")
  master_data$Open = as.numeric(master_data$Open)
  master_data$High = as.numeric(master_data$High)
  master_data$Low = as.numeric(master_data$Low)
  master_data$Close = as.numeric(master_data$Close)
  master_data$Volume = as.numeric(master_data$Volume)
  master_data$`Market Cap` = as.numeric(master_data$`Market Cap`)
  
  master_data = rbind(master,master_data)
  master_data = master_data[na.omit(master_data$Date),]
  
  return(master_data)
}


