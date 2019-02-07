currency_info = function(currency_name, snapshot)
{
  if(!is.null(snapshot))
  {
    result = snapshot[snapshot$unique_name == currency_name,]
    #return(result)
  }else
  {
    print("Output is a list of two objects, use output[[1]] for currency_info")
    print("By using snapshot = output[[2]], fetch the info of other currency faster ")
    
    library("rvest")
    library("RCurl")
    library("XML")
    
    
    url1 = 'https://coinmarketcap.com/all/views/all/'
    xpath1 = '//*[@id="currencies-all"]'
    
    snapshot = read_html(url1)
    url2<-getURL(url1)
    
    snapshot = html_node(snapshot, xpath = xpath1)
    snapshot = html_table(snapshot)
    
    parsed<-htmlParse(url2)
    links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
    
    colnames(snapshot)[1] = "Rank"
    
    snapshot$`Market Cap` = gsub("\\$","",snapshot$`Market Cap`)
    snapshot$`Market Cap` = gsub("\\,","",snapshot$`Market Cap`)
    snapshot$`Market Cap` = as.numeric(snapshot$`Market Cap`)
    
    snapshot$Price = gsub("\\$","",snapshot$Price)
    snapshot$Price = gsub("\\,","",snapshot$Price)
    snapshot$Price = as.numeric(snapshot$Price)
    
    snapshot$`Circulating Supply` = gsub("\\,","",snapshot$`Circulating Supply`)
    snapshot$`Circulating Supply` = gsub("\\*","",snapshot$`Circulating Supply`)
    snapshot$`Circulating Supply` = gsub("\\?","",snapshot$`Circulating Supply`)
    snapshot$`Circulating Supply` = as.numeric(snapshot$`Circulating Supply`)
    
    snapshot$`Volume (24h)` = gsub("\\$","",snapshot$`Volume (24h)`)
    snapshot$`Volume (24h)` = gsub("\\,","",snapshot$`Volume (24h)`)
    snapshot$`Volume (24h)` = gsub("\\?","",snapshot$`Volume (24h)`)
    snapshot$`Volume (24h)` = gsub("\\Low Vol","",snapshot$`Volume (24h)`)
    snapshot$`Volume (24h)` = as.numeric(snapshot$`Volume (24h)`)
    
    snapshot$`% 1h` = gsub("\\%","",snapshot$`% 1h`)
    snapshot$`% 1h` = gsub("\\?","",snapshot$`% 1h`)
    snapshot$`% 1h` = as.numeric(snapshot$`% 1h`)
    
    snapshot$`% 24h` = gsub("\\%","",snapshot$`% 24h`)
    snapshot$`% 24h` = gsub("\\?","",snapshot$`% 24h`)
    snapshot$`% 24h` = as.numeric(snapshot$`% 24h`)
    
    snapshot$`% 7d` = gsub("\\%","",snapshot$`% 7d`)
    snapshot$`% 7d` = gsub("\\?","",snapshot$`% 7d`)
    snapshot$`% 7d` = as.numeric(snapshot$`% 7d`)
    
    links = do.call(rbind.data.frame, links)
    links = as.data.frame(links[!duplicated(links[,1]),])
    links = as.data.frame(links[grepl("/currencies/",links[,1]),])
    links = as.data.frame(links[!grepl("#markets",links[,1]),])
    links = as.data.frame(links[!grepl("https://",links[,1]),])
    library(stringr)
    links["count"] = str_count(links[,1],"/")
    links = links[links$count == 3,]
    
    links[,1] = gsub("\\/currencies/","",links[,1])
    links[,1] = gsub("\\/","",links[,1])
    colnames(links)[1] = "unique_name"
    links[,2] = NULL
    
    snapshot["unique_name"] = links$unique_name
    snapshot["last_updated"] = Sys.time()
    result = snapshot[snapshot$unique_name == currency_name,]
    temp = list(result,snapshot)
    #return(temp)
  }
}