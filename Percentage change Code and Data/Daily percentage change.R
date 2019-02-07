url1 = 'https://coinmarketcap.com/all/views/all/'
xpath1 = '//*[@id="currencies-all"]'

library("rvest")
snapshot = read_html(url1)
snapshot = html_node(snapshot, xpath = xpath1)
snapshot = html_table(snapshot)
colnames(snapshot)[1] = "Rank"

n<-50
required_currency<-snapshot[snapshot$Rank<=n,]

load("C:/Users/Jaideep Reddy/Desktop/INSOFE/Crypto Proj/functions/master_data.RData")

required_currency<-merge(required_currency,master_list,by.x = 'Name',by.y = 'initial_name')

days_500<-as.data.frame(table(master_data$unique_name))

days_500<-days_500[days_500$Freq>=500,]

required_currency<-required_currency[required_currency$new_name %in% days_500$Var1,]

required_currency<-required_currency[order(required_currency$Rank),]


rownames(required_currency)<-c(1:length(required_currency$new_name))
library(quantmod)
# converting to xts
finalxts<-list()
str(master_data)
for(i in 1:length(required_currency$new_name))
{
  OHLCV<-master_data[master_data$unique_name==required_currency$new_name[i],1:7]
  temp_Date<-OHLCV$Date
  OHLCV$Date<-NULL
  finalxts[[i]]<-as.xts(OHLCV,order.by =as.Date(temp_Date))
}
names(finalxts)<-required_currency$new_name
BitCoin<-finalxts[[1]]
View(finalxts[[1]])
length(finalxts)

library(rowr)
returns<-as.data.frame(finalxts[[1]]) 
returns["date"]<-rownames(returns)
returns<-as.data.frame(returns[,'date'])
colnames(returns)<-'date'


for(j in 1:length(finalxts))
{
  temp<-ClCl(finalxts[[j]])
  temp<-as.data.frame(temp)
  temp['date']<-rownames(temp)
  returns<-merge(returns,temp,by.x ='date',by.y ='date',all = T)
}
length(required_currency$new_name)
colnames(returns)[2:24]<-required_currency$new_name
str(returns)
returns$date<-as.Date(returns$date)

returns<-returns[rev(order(returns$date)),]

rownames(returns)<-1:length(returns$date)





# x<-returns[1:500,]
# rownames(x)<-x$date
# x$date<-NULL
# x$Augur<-NULL
# library(corrplot)
# corrplot(cor(x))


length(returns)
# plot(runCor(returns[1:500,2],returns[1:500,3],20),type="l")
super<-data.frame()
for (i in 2:(length(returns)-1))
{
  
  for (j in 3:length(returns)) 
    {
    if(i<j)
    {
    x=returns[1:500,i]
    y=returns[1:500,j]
    m<-sum(is.na(x))
    n<-sum(is.na(y))
    l<-500-max(m,n)
    corrs <- runCor(returns[1:l,i],returns[1:l,j],20)
    corrs<-na.omit(corrs)
    k <- adf.test(corrs,k=0)
    tempo<-data.frame(Currency_1=colnames(returns)[i],Currency_2=colnames(returns)[j],p_values=k$p.value)
    super<-rbind(super,tempo)
    }
  }
  
}

toporder<-super[order(super$p_values,decreasing = T),]
rownames(toporder)<-1:length(toporder$p_values)

corrs <- runCor(returns[1:500,'Factom'],returns[1:500,'Dogecoin'],20)
corrs<-na.omit(corrs)
plot(corrs,type="l")


