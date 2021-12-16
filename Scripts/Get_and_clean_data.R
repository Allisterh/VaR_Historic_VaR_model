# set working directory to project directory
# data must be downloaded manually from yahoo finance (don't know how to submit html forms yet)
# downloaded historic prices on SAN.MC ; DBK.DE ; MSFT
require(readr)
require(tidyverse)

SAN_link<- "https://finance.yahoo.com/quote/SAN.MC/history?period1=1481760000&period2=1639526400&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true"
DBK_link<- "https://finance.yahoo.com/quote/DBK.DE/history?period1=1481760000&period2=1639526400&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true"
MSFT_link<- "https://finance.yahoo.com/quote/MSFT/history?period1=1481760000&period2=1639440000&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true"

#vector of relative paths to datasets
yahoo_finance<- c("Data/Raw/SAN.MC.csv","Data/Raw/DBK.DE.csv","Data/Raw/MSFT.csv")
#import the datasets
stocks_raw<- lapply(yahoo_finance, function(X){
  a<- read_csv(X)
} )
#set new tibble names and column names
names(stocks_raw) = c("SAN","DBK","MSFT")
stocks_raw<- lapply(stocks_raw, function(x){
  setNames(x, c("Fecha","Apertura","Alto","Bajo","Cierre","Adj_Cierre","volumen"))
})
#changing SAN dataset to numeric, replacing missing value SAN$Adj_Cierre[451] 
missing_value<- which(is.na(as.numeric(stocks_raw$SAN$Adj_Cierre))==TRUE)
y2<- as.numeric(stocks_raw$SAN$Adj_Cierre[missing_value+1])
y1<- as.numeric(stocks_raw$SAN$Adj_Cierre[missing_value-1])
# missing value was linearly interpolated 
linear_interpolation<- y1+((y2-y1)/(3-1))*(2-1)
#replace missing value in SAN.MC
SAN<- stocks_raw$SAN %>%
  mutate(Adj_Cierre=as.numeric(Adj_Cierre))%>%
  mutate(Adj_Cierre=replace(.$Adj_Cierre,missing_value,linear_interpolation))
stocks_numeric<- list(SAN,stocks_raw$DBK,stocks_raw$MSFT)
names(stocks_numeric) = c("SAN","DBK","MSFT")

#calculate log returns for every dataset within the list
stocks_returns<- lapply(stocks_numeric, function(x){
  a<- mutate(x, new_col = lag(Adj_Cierre))
  b<- slice(a, -1)
  c<- mutate(b,log_rend=log(Adj_Cierre/new_col)) #get log returns
  d<- select(c,c("Fecha","log_rend"))
  e<- mutate(d,normal = rnorm(nrow(d), mean(d$log_rend), sd(d$log_rend))) #adds col with normally distributed returns(for plotting)
})
# tidy format datasets 
SAN.MC<- stocks_returns$SAN
DBK.DE<- stocks_returns$DBK
MSFT<- stocks_returns$MSFT
#save as rds
saveRDS(SAN.MC, "Data/Tidy/SAN_MC_tidy.rds")
saveRDS(DBK.DE, "Data/Tidy/DBK_DE_tidy.rds")
saveRDS(MSFT, "Data/Tidy/MSFT_tidy.rds")
#save as csv
write_csv(SAN.MC, "Data/Tidy/SAN_MC_tidy.csv")
write_csv(DBK.DE, "Data/Tidy/DBK_DE_tidy.csv")
write_csv(MSFT, "Data/Tidy/MSFT_tidy.csv")

