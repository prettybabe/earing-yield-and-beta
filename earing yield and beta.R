setwd("D:\\working\\earing yield and beta")
options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

file_server <- list()
file_server$channel  <- src_sqlserver(server="file", database="XY", user="libo.jin",password="Aa123123")
channel <- dbConnect(SQLServer(), server = 'FILE', database = 'XY', user = 'libo.jin', password = 'Aa123123' )
#########################################################################################
#   下载需要的表

file_server$SecuMain <- tbl(file_server$channel, "SecuMain") %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr, SecuMarket, SecuCategory, ListedDate) %>%
  collect %>%
  filter(SecuCategory == 1,  SecuMarket == 83 | SecuMarket == 90, InnerCode != 307,
                    !is.na(ListedDate), !grepl("^9.*", SecuCode)) %>%
  mutate(ListedDate = as.Date(ListedDate)) %>%
  arrange(SecuCode) 


sql <- "Select T1.InnerCode, 
        T1.CompanyCode,
        T1.SecuCode, 
        T1.SecuAbbr,
        T2.TradingDay,
        T2.PrevClosePrice, T2.OpenPrice, 
        T2.HighPrice, 
        T2.LowPrice, 
        T2.ClosePrice,
        T2.TurnoverVolume, 
        T2.TurnoverValue,
        T3.NonRestrictedShares,
        T4.FirstIndustryCode,
        T4.FirstIndustryName,
        T4.FirstIndustryCodeNew,
        T4.FirstIndustryNameNew,
        T5.NetProfit,
        T6.IfMonthEnd,
        T6.IfQuarterEnd,
        T6.IfTradingDay,
        T6.IfWeekEnd,
        T6.IfYearEnd
        from SecuMain AS T1
        inner join QT_DailyQuote AS T2
        ON T1.SecuCategory = 1
        AND T1.SecuMarket IN (83, 90)
        AND T1.ListedDate IS NOT NULL
        AND T1.SecuCode NOT LIKE '9%'
        AND T1.InnerCode = T2.InnerCode
        left join LC_ShareStru AS T3
        ON T1.CompanyCode = T3.CompanyCode
        AND   T3.EndDate = (SELECT MAX(EndDate) FROM LC_ShareStru WHERE CompanyCode = T1.CompanyCode AND InfoPublDate <= T2.TradingDay
        AND EndDate <= T2.TradingDay)
        LEFT JOIN LC_ExgIndustry201510 AS T4
        ON T1.CompanyCode = T4.CompanyCode
        AND T4.InfoPublDate = (SELECT MAX(InfoPublDate) FROM LC_ExgIndustry201510 WHERE CompanyCode = T1.CompanyCode AND InfoPublDate <= T2.TradingDay)
        LEFT JOIN TTM_LC_IncomeStatementAll  AS T5
        ON T5.SecuCode = T1.SecuCode
        AND T5.DataDate = T2.TradingDay
        LEFT JOIN QT_TradingDayNew AS T6
        ON T2.TradingDay = T6.TradingDate
        AND T6.SecuMarket = 83
        ORDER BY T1.InnerCode, T2.TradingDay"
file_server$QT_DailyQuote <- dbGetQuery(channel, sql) %>%
  mutate(TradingDay = as.Date(TradingDay)) 
  

file_server$NetProfit <- tbl(file_server$channel, "TTM_LC_IncomeStatementAll") %>%
  select(DataDate, SecuCode, NetProfit) %>%
  collect %>%
  mutate(DataDate = as.Date(ymd(DataDate)))
  

file_server$Shares <- tbl(file_server$channel, "LC_ShareStru") %>%
  select(CompanyCode, EndDate, InfoPublDate, NonRestrictedShares) %>%
  collect %>%
  mutate(EndDate = as.Date(EndDate), InfoPublDate = as.Date(InfoPublDate))
  
file_server$TradingDay <- tbl(file_server$channel, "QT_TradingDayNew") %>%
  filter(SecuMarket == 83) %>%
  select(-ID, -SecuMarket, -XGRQ, -JSID) %>%
  collect %>%
  mutate(TradingDate = as.Date(TradingDate)) 
  

dbDisconnect(channel, file_server$channel)
#######################################################################################

#  选取符合条件的股票

startdate <- as.Date("2007-01-01")
enddate <- as.Date("2015-02-28")

raw.data <- file_server$QT_DailyQuote %>%
  filter(TradingDay >= startdate & TradingDay <= enddate) %>%    
  left_join(file_server$TradingDay, by = c("TradingDay" = "TradingDate")) %>%
  left_join(file_server$NetProfit, by = c("SecuCode" = "SecuCode", "TradingDay" = "DataDate"))  


  



index <- filter(index , TradingDay >= startdate & TradingDay <= enddate)
index$TradingDay <- ymd(as.Date(index$TradingDay))
index <- mutate(index, Returns = ClosePrice/PrevClosePrice-1)
                        


freerate <- read.csv('Yield.csv', header = TRUE, sep = ",")
freerate$DataDate <- ymd(freerate$DataDate)
freerate <- select(freerate, DataDate, yield_30y)
freerate <- filter(freerate, DataDate >= startdate & DataDate <= enddate)
freerate$yield_30y <- (1+freerate$yield_30y)^(1/252)-1



data$DataDate <- ymd(as.Date(data$DataDate))
data.stocks <- filter(data, DataDate >= startdate & DataDate <= enddate, SecuCode %in% data.secu$SecuCode,
                      IfTradingDay == 1, !is.na(IndustryCode))

return.industry <- data.stocks %>%
  group_by(IndustryCode, DataDate) %>%
  summarise(return.industry = sum(DailyReturn* NetProfit, na.rm = TRUE)/sum(NetProfit, na.rm = TRUE)) %>%
  left_join(freerate, by = NULL) %>%
  mutate(abnormal.industry = return.industry - yield_30y)


return.market <- data.stocks %>%
  group_by(DataDate) %>%
  summarise(return.market = sum(DailyReturn * NetProfit, na.rm = TRUE)/sum(NetProfit, na.rm = TRUE)) %>%
  left_join(freerate, by = NULL) %>%
  mutate(abnormal.market = return.market - yield_30y)


return.data <- left_join(return.industry, return.market, by = NULL)


ep.industry <- data.stocks %>%
  group_by(IndustryCode, DataDate) %>%
  summarise(ep = sum(NetProfit, na.rm = TRUE)/sum(FloatMarketCap, na.rm = TRUE))



tradingdate <- data.stocks %>%
  filter(IfMonthEnd == 1, DataDate >= startdate) %>%
  select(DataDate) %>%
  unique()

regression.time <- data.frame(startdate = tradingdate[1:(nrow(tradingdate)-12), 1],
                              enddate = tradingdate[12:(nrow(tradingdate)-1), 1])
return.interval <- data.frame(startdate =  tradingdate[12:(nrow(tradingdate)-1), 1],
                              enddate = tradingdate[13:nrow(tradingdate), 1])

Beta <- function(x, y) coef(lm(x~y))[[2]]

beta.threshold <- 0
trade.industry.number <- 1


return.trading <- data.frame(NULL)
for (i in c(1:nrow(regression.time))){
  beta.industry <- return.data %>%
    filter(DataDate <= regression.time[i, 2] & DataDate >= regression.time[i, 1])  %>%
    mutate(DataDate = regression.time[i, 2]) %>%
    mutate(StartDate = regression.time[i, 1]) %>%
    group_by(IndustryCode, StartDate, DataDate) %>%
    summarise(beta = Beta(abnormal.industry, abnormal.market)) %>%
    ungroup() %>%
    arrange(beta)
    
   
  corr.industry <- beta.industry %>%
    left_join(ep.industry, by = NULL) %>%
    group_by(DataDate) %>%
    summarise(corr = cor(beta, ep)) 
    
  
  if(corr.industry$corr > beta.threshold){
    trading.industry <- beta.industry %>%
      ungroup() %>%
      arrange(desc(beta)) %>%
      slice(1:trade.industry.number) %>%
      select(IndustryCode) 
  }else{
    trading.industry <- beta.industry %>%
      ungroup() %>%
      arrange(beta) %>%
      slice(1:trade.industry.number) %>%
      select(IndustryCode) 
  }
    
  
    
  return.industry.trading <- return.industry %>%
    filter(DataDate <= return.interval[i, 2] & DataDate > return.interval[i, 1]) 
  
  
  return.market.trading <- index %>%
    filter(TradingDay <= return.interval[i, 2] & TradingDay  > return.interval[i, 1]) 
   
  return.trading.temp <- return.industry.trading %>%
    filter(IndustryCode %in% trading.industry$IndustryCode) %>%
    group_by(IndustryCode) %>%
    summarise(return.industry = exp(sum(log1p(return.industry)))-1) %>%
    summarise(return.industry = sum(return.industry))  %>%
    mutate(return.market = exp(sum(log1p(return.market.trading$Returns)))-1)
  
  return.trading.temp <- cbind(return.interval[i, ], return.trading.temp)
  return.trading <- rbind(return.trading, return.trading.temp)
}

return.trading <- return.trading %>%
  mutate(return.industry.cumulate = exp(cumsum(log1p(return.industry))) - 1) %>%
  mutate(return.market.cumulate = exp(cumsum(log1p(return.market)))-1)

return.trading.cumulate <- select(return.trading, enddate, return.industry.cumulate, return.market.cumulate)
names(return.trading.cumulate) <- c("Date", "industry", "market")
return.trading.cumulate <- melt(return.trading.cumulate, id=c("Date"))
names(return.trading.cumulate) <- c("Date", "type", "CumulateReturn")
qplot(Date, CumulateReturn, data = return.trading.cumulate, colour = type, geom = "line")





MaxDropdown <- function(date, return.simple, type){
  maxdropdown.value <- 0
  maxdropdown.time <- today()
  return.cumulate <- cumsum(log1p(return.simple))
  if(maxdropdown.value > return.cumulate[1]){
    maxdropdown.value <- return.cumulate[1]
    maxdropdown.time <- date[1]
  }
  
  if(length(return.cumulate)>=2)
    for(i in 2:length(return.cumulate)){
      maxdropdown.temp <- return.cumulate[i]- max(return.cumulate[1:i])
      if(maxdropdown.temp < maxdropdown.value){
        maxdropdown.value <- maxdropdown.temp
        maxdropdown.time <- date[i]
      }
    }
  
  maxdropdown.value <- exp(maxdropdown.value) - 1
  ifelse(type == "value", return(maxdropdown.value), return(maxdropdown.time))
}



return.trading.annul<- return.trading %>%
  mutate(year = year(enddate)) %>%
  select(year, enddate, return.industry) %>%
  group_by(year) %>%
  summarise(maxdropdown.time = MaxDropdown(enddate, return.industry, type = "time"),
            maxdropdown.value =  MaxDropdown(enddate, return.industry, type = "value"),
            return.annualize = exp(mean(log1p(return.industry))*12) - 1,
            std.annualize = sd(return.industry)*sqrt(12)) %>%
  mutate(IR = return.annualize / std.annualize)




