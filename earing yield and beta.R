
options(java.parameters="-Xmx4g")


library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
channel <- dbConnect(SQLServer(), server = 'FILE', database = 'XY', user = 'libo.jin', password = 'Aa123123' )


# ɸ
setwd("D:\\working\\earing yield and beta")
SecuMain <- dbReadTable(channel, "SecuMain")

data.secu <- filter(SecuMain, SecuCategory==1,  SecuMarket == 83 | SecuMarket == 90, InnerCode != 307,
                    !is.na(ListedDate), !grepl("^9.*", SecuCode))
data.secu <- select(data.secu, InnerCode, CompanyCode, SecuCode, SecuAbbr, SecuMarket, ListedDate)
data.secu$ListedDate <- as.Date(data.secu$ListedDate)
data.secu <- arrange(data.secu, SecuCode)


sql <-  "SELECT T1.DataDate,
         T1.IfTradingDay,
         T1.IfWeekEnd,
         T1.IfMonthEnd,
         T1.IfQuarterEnd,
         T1.IfYearEnd,
         T1.IfSpecialTrade,
         T1.IfSuspended,
         T1.SecuCode,
         T1.SecuAbbr,
         T1.IndustryCode,
         T1.IndustryName,
         T1.FloatMarketCap,
         T1.DailyReturn,T2.NetProfit FROM
         ReturnDaily T1 
         left join TTM_LC_IncomeStatementAll T2
         ON T1.SecuCode = T2.SecuCode
         AND CONVERT(CHAR(8),T1.DataDate,112) = T2.DataDate
         order by T1.SecuCode,T1.DataDate"
data <- dbGetQuery(channel, sql)


sql <- "SELECT TradingDay, ClosePrice, PrevClosePrice 
        FROM QT_IndexQuote WHERE InnerCode=1 ORDER BY TradingDay"
index <- dbGetQuery(channel, sql)

 
dbDisconnect(channel)


startdate <- ymd("2007-01-01")
enddate <- ymd("2015-02-28")
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
  
  for(i in 2:nrow(return.cumulate)){
    maxdropdown.temp <- return.cumulate[i]- max(return.cumulate[1:i])
    if(maxdropdown.temp < maxdropdown.value){
      maxdropdown.value <- maxdropdown.temp
      maxdropdown.time <- date[i]
    }
  }

  maxdropdown.value <- exp(maxdropdown.value) - 1
  ifelse(type == "value", return(maxdropdown.value), return(maxdropdown.time))
}


AnnualizeReturn <- function(returns, period){
  return.annualize <- exp(mean(log1p(returns))*period) - 1
}    

AnnualizeStd <- function(returns, period){
  return.annualize <- sd(returns)*sqrt(period)
}    


return.trading.annul<- return.trading %>%
  mutate(year = year(enddate)) %>%
  select(year, enddate, return.industry) %>%
  group_by(year) %>%
  summarise(maxdropdown.time = MaxDropdown(enddate, return.industry, type = "time"),
            maxdropdown.value =  MaxDropdown(enddate, return.industry, type = "value"),
            return.annualize = AnnualizeReturn,
            return.annualize = AnnualizeStd) %>%
  mutate(IR = return.annualize / return.annualize)




