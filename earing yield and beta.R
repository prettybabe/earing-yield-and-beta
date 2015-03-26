setwd("D:\\working\\earing yield and beta")
options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)


startdate <- as.Date("2007-01-01")
enddate <- as.Date("2015-02-28")


data <- list()
data$channel <- dbConnect(SQLServer(), server = 'FILE', database = 'XY', user = 'libo.jin', password = 'Aa123123' )


sql <- "SELECT * FROM SecuMain WHERE SecuCategory = 1 AND SecuMarket IN (83,90) AND ListedDate IS NOT NULL 
        AND SecuCode NOT LIKE '9%'"
data$SecuMain <- dbGetQuery(data$channel, sql) %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr, ListedDate) %>%
  mutate(ListedDate = as.Date(ListedDate))

sql <- paste("SELECT * FROM QT_IndexQuote WHERE InnerCode = 1 AND TradingDay >='",
             startdate,"' AND TradingDay <='", enddate,"'ORDER BY InnerCode, TradingDay", sep = "")
data$Index <-  dbGetQuery(data$channel, sql) %>%
  mutate(TradingDay = as.Date(TradingDay)) %>%
  select(InnerCode, TradingDay, PrevClosePrice, ClosePrice)


sql <- paste("SELECT * FROM ReturnDaily WHERE TradingDay >='", startdate,"' AND TradingDay <='", enddate,"'", sep = "")
data$Stocks <- dbGetQuery(data$channel, sql) %>%
  mutate(DataDate = as.Date(DataDate), TradingDay = as.Date(TradingDay)) 


sql <- paste("SELECT SecuCode,DataDate,NetProfit FROM TTM_LC_IncomeStatementAll WHERE DataDate >='",
             startdate,"' AND DataDate <='", enddate,"'", sep = "")
data$NetProfit <- dbGetQuery(data$channel, sql) %>%
  mutate(DataDate = as.Date(ymd(DataDate))) 

sql <- paste("SELECT T1.STOCK_CODE, T1.CON_DATE, T1.RPT_DATE, T1.C5 AS PE FROM  CON_FORECAST AS T1
        INNER JOIN  QT_TradingDayNew AS T2 ON T1.CON_DATE = T2.TradingDate AND T2.IfMonthEnd = 1
        AND T2.SecuMarket = 83 AND T2.TradingDate >= '", startdate, "' AND T2.TradingDate <= '",
        enddate, "' AND T1.STOCK_TYPE = 1 AND T1.CON_TYPE != 0 ORDER BY T1.STOCK_CODE, T1.CON_DATE", sep = "")
data$PE <-  dbGetQuery(data$channel, sql) %>%
  group_by(CON_DATE, STOCK_CODE) %>%
  arrange(RPT_DATE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(CON_DATE = as.Date(CON_DATE), ep = 1/PE)  

dbDisconnect(channel)
#######################################################################################

freerate <- read.csv('Yield.csv', header = TRUE, sep = ",") %>%
  mutate(TradingDay = as.Date(ymd(TradingDay))) %>%
  filter(TradingDay >= startdate & TradingDay <= enddate) %>%
  mutate(yield_30y = (1+ Yield)^(1/252)-1)
           

returns <- list()
returns$Market <- data$Stocks %>%
  filter(!is.na(DailyReturn) & DailyReturn != 0 )  %>%
  group_by(TradingDay) %>%
  summarise(MarketReturn = sum(DailyReturn * FloatMarketCap, na.rm = TRUE)/sum(FloatMarketCap, na.rm = TRUE)) %>%
  left_join(freerate, by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalMarketReturn = MarketReturn - yield_30y) %>%
  select(TradingDay, MarketReturn, AbnormalMarketReturn) %>%
  ungroup()

 
returns$MarketIndex <- data$Index %>%
  group_by(TradingDay) %>%
  summarise(MarketIndexReturn = (ClosePrice/PrevClosePrice -1)) %>%
  left_join(freerate, by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalMarketIndexReturn = MarketIndexReturn - yield_30y) %>%
  select(TradingDay, MarketIndexReturn, AbnormalMarketIndexReturn) %>%
  ungroup() 


returns$Industry <- data$Stocks %>% 
  group_by(IndustryCodeNew, TradingDay) %>%
  filter(!is.na(DailyReturn) & DailyReturn != 0, !is.na(IndustryCodeNew))  %>%
  summarise(IndustryReturn = sum(DailyReturn * FloatMarketCap, na.rm = TRUE)/sum(FloatMarketCap, na.rm = TRUE),
            IndustryFloatMarketcap = sum(FloatMarketCap, na.rm = TRUE)) %>%
  left_join(freerate, by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormaiIndustryReturn = IndustryReturn - yield_30y) %>%
  select(IndustryCodeNew, TradingDay, IndustryReturn, IndustryFloatMarketcap, AbnormaiIndustryReturn) %>%
  ungroup() 

ep.industry <- list()
ep.industry$History <- data$Stocks %>% 
  filter(IfMonthEnd == 1, !is.na(IndustryCodeNew)) %>%
  select(SecuCode, TradingDay, FloatMarketCap, IndustryCodeNew) %>%
  left_join(data$NetProfit, by = c("SecuCode" = "SecuCode", "TradingDay" = "DataDate")) %>%
  group_by(IndustryCodeNew, TradingDay) %>%
  summarise(ep = sum(NetProfit, na.rm = TRUE)/sum(FloatMarketCap, na.rm = TRUE)) %>%
  ungroup()

ep.industry$Forecast <- data$PE %>%
  left_join(data$Stocks, by = c("STOCK_CODE" = "SecuCode", "CON_DATE" = "TradingDay")) %>%
  group_by(IndustryCodeNew, CON_DATE) %>%
  summarise(ep = sum(ep * FloatMarketCap/sum(FloatMarketCap, na.rm = TRUE), na.rm = TRUE)) %>%
  filter(!is.na(IndustryCodeNew)) %>%
  ungroup()



returns.monthly <- list()
returns.monthly$Market <- returns$Market %>%
  mutate(Year = year(TradingDay))

tradingdate <- data$Stocks %>%
  filter(IfMonthEnd == 1, TradingDay >= startdate,  TradingDay <= enddate) %>%
  select(TradingDay) %>%
  unique() %>%
  arrange(TradingDay)




regression.time <- data.frame(startdate = tradingdate[1:(nrow(tradingdate)-12), 1],
                              enddate = tradingdate[12:(nrow(tradingdate)-1), 1])
return.interval <- data.frame(startdate =  tradingdate[12:(nrow(tradingdate)-1), 1],
                              enddate = tradingdate[13:nrow(tradingdate), 1])




Beta <- function(x, y) coef(lm(x~y))[[2]]


beta <- data.frame()
corr <- data.frame()
for (i in c(1:nrow(regression.time))){
  beta.temp <- returns$industry %>%
    filter(TradingDay <= regression.time[i, 2] & TradingDay >= regression.time[i, 1])  %>%
    mutate(TradingDate = regression.time[i, 2]) %>%
    mutate(StartDate =  regression.time[i, 1]) %>%
    group_by(FirstIndustryCode, StartDate, TradingDate) %>%
    summarise(beta = Beta(abnormal.industry, abnormal.market)) %>%
    ungroup() %>%
    arrange(beta)
    
  corr.temp <- beta.temp %>%
    left_join(ep.industry, by = c("TradingDate" = "TradingDay", "FirstIndustryCode" = "FirstIndustryCode")) %>%
    group_by(TradingDate) %>%
    summarise(corr = cor(beta, ep))
  
  returns.industry.monthly.temp <- returns$industry  %>% 
    filter(TradingDay <= return.interval[i, 2] & TradingDay > return.interval[i, 1])  %>%
    mutate(StartDate = return.interval[i, 1]) %>%
    mutate(ReturnDate = return.interval[i, 2]) %>%
    group_by(FirstIndustryCode, StartDate, ReturnDate) %>%
    summarise(return.industry.monthly = exp(sum(log1p(return.industry)))-1,
              float.marketcap = last(industry.float.marketcap)) %>%
    ungroup()  
  
  beta <- rbind(beta, beta.temp)
  corr <- rbind(corr, corr.temp)
  returns$industry.monthly <- rbind(returns$industry.monthly, returns.industry.monthly.temp)  
}
  


threshold <- 0.5
trade.industry.number <- 1
half.life <- 0.5
ts.score <- 6


for(i in c(1:nrow(corr))){
  if(i == 1) corr$smooth[1] <- corr$corr[1]
  else corr$smooth[i] <- corr$corr[i] + half.life * corr$smooth[i-1]
  
  if(i < ts.score)  corr$score[i] <- 0
  else corr$score[i] <- (corr$smooth[i] - mean(corr$smooth[(i-ts.score +1) : i]))/ sd(corr$smooth[(i-ts.score +1) : i])
}

corr <- corr %>%
  slice(-c(1:(ts.score-1)))


data.final <- corr %>%
  left_join(beta, by = c("TradingDate" = "TradingDate")) %>%
  left_join(returns$industry.monthly, by = c("FirstIndustryCode" = "FirstIndustryCode",
                                             "TradingDate" = "StartDate")) %>%
  mutate(corr.threshold = threshold)  %>%
  mutate(siginal = ifelse(score >= corr.threshold, 1, ifelse(score <= -corr.threshold, -1, 0))) %>%
  group_by(TradingDate) %>%
  mutate(order = ifelse(row_number(beta*siginal) %in% c(1:7), 1,
                        ifelse(row_number(beta*siginal) %in% c(8:14), 2,
                               ifelse(row_number(beta*siginal) %in% c(15:21), 3, 4)))) %>%
  mutate(order = as.factor(order)) %>%
  group_by(TradingDate, ReturnDate, order) %>%
  summarise(average.return = sum(return.industry.monthly*float.marketcap)/sum(float.marketcap)) %>%
  group_by(order) %>%
  arrange(TradingDate) %>%
  mutate(return.industry.cumulate = exp(cumsum(log1p(average.return))) - 1) %>% 
  ungroup()


qplot(ReturnDate, return.industry.cumulate, data = data.final, colour = order, geom = "line")

aa <- corr %>%
  left_join(beta, by = c("TradingDate" = "TradingDate")) %>%
  left_join(returns$industry.monthly, by = c("FirstIndustryCode" = "FirstIndustryCode",
                                             "TradingDate" = "StartDate")) %>%
  group_by(score, ReturnDate) %>%
  summarise(returns = sum(return.industry.monthly*float.marketcap)/sum(float.marketcap))
  summary(lm(aa$returns~aa$score))




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




