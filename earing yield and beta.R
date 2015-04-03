setwd("D:\\working\\earing yield and beta")
options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(TTR)


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
  mutate(CON_DATE = as.Date(CON_DATE), ep = 1/PE)  %>%
  rename(DataDate = CON_DATE) 

dbDisconnect(channel)
#######################################################################################

freerate <- read.csv('Yield.csv', header = TRUE, sep = ",") %>%
  mutate(TradingDay = as.Date(ymd(TradingDay))) %>%
  filter(TradingDay >= startdate & TradingDay <= enddate) %>%
  mutate(yield_30y = (1+ Yield)^(1/252)-1)
           

returns <- list()
returns$Market <- data$Stocks %>%
  filter(IfTradingDay == 1, DailyReturn > 0.11, !grepl("ST", SecuAbbr)) %>%
  group_by(TradingDay) %>%
  summarise(MarketReturn = weighted.mean(DailyReturn, FloatMarketCap, na.rm = TRUE)) %>%
  left_join(freerate, by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalMarketReturn = MarketReturn - yield_30y) %>%
  select(TradingDay, MarketReturn, AbnormalMarketReturn) %>%
  ungroup()
 
returns$MarketIndex <- data$Index %>%
  group_by(TradingDay) %>%
  summarise(MarketIndexReturn = (ClosePrice/PrevClosePrice - 1)) %>%
  left_join(freerate, by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalMarketIndexReturn = MarketIndexReturn - yield_30y) %>%
  select(TradingDay, MarketIndexReturn, AbnormalMarketIndexReturn) %>%
  ungroup() 


returns$Industry <- data$Stocks %>%
  filter(IfTradingDay == 1, !is.na(IndustryCodeNew), DailyReturn > 0.11, !grepl("ST", SecuAbbr)) %>%
  group_by(TradingDay, IndustryCodeNew) %>%
  summarise(IndustryReturn = weighted.mean(DailyReturn, FloatMarketCap, na.rm = TRUE),
            IndustryFloatMarketCap = sum(FloatMarketCap, na.rm = TRUE)) %>%
  left_join(freerate, by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalIndustryReturn = IndustryReturn - yield_30y) %>%
  select(TradingDay, IndustryCodeNew, IndustryReturn, AbnormalIndustryReturn, IndustryFloatMarketCap) %>%
  ungroup()
 

ep.industry <- list()
ep.industry$History <- data$Stocks %>% 
  filter(IfMonthEnd == 1, !is.na(IndustryCodeNew), DailyReturn > 0.11, !grepl("ST", SecuAbbr)) %>%
  select(SecuCode, TradingDay, FloatMarketCap, IndustryCodeNew) %>%
  left_join(data$NetProfit, by = c("SecuCode" = "SecuCode", "TradingDay" = "DataDate")) %>%
  group_by(IndustryCodeNew, TradingDay) %>%
  summarise(ep = sum(NetProfit, na.rm = TRUE)/sum(FloatMarketCap, na.rm = TRUE)) %>%
  ungroup() 
 
ep.industry$Forecast <- data$PE %>%
  left_join(data$Stocks, by = c("STOCK_CODE" = "SecuCode", "DataDate" = "DataDate")) %>%
  filter(!is.na(IndustryCodeNew), DailyReturn > 0.11, !grepl("ST", SecuAbbr)) %>%
  group_by(IndustryCodeNew, DataDate) %>%
  summarise(ep = weighted.mean(ep, FloatMarketCap, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(TradingDay = DataDate)

###################################################################################################

returns.monthly <- list()
returns.monthly$Market <- returns$Market %>%
  mutate(Month = format(TradingDay,  "%Y%m")) %>%
  group_by(Month) %>%
  mutate(TradingDay = last(TradingDay)) %>%
  group_by(TradingDay) %>%
  summarise(MarketMonthlyReturn = exp(sum(log1p(MarketReturn))) - 1,
            MarketAbnormalMonthlyReturn = exp(sum(log1p(AbnormalMarketReturn))) - 1)

returns.monthly$MarketIndex <- returns$MarketIndex %>%
  mutate(Month = format(TradingDay,  "%Y%m")) %>%
  group_by(Month) %>%
  mutate(TradingDay = last(TradingDay)) %>%
  group_by(TradingDay) %>%
  summarise(MarketIndexMonthlyReturn = exp(sum(log1p(MarketIndexReturn))) - 1,
            MarketIndexAbnormalMonthlyReturn = exp(sum(log1p(AbnormalMarketIndexReturn))) - 1)

returns.monthly$Industry <- returns$Industry %>%
  mutate(Month = format(TradingDay,  "%Y%m")) %>%
  group_by(Month, IndustryCodeNew) %>%
  arrange(TradingDay) %>%
  mutate(TradingDay = last(TradingDay), IndustryFloatMarketCap = last(IndustryFloatMarketCap)) %>%
  group_by(TradingDay, IndustryCodeNew, IndustryFloatMarketCap) %>%
  summarise(IndustryMonthlyReturn = exp(sum(log1p(IndustryReturn))) - 1,
            IndustryAbnormalMonthlyReturn = exp(sum(log1p(AbnormalIndustryReturn))) - 1) 


##############################################################################################
# 月度数据beta
tradingdate <- data$Stocks %>%
  filter(IfMonthEnd == 1, TradingDay >= startdate,  TradingDay <= enddate) %>%
  select(TradingDay) %>%
  distinct() %>%
  arrange(TradingDay) 


regression.time.monthly <- data.frame(startdate = tradingdate[1:(nrow(tradingdate) - 24), 1],
                              enddate = tradingdate[24:(nrow(tradingdate)-1), 1])

return.interval.monthly <- data.frame(startdate =  tradingdate[24:(nrow(tradingdate)-1), 1],
                              enddate = tradingdate[25:nrow(tradingdate), 1])

Beta <- function(x, y) coef(lm(x~y))[[2]]

beta.monthly <- data.frame()
corr.monthly <- data.frame()
for (i in c(1:nrow(regression.time.monthly))){
  beta.temp <- returns.monthly$Industry %>%
    group_by(IndustryCodeNew) %>%
    filter(TradingDay <= regression.time.monthly[i, 2] & TradingDay >= regression.time.monthly[i, 1])  %>%
    mutate(RegressionTime = regression.time.monthly[i, 2]) %>%
    group_by(IndustryCodeNew, RegressionTime) %>%
    left_join(returns.monthly$MarketIndex, by = c("TradingDay" = "TradingDay")) %>%
    summarise(beta = Beta(IndustryAbnormalMonthlyReturn, MarketIndexAbnormalMonthlyReturn)) %>%
    ungroup() 
  
  corr.temp <- beta.temp %>%
    left_join(ep.industry$Forecast, by = c("RegressionTime" = "TradingDay", "IndustryCodeNew" = "IndustryCodeNew")) %>%
    group_by(RegressionTime) %>%
    summarise(corr = cor(beta, ep))
  
  beta.monthly <- rbind(beta.monthly, beta.temp)
  corr.monthly <- rbind(corr.monthly, corr.temp)
    
}

threshold <- 0
ts.score <- 6
kSmooth <- 3
corr.monthly$smooth <- EMA(corr.monthly$corr, kSmooth)
for(i in c(1:nrow(corr.monthly))){
  if(i < ts.score)  corr.monthly$score[i] <- NA
  else corr.monthly$score[i] <- (corr.monthly$smooth[i] - mean(corr.monthly$smooth[(i-ts.score +1) : i]))/ sd(corr.monthly$smooth[(i-ts.score +1) : i])
}


data.final <- corr.monthly %>%
  select(RegressionTime, corr) %>%
  filter(!is.na(corr)) %>%
  left_join(beta.monthly, by = c("RegressionTime" = "RegressionTime")) %>%
  left_join(return.interval.monthly, by = c("RegressionTime" = "startdate")) %>%
  left_join(returns.monthly$Industry, by = c("IndustryCodeNew" = "IndustryCodeNew",
                                             "enddate" = "TradingDay")) %>%
  mutate(corr.threshold = threshold)  %>%
  mutate(siginal = ifelse(corr >= corr.threshold, 1, ifelse(corr <= -corr.threshold, -1, 0))) %>%
  group_by(enddate) %>%
  mutate(order = ifelse(row_number(beta*siginal) %in% c(1:5), "first",
                        ifelse(row_number(beta*siginal) %in% c(6:10), "second",
                               ifelse(row_number(beta*siginal) %in% c(11:15), "third", 
                                      ifelse(row_number(beta*siginal) %in% c(15:20), "forth", "fifth"))))) %>%
  group_by(enddate, order) %>%
  summarise(average.return = weighted.mean(IndustryMonthlyReturn, IndustryFloatMarketCap, na.rm = TRUE)) %>%
  group_by(order) %>%
  arrange(enddate) %>%
  mutate(return.industry.cumulate = exp(cumsum(log1p(average.return))) - 1) %>% 
  ungroup()


qplot(enddate, return.industry.cumulate, data = data.final, color = order, geom = "line")

##############################################################################################
# 日度数据beta
tradingdate <- data$Stocks %>%
  filter(IfMonthEnd == 1, TradingDay >= startdate,  TradingDay <= enddate) %>%
  select(TradingDay) %>%
  distinct() %>%
  arrange(TradingDay) 


regression.time.monthly <- data.frame(startdate = c(startdate,tradingdate[1:(nrow(tradingdate) - 13), 1]),
                                      enddate = tradingdate[12:(nrow(tradingdate)-1), 1])

return.interval.monthly <- data.frame(startdate =  tradingdate[12:(nrow(tradingdate)-1), 1],
                                      enddate = tradingdate[13:nrow(tradingdate), 1])

Beta <- function(x, y) coef(lm(x~y))[[2]]

beta.monthly <- data.frame()
corr.monthly <- data.frame()
for (i in c(1:nrow(regression.time.monthly))){
  beta.temp <- returns$Industry %>%
    group_by(IndustryCodeNew) %>%
    filter(TradingDay <= regression.time.monthly[i, 2] & TradingDay > regression.time.monthly[i, 1])  %>%
    mutate(RegressionTime = regression.time.monthly[i, 2]) %>%
    group_by(IndustryCodeNew, RegressionTime) %>%
    left_join(returns$MarketIndex, by = c("TradingDay" = "TradingDay")) %>%
    summarise(beta = Beta(AbnormalIndustryReturn, AbnormalMarketIndexReturn)) %>%
    ungroup() 
  
  corr.temp <- beta.temp %>%
    left_join(ep.industry$Forecast, by = c("RegressionTime" = "TradingDay", "IndustryCodeNew" = "IndustryCodeNew")) %>%
    group_by(RegressionTime) %>%
    summarise(corr = cor(beta, ep))
  
  beta.monthly <- rbind(beta.monthly, beta.temp)
  corr.monthly <- rbind(corr.monthly, corr.temp)
  
}

threshold <- 0
ts.score <- 3
kSmooth <- 2
corr.monthly$smooth <- EMA(corr.monthly$corr, kSmooth)
for(i in c(1:nrow(corr.monthly))){
  if(i < ts.score)  corr.monthly$score[i] <- NA
  else corr.monthly$score[i] <- (corr.monthly$smooth[i] - mean(corr.monthly$smooth[(i-ts.score +1) : i]))/ sd(corr.monthly$smooth[(i-ts.score +1) : i])
}


data.final <- corr.monthly %>%
  select(RegressionTime, score) %>%
  filter(!is.na(score)) %>%
  left_join(beta.monthly, by = c("RegressionTime" = "RegressionTime")) %>%
  left_join(return.interval.monthly, by = c("RegressionTime" = "startdate")) %>%
  left_join(returns.monthly$Industry, by = c("IndustryCodeNew" = "IndustryCodeNew",
                                             "enddate" = "TradingDay")) %>%
  mutate(corr.threshold = threshold)  %>%
  mutate(siginal = ifelse(score >= corr.threshold, 1, ifelse(score <= -corr.threshold, -1, 0))) %>%
  group_by(enddate) %>%
  mutate(order = ifelse(row_number(beta*siginal) %in% c(1:6), "first",
                        ifelse(row_number(beta*siginal) %in% c(7:12), "second",
                               ifelse(row_number(beta*siginal) %in% c(13:18), "third", 
                                      ifelse(row_number(beta*siginal) %in% c(19:24), "forth", "fifth"))))) %>%
  group_by(enddate, order) %>%
  summarise(average.return = weighted.mean(IndustryMonthlyReturn, IndustryFloatMarketCap, na.rm = TRUE)) %>%
  group_by(order) %>%
  arrange(enddate) %>%
  mutate(return.industry.cumulate = exp(cumsum(log1p(average.return))) - 1) %>% 
  ungroup()


qplot(enddate, return.industry.cumulate, data = data.final, color = order, geom = "line")







###############################################################################################

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




