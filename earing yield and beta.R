
options(java.parameters="-Xmx4g")


library(RSQLServer)
library(dplyr)
library(lubridate)
channel <- dbConnect(SQLServer(), server = 'FILE', database = 'XY', user = 'libo.jin', password= 'Aa123123' )


# ɸ
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


sql <- "SELECT TradingDay, (ClosePrice/PrevClosePrice - 1) as Returns 
        FROM QT_IndexQuote WHERE InnerCode=1 ORDER BY TradingDay"
index <- dbGetQuery(channel, sql)

 
dbDisconnect(channel)


startdate <- ymd("2007-01-01")
enddate <- ymd("2015-02-28")
index <- filter(index , TradingDay >= startdate & TradingDay <= enddate)


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
  filter(IfMonthEnd == 1, DataDate > startdate + months(11)) %>%
  select(DataDate) %>%
  unique()



Beta <- function(x, y) coef(lm(x~y))[[2]]

for (i in c(1:nrow(tradingdate))){
  beta.industry <- return.data %>%
    filter(DataDate <= tradingdate[i, 1] & DataDate >= tradingdate[i, 1] %m-% years(1)) %>%
    mutate(DataDate = tradingdate[i, 1]) %>%
    group_by(IndustryCode, DataDate) %>%
    summarise(beta = Beta(abnormal.industry, abnormal.market)) 
    
   
  corr.industry <- beta.industry %>%
    left_join(ep.industry, by = NULL) %>%
    group_by(DataDate) %>%
    summarise(corr = cor(beta, ep)) 
    
  
  if (corr.industry$corr > 0){
    
    
  }
  

}





#
corr.industry.positive <- filter(corr.industry, corr > 0)
corr.industry.negative <- filter(corr.industry, corr < 0)
beta.industry <- group_by(beta.industry, DataDate)
beta.industry <- filter(beta.industry, !is.na(IndustryCode))


beta.industry.top3 <-  arrange(beta.industry, desc(beta))
beta.industry.top3 <-  slice(beta.industry.top3, 1:3)
corr.industry.positive <- left_join(corr.industry.positive, beta.industry.top3, by = NULL)


beta.industry.last3 <-  arrange(beta.industry, beta)
beta.industry.last3 <-  slice(beta.industry.top3, 1:3)
corr.industry.negative <- left_join(corr.industry.negative, beta.industry.last3, by = NULL)






