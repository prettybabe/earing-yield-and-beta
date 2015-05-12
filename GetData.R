options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)

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

dbDisconnect(data$channel)