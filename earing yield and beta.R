
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
data.secu <- arrange(data.secu, InnerCode)


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

sql <- "  SELECT TradingDay, (ClosePrice/PrevClosePrice - 1) as Returns FROM QT_IndexQuote WHERE InnerCode=1 ORDER BY TradingDay"
index <- dbGetQuery(channel, sql)

 
dbDisconnect(channel)


# 


startdate <- ymd("2007-01-01")
enddate <- ymd("2015-02-28")


freerate <- read.csv('Yield.csv', header = TRUE, sep = ",")
index <- filter(index , TradingDay >= startdate & TradingDay <= enddate)
index$TradingDay <- as.Date(index$TradingDay)
# 

data$DataDate <- ymd(as.Date(data$DataDate))
data <- filter(data, DataDate >= startdate & DataDate <= enddate, SecuCode %in% data.secu$SecuCode)

data.industry <- group_by(data, IndustryCode, DataDate)
return.industry <- summarise(data.industry , return.industry = sum(DailyReturn* NetProfit, na.rm = TRUE)/sum(NetProfit, na.rm = TRUE)) 
ep.industry <- summarise(data.industry , return.industry = sum(NetProfit, na.rm = TRUE)/sum(FloatMarketCap, na.rm = TRUE))
data.market <- group_by(data, DataDate)
return.market <- summarise(data.market , return.market = sum(DailyReturn* NetProfit, na.rm = TRUE)/sum(NetProfit,  na.rm = TRUE))


