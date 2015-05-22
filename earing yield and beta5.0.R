setwd("E:\\working\\earing yield and beta")
options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)  
library(reshape2)
library(TTR)
library(rCharts)


#######################################################################################################
# source("D:\\working\\R\\GetData.R")
load("E:/working/earing yield and beta/data.RData")
#######################################################################################################
# 确定参数
startdate <- as.Date("2006-12-31")
enddate <- as.Date("2015-3-31")
nInterval <- 12 # 回归窗口长度
nIndexCode <- 4982 # 确认市场指数， 全流通 4088，中证500 4978， 中证800 4982， 沪深300 3145

#######################################################################################################
# 计算回归窗口，收益率区间
tradingdate <- data$TradingDay %>%
  filter(IfMonthEnd == 1, TradingDate > startdate %m-% months(1),  TradingDate <= enddate) %>%
  select(TradingDate)

regression.time.monthly <- data.frame(Startdate = as.Date(lag(tradingdate$TradingDate, nInterval)),
                                      Enddate = as.Date(tradingdate$TradingDate)) %>%
  slice(-c(1:nInterval, length(Enddate)))

return.interval.monthly <- data.frame(Startdate =  as.Date(lag(tradingdate$TradingDate)),
                                      Enddate = as.Date(tradingdate$TradingDate)) %>%
  slice(-c(1:(nInterval + 1)))

#######################################################################################
#  读取无风险收益率

freerate <- read.csv('Yield.csv', header = TRUE, sep = ",") %>%
  mutate(TradingDay = as.Date(TradingDay)) %>%
  filter(TradingDay >= startdate & TradingDay <= enddate) %>%
  mutate(Daily = (1+ Yield/100)^(1/252)-1) %>%
  rename(Yearly = Yield)

#########################################################################################
#  计算市场收益率  全市场指数可以使用中证流通指数

returns <- list()
returns$Industry <- data$ReturnDaily %>%
  semi_join(data$TradingDay, by = c("DataDate" = "TradingDate")) %>%
  group_by(SecuCode) %>%
  arrange(TradingDay) %>%
  mutate(FloatMarkerCapLag1 = lag(FloatMarketCap, 1)) %>%
  filter(!is.na(FloatMarkerCapLag1)) %>%
  select(InnerCode, SecuCode, SecuAbbr, TradingDay, DailyReturn, FloatMarkerCapLag1) %>%
  left_join(data$SecuMainStock %>% select(InnerCode, CompanyCode), by = "InnerCode") %>%
  left_join(data$ExgIndustry %>% filter(Standard == 9), by = "CompanyCode") %>%
  filter(TradingDay >= InfoPublDate & TradingDay < CancelDate) %>%
  select(-InfoPublDate, -Standard, -CancelDate) %>%
#   left_join(data$IndexComponent %>% filter(IndexInnerCode == nIndexCode) %>% select(-IndexInnerCode),
#             by = c("InnerCode" = "SecuInnerCode")) %>% # 这里选择指数成分股
#   filter(TradingDay >= InDate & TradingDay < OutDate) %>%
  group_by(TradingDay, SecondIndustryCode, SecondIndustryName) %>% # 以后可以从这里调成二级行业
  summarise(Return =  weighted.mean(DailyReturn, FloatMarkerCapLag1, na.rm = TRUE),
            Count = n()) %>%
  ungroup() %>%
  inner_join(freerate %>% select(TradingDay, Daily), by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalReturn = Return - Daily) %>%
  select(-Daily) 

returns$IndustryMonthly <- data.frame()
for(i in c(1:nrow(return.interval.monthly))){
  temp.return <- returns$Industry %>%
    filter(TradingDay > return.interval.monthly$Startdate[i] & TradingDay <= return.interval.monthly$Enddate[i]) %>%
    mutate(Startdate = return.interval.monthly$Startdate[i],
           Enddate = return.interval.monthly$Enddate[i]) %>%
    group_by(SecondIndustryCode, SecondIndustryName, Startdate, Enddate) %>%
    summarise(Return = exp(sum(log1p(Return))) - 1, Count = mean(Count)) %>%
    ungroup()
  returns$IndustryMonthly <- rbind(returns$IndustryMonthly, temp.return)
} 

p1 <- returns$IndustryMonthly  %>%
  group_by(SecondIndustryCode, SecondIndustryName) %>%
  mutate(CumulateReturn = exp(cumsum(log1p(Return))) - 1, 
         Date = as.numeric(as.POSIXct(Startdate))*1000) %>%
  arrange(Startdate) %>%
  ungroup() 
x1 <- hPlot(x = "Date", y = "CumulateReturn", data = p1 , type = "line", group = "SecondIndustryName")
x1$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%M-%d}' ))
x1

p2 <- returns$IndustryMonthly  %>%
  filter(Enddate > as.Date("2007-12-31")) %>%
  mutate(Year = format(Enddate, "%Y")) %>%
  group_by(Year, SecondIndustryCode, SecondIndustryName) %>%
  summarise(YearReturn = exp(sum(log1p(Return))) - 1) %>%
  ungroup()
x2 <- nPlot(x = "Year", y = "YearReturn", data = p2, type = "multiBarChart", group = "SecondIndustryName",
            width = 2000, height = 1000)
x2

returns$Index <- data$IndexQuote %>%
  filter(InnerCode == nIndexCode) %>%
  select(TradingDay, Return) %>%
  inner_join(freerate %>% select(TradingDay, Daily), by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalReturn = Return - Daily) %>%
  select(-Daily) %>%
  arrange(TradingDay)

################################################################################################
#  计算beta
Beta <- function(x, y1, y2, y3, y4, y5, y6){
  beta = sum(coef(lm(x ~ y1 + y2 + y3 + y4 + y5 + y6))[2:7]) * 0.5 + 0.5
}

low.beta <- data.frame()
high.beta <- data.frame()
beta <- data.frame()
high.beta.return <- data.frame()
low.beta.return <- data.frame()
industry.ep <- data.frame()
corr <- data.frame()
kIndustryNumber <- 20

for(i in c(2:nrow(regression.time.monthly))){
  temp.industry <- returns$Industry %>%
    filter(TradingDay > return.interval.monthly$Startdate[i] & TradingDay <=  return.interval.monthly$Enddate[i]) %>%
    group_by(SecondIndustryCode, SecondIndustryName) %>%
    summarise(Return = exp(sum(log1p(Return))) - 1) %>%
    ungroup()
  
  temp.beta  <- returns$Industry %>%
    filter(TradingDay > regression.time.monthly$Startdate[i] & TradingDay <= regression.time.monthly$Enddate[i]) %>%
    group_by(SecondIndustryCode) %>%
    filter(n() >= 205) %>%
    arrange(TradingDay) %>%
    mutate(lag1 = lag(AbnormalReturn, 1), lag2 = lag(AbnormalReturn, 2),
           lag3 = lag(AbnormalReturn, 3), lag4 = lag(AbnormalReturn, 4),
           lag5 = lag(AbnormalReturn, 5)) %>%
    slice(-c(1:4)) %>%
    left_join(returns$Index %>% select(TradingDay, AbnormalReturn), by = c("TradingDay" = "TradingDay")) %>%
    mutate(RegressionTime = regression.time.monthly$Enddate[i]) %>%
    group_by(RegressionTime, SecondIndustryCode, SecondIndustryName) %>%
    summarise(Beta = Beta(AbnormalReturn.x, AbnormalReturn.y, lag1, lag2, lag3, lag4, lag5)) %>%
    ungroup() 
  
  temp.low.beta <- temp.beta %>%
    arrange(Beta) %>% 
    slice(c(1:kIndustryNumber))
  
  temp.high.beta <- temp.beta %>%
    arrange(desc(Beta)) %>%
    slice(c(1:kIndustryNumber))
  
  temp.high.beta.return <- temp.high.beta %>%
    left_join(temp.industry, by = c("SecondIndustryCode", "SecondIndustryName")) %>%
    group_by(RegressionTime) %>%
    summarise(Return = sum(Return/Beta/n())) %>%
    ungroup()
  
  temp.low.beta.return <- temp.low.beta %>%
    left_join(temp.industry, by = c("SecondIndustryCode", "SecondIndustryName")) %>%
    group_by(RegressionTime) %>%
    summarise(Return = sum(Return/Beta/n()))%>%
    ungroup()  
  
  low.beta <- rbind(low.beta, temp.low.beta)
  high.beta <- rbind(high.beta, temp.high.beta)
  beta <- rbind(beta, temp.beta) 
  high.beta.return <- rbind(high.beta.return, temp.high.beta.return) 
  low.beta.return <- rbind(low.beta.return, temp.low.beta.return) 
  
  temp.industry.ep <- data$ReturnDaily %>%
    filter(DataDate == regression.time.monthly$Enddate[i]) %>%
#     left_join(data$IndexComponent %>% filter(IndexInnerCode == nIndexCode) %>% select(-IndexInnerCode),
#               by = c("InnerCode" = "SecuInnerCode")) %>%
#     filter(TradingDay >= InDate & TradingDay < OutDate) %>%
    left_join(data$SecuMainStock %>% select(InnerCode, CompanyCode), by = "InnerCode") %>%
    left_join(data$ExgIndustry %>% filter(Standard == 9), by = "CompanyCode") %>%
    select(SecuCode, TradingDay, ClosePrice, FloatMarketCap, TotalShares, SecondIndustryCode, SecondIndustryName) %>%
    left_join(data$NetProfit %>% filter(DataDate == regression.time.monthly$Enddate[i]), by = c("SecuCode" = "SecuCode", "TradingDay" = "DataDate")) %>%
    select(-EndDate) %>%
    filter(!is.na(NetProfit)) %>%
    mutate(EP = NPParentCompanyOwners/TotalShares/ClosePrice) %>%
    group_by(TradingDay, SecondIndustryCode, SecondIndustryName) %>%
    summarise(EP = weighted.mean(EP, FloatMarketCap, na.rm = TRUE)) %>%
    ungroup() %>%
    semi_join(temp.industry, by = "SecondIndustryName")
  
  temp.corr <- temp.industry.ep %>%
    inner_join(temp.beta, by = c("TradingDay" = "RegressionTime", "SecondIndustryCode", "SecondIndustryName")) %>%
    group_by(TradingDay) %>%
    summarise(Corr = cor(EP, Beta)) %>%
    ungroup()
  
  industry.ep <-  rbind(industry.ep, temp.industry.ep) 
  corr <- rbind(corr, temp.corr)
}

####################################################################################################
# 买前5个高贝塔行业 和 后五个低贝塔行业
p3 <- high.beta.return %>% 
  left_join(low.beta.return, by = c("RegressionTime" = "RegressionTime")) %>%
  mutate(Date = as.character(RegressionTime), HighBeta = exp(cumsum(log1p(Return.x))) - 1,
         LowBeta = exp(cumsum(log1p(Return.y))) - 1) 
x3 <- mPlot(x = "Date", y = list("HighBeta", "LowBeta"), data = p3, type = 'Line',
            pointSize = 0, lineWidth = 1)
x3

p4 <- beta %>%
  filter(RegressionTime > as.Date("2006-12-31")) %>%
  mutate(Year = format(RegressionTime, "%Y")) %>%
  group_by(Year, SecondIndustryName) %>%
  summarise(Beta = mean(Beta)) %>%
  ungroup() %>%
  arrange(Year, Beta)
x4 <- nPlot(x = "Year", y = "Beta", data = p4, type = "multiBarChart", group = "SecondIndustryName",
            width = 2000, height = 1000)
x4$chart(forceY = c(0.6, 1.2))
x4

############################################################
corr.use <- corr
kSmooth <- 3
ts.score <- 12

corr.use$Smooth <- corr.use$Corr
for(i in c(1:nrow(corr.use))){
  if(i < ts.score)  corr.use$Score[i] <- NA
  else corr.use$Score[i] <- (corr.use$Smooth[i] - mean(corr.use$Smooth[(i-ts.score +1) : i]))/ sd(corr.use$Smooth[(i-ts.score +1) : i])
}

corr.use <- corr.use %>%
  slice(- c(1:(kSmooth + ts.score - 2))) %>%
  rename(RegressionTime = TradingDay) %>%
  select(RegressionTime, Score)

p7 <- corr.use  %>%
  mutate(RegressionTime = as.character(RegressionTime))
p7 <- mPlot(x = "RegressionTime", y = "Score", data = p7, type = 'Bar', labels = 1:4)
p7

####################################################################################################
kIndustryNumber <- 20
beta.use <- beta
backtest.return <- data.frame()
for(i in c(1:nrow(corr.use))){
  temp.return.interval <- return.interval.monthly %>%
    filter(Startdate == corr.use$RegressionTime[i])
  
  temp.return <- beta.use %>%
    filter(RegressionTime == corr.use$RegressionTime[i]) %>%
    left_join(corr.use, by = c("RegressionTime" = "RegressionTime")) %>%
    arrange(Beta) %>%
    mutate(Order = ifelse(row_number(Beta*Score/abs(Score)) %in% c(1:kIndustryNumber), "Second", 
                          ifelse(row_number(-Beta*Score/abs(Score)) %in% c(1:kIndustryNumber), "First", "Mid"))) %>%
    filter(Order == "First"| Order == "Second") %>%
    left_join(returns$IndustryMonthly, by = c("RegressionTime" = "Startdate", "SecondIndustryCode" = "SecondIndustryCode")) %>%
    mutate(Weight = ifelse(Order == 'First',Beta, 1/Beta)) %>%
    group_by(RegressionTime, Enddate, Order) %>%
    summarise(Return = weighted.mean(Return, Weight)) %>%
    ungroup()
  
  backtest.return <- rbind(backtest.return, temp.return)
}


####################################################################################################
p <- dcast(backtest.return,  RegressionTime + Enddate ~ Order) %>%
  mutate(Diff = First - Second) %>%
  mutate(First = exp(cumsum(log1p(First))) - 1, Second = exp(cumsum(log1p(Second))) - 1,
         Diff = exp(cumsum(log1p(Diff))) - 1, Date = as.character(RegressionTime))

p <- mPlot(x = "Date", y = list("First", "Second", "Diff"), data = p, type = 'Line',
           pointSize = 0, lineWidth = 1) 
print(p)

###########################################################################################
source("C:/Users/libo.jin/Desktop/R/BacktestFunction.R")
source("C:/Users/libo.jin/Desktop/R/Backtest.R")
######################################################################################
