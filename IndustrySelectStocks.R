setwd("D:\\working\\earing yield and beta")
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
load("D:/working/data.RData")
#######################################################################################################
# 确定参数
startdate <- as.Date("2006-12-31")
enddate <- as.Date("2015-3-31")
nInterval <- 12 # 回归窗口长度
nIndexCode <- 4088  # 确认市场指数， 全流通 4088，中证500 4978， 中证800 4982， 沪深300 3145

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

low.beta <- data.frame()
high.beta <- data.frame()
beta <- data.frame()
high.beta.return <- data.frame()
low.beta.return <- data.frame()
ep <- data.frame()
industry.return <- data.frame()
corr <- data.frame()
kIndustryNumber <- 5

Beta <- function(x, y){
  beta = coef(lm(x ~ y))[2] 
}

SMB <- function(x){
  Number <- length(x)
  n <- round(Number/3)
  small <- x[1:n]
  big <- x[(2*n+1):Number]            
  SMB = mean(small) - mean(big)
}

for(i in c(1:nrow(regression.time.monthly))){
  regression_startdate <- regression.time.monthly$Startdate[i]
  regression_enddate <- regression.time.monthly$Enddate[i]
  return_startdate <- return.interval.monthly$Startdate[i]
  return_enddate <- return.interval.monthly$Enddate[i]
  
  # 选取你投资标的的股票 例如全市场选 还是 中证800 
  # 选择成分股 剔除净利润是负的股票
  stock <- data$IndexComponent %>%
    filter(IndexInnerCode == nIndexCode,
           regression_enddate >= InDate & regression_enddate < OutDate) %>%
    mutate(RegressionTime = regression_enddate) %>% 
    left_join(data$SecuMainStock %>% select(InnerCode, SecuCode, CompanyCode), by = c("SecuInnerCode" = "InnerCode")) %>%
    left_join(data$NetProfit %>% select(SecuCode, DataDate, NPParentCompanyOwners) %>%
                filter(DataDate == regression_enddate), by = "SecuCode") %>%
    filter(NPParentCompanyOwners > 0) %>%
    left_join(data$ExgIndustry %>% filter(Standard == 9), by = "CompanyCode") %>%
    filter(InfoPublDate <= RegressionTime, CancelDate > RegressionTime) %>%
    select(SecuInnerCode, NPParentCompanyOwners, FirstIndustryCode, FirstIndustryName) 
    
  # 按选出的股票 计算个股beta以及行业Beta
  temp_daily_return <- data$ReturnDaily %>%
    semi_join(data$TradingDay, by = c("DataDate" = "TradingDate")) %>%
    group_by(SecuCode) %>%
    arrange(TradingDay) %>%
    mutate(FloatMarkerCapLag1 = lag(FloatMarketCap, 1)) %>%
    filter(!is.na(FloatMarkerCapLag1)) %>%
    select(InnerCode, SecuCode, SecuAbbr, TradingDay, DailyReturn, FloatMarkerCapLag1) %>%
    inner_join(stock, by = c("InnerCode" = "SecuInnerCode")) %>%
    filter(TradingDay > regression_startdate & TradingDay <= regression_enddate) %>%
    group_by(FirstIndustryCode, FirstIndustryName, TradingDay) %>%
    summarise(Return = weighted.mean(DailyReturn, FloatMarkerCapLag1)) %>%
    ungroup() %>%
    rename(IndustryReturn = Return, RegressionTime = TradingDay) %>%
    inner_join(data$IndexQuote %>% filter(InnerCode == nIndexCode) %>% select(TradingDay, Return),
              by = c("RegressionTime" = "TradingDay")) %>%
    rename(IndexReturn = Return) %>%
    inner_join(freerate %>% select(TradingDay, Daily), by = c("RegressionTime" = "TradingDay")) %>%
    rename(FreeRate = Daily) 
                
  temp.beta <- temp_daily_return %>%
    group_by(FirstIndustryName) %>% 
    mutate(IndustryAbnormalReturn = IndustryReturn - FreeRate,
           IndexAbnormalReturn = IndexReturn - FreeRate) %>%
    summarise(Beta = Beta(IndustryAbnormalReturn, IndexAbnormalReturn)) %>%
    mutate(Date = regression_enddate) 
  
  temp.ep <- data$ReturnDaily %>% 
    filter(DataDate == regression_enddate) %>%
    inner_join(stock,  by = c("InnerCode" = "SecuInnerCode")) %>%
    group_by(FirstIndustryName) %>%
    mutate(EPS = NPParentCompanyOwners/TotalShares/ClosePrice) %>%
    summarise(EP = weighted.mean(EPS, FloatMarketCap)) %>%
    mutate(Date = regression_enddate) 
    
  temp <- data$ReturnDaily %>%
    semi_join(data$TradingDay, by = c("DataDate" = "TradingDate")) %>%
    filter(DataDate == regression_enddate) %>%
    inner_join(stock, by = c("InnerCode" = "SecuInnerCode")) %>%
    select(InnerCode, FloatMarketCap, FirstIndustryName) 
 
  temp.industry <- data$ReturnDaily %>%
    semi_join(data$TradingDay, by = c("DataDate" = "TradingDate")) %>%
    filter(DataDate > return_startdate & DataDate <= return_enddate) %>%
    inner_join(stock, by = c("InnerCode" = "SecuInnerCode")) %>%
    group_by(InnerCode) %>% 
    summarise(Return =  exp(sum(log1p(DailyReturn))) - 1) %>%
    left_join(temp, by = "InnerCode") %>% 
    group_by(FirstIndustryName) %>%
    summarise(Return = weighted.mean(Return, FloatMarketCap)) %>%
    mutate(Date = regression_enddate) 
    
  temp.low.beta <- temp.beta %>%
    mutate(Date = regression_enddate) %>%
    arrange(Beta) %>% 
    slice(c(1:kIndustryNumber))
  
  temp.high.beta <- temp.beta %>%
    mutate(Date = regression_enddate) %>%
    arrange(desc(Beta)) %>%
    slice(c(1:kIndustryNumber))
  
  temp.high.beta.return <- temp.high.beta %>%
    group_by(Date) %>%
    left_join(temp.industry, by = c("FirstIndustryName", "Date")) %>%
    summarise(Return = sum(Return/Beta/n())) %>%
    ungroup()

  temp.low.beta.return <- temp.low.beta %>%
    group_by(Date) %>%
    left_join(temp.industry, by = c("FirstIndustryName", "Date")) %>%
    summarise(Return = sum(Return/Beta/n())) %>%
    ungroup()

  low.beta <- rbind(low.beta, temp.low.beta)
  high.beta <- rbind(high.beta, temp.high.beta)
  beta <- rbind(beta, temp.beta) 
  ep <-  rbind(ep, temp.ep) 
  industry.return <- rbind(industry.return, temp.industry)
  high.beta.return <- rbind(high.beta.return, temp.high.beta.return) 
  low.beta.return <- rbind(low.beta.return, temp.low.beta.return) 
  
  temp.corr <- temp.ep %>%
    inner_join(temp.beta, by = c("Date" = "Date", "FirstIndustryName")) %>%
    group_by(Date) %>%
    summarise(Corr = cor(EP, Beta)) %>%
    ungroup()
  
  
  corr <- rbind(corr, temp.corr)
}

####################################################################################################
# 买前5个高贝塔行业 和 后五个低贝塔行业
p3 <- high.beta.return %>% 
  left_join(low.beta.return, by = "Date") %>%
  mutate(Date = as.character(Date), HighBeta = exp(cumsum(log1p(Return.x))) - 1,
         LowBeta = exp(cumsum(log1p(Return.y))) - 1) 
x3 <- mPlot(x = "Date", y = list("HighBeta", "LowBeta"), data = p3, type = 'Line',
            pointSize = 0, lineWidth = 1)
x3

p4 <- beta %>%
  filter(Date > as.Date("2006-12-31")) %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year, FirstIndustryName) %>%
  summarise(Beta = mean(Beta)) %>%
  ungroup() %>%
  arrange(Year, Beta)
x4 <- nPlot(x = "Year", y = "Beta", data = p4, type = "multiBarChart", group = "FirstIndustryName",
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
  rename(RegressionTime = Date) %>%
  select(RegressionTime, Score)

p7 <- corr.use  %>%
  mutate(RegressionTime = as.character(RegressionTime))
p7 <- mPlot(x = "RegressionTime", y = "Score", data = p7, type = 'Bar', labels = 1:4)
p7

####################################################################################################
kIndustryNumber <- 1
beta.use <- beta
backtest.return <- data.frame()
for(i in c(1:nrow(corr.use))){
  temp.return.interval <- return.interval.monthly %>%
    filter(Startdate == corr.use$RegressionTime[i])
  
  temp.return <- beta.use %>%
    filter(Date == corr.use$RegressionTime[i]) %>%
    left_join(corr.use, by = c("Date" = "RegressionTime")) %>%
    arrange(Beta) %>%
    mutate(Order = ifelse(row_number(Beta*Score/abs(Score)) %in% c(1:kIndustryNumber), "Second", 
                          ifelse(row_number(-Beta*Score/abs(Score)) %in% c(1:kIndustryNumber), "First", "Mid"))) %>%
    filter(Order == "First"| Order == "Second") %>%
    left_join(industry.return, by = c("Date" = "Date", "FirstIndustryName" = "FirstIndustryName")) %>%
    mutate(Weight = ifelse(Order == 'First',Beta, 1/Beta)) %>%
    group_by(Date, Order) %>%
    summarise(Return = weighted.mean(Return, Weight)) %>%
    ungroup()
  
  backtest.return <- rbind(backtest.return, temp.return)
}

####################################################################################################
p <- dcast(backtest.return,  Date  ~ Order) %>%
  mutate(Diff = First - Second) %>%
  mutate(First = exp(cumsum(log1p(First))) - 1, Second = exp(cumsum(log1p(Second))) - 1,
         Diff = exp(cumsum(log1p(Diff))) - 1, Date = as.character(Date))

p <- mPlot(x = "Date", y = list("First", "Second", "Diff"), data = p, type = 'Line',
           pointSize = 0, lineWidth = 1) 
print(p)

###########################################################################################
source("C:/Users/libo.jin/Desktop/R/BacktestFunction.R")
source("C:/Users/libo.jin/Desktop/R/Backtest.R")
######################################################################################
