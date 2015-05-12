setwd("D:\\working\\earing yield and beta")
options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(TTR)

startdate <- as.Date("2006-12-31")
enddate <- as.Date("2015-3-31")

#######################################################################################
# source("D:\\working\\R\\GetData.R")
load("D:/working/data.RData")
#######################################################################################
#  读取无风险收益率
freerate <- read.csv('Yield.csv', header = TRUE, sep = ",") %>%
  mutate(TradingDay = as.Date(TradingDay)) %>%
  filter(TradingDay >= startdate & TradingDay <= enddate) %>%
  mutate(Daily = (1+ Yield/100)^(1/252)-1) %>%
  rename(Yearly = Yield)
summary(freerate)

######################################################################################################
# 确定交易时间
tradingdate <- data$TradingDay %>%
  filter(IfMonthEnd == 1, TradingDate > startdate %m-% months(1),  TradingDate <= enddate) %>%
  select(TradingDate)

nInterval <- 12
regression.time.monthly <- data.frame(Startdate = as.Date(lag(tradingdate$TradingDate, nInterval)),
                                      Enddate = as.Date(tradingdate$TradingDate)) %>%
  slice(-c(1:nInterval, length(Enddate)))

return.interval.monthly <- data.frame(Startdate =  as.Date(lag(tradingdate$TradingDate)),
                                      Enddate = as.Date(tradingdate$TradingDate)) %>%
  slice(-c(1:(nInterval + 1)))

################################################################################################
# 先从整个行业的水平看 计算行业收益率

kIndexInnerCode <- 4982
stocks <- data$ReturnDaily %>%
  semi_join(data$TradingDay, by = c("DataDate" = "TradingDate")) %>%
  inner_join(data$IndexComponent %>% filter(IndexInnerCode == kIndexInnerCode), 
             by = c("InnerCode" = "SecuInnerCode")) %>%
  mutate(IsComponent = ifelse(DataDate >= InDate & DataDate < OutDate, 1, 0)) 

returns <- list()
returns$Industry <- stocks %>%
  filter(IsComponent == 1) %>%
  semi_join(data$TradingDay %>% select(TradingDate), by = c("DataDate" = "TradingDate")) %>%
  group_by(SecuCode) %>%
  arrange(TradingDay) %>%
  mutate(FloatMarkerCapLag1 = lag(FloatMarketCap, 1)) %>%
  filter(!is.na(FloatMarkerCapLag1)) %>%
  group_by(TradingDay, IndustryCodeNew, IndustryNameNew) %>%
  summarise(Return =  weighted.mean(DailyReturn, FloatMarkerCapLag1, na.rm = TRUE),
            Count = n()) %>%
  filter(!is.na(IndustryCodeNew)) %>%
  ungroup() %>%
  inner_join(freerate %>% select(TradingDay, Daily), by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalReturn = Return - Daily) %>%
  select(-Daily) %>%
  arrange(IndustryCodeNew, TradingDay) 


             
returns$Index <- data$IndexQuote %>%
  filter(InnerCode == kIndexInnerCode) %>%
  select(TradingDay, Return) %>%
  inner_join(freerate %>% select(TradingDay, Daily), by = c("TradingDay" = "TradingDay")) %>%
  mutate(AbnormalReturn = Return - Daily) %>%
  select(-Daily) %>%
  arrange(TradingDay)
#################################################################################################
# 每月末计算Beta 但选择下月还在投资域中的行业
Beta1 <- function(x, y1, y2, y3, y4, y5, y6){
  beta = sum(coef(lm(x ~ y1 + y2 + y3 + y4 + y5 + y6))[2:7]) * 0.5 + 0.5
}

Beta2 <- function(x, y){
  beta = coef(lm(x~y))[2]
}


low.beta <- list()
high.beta <- list()
beta <- list()
high.beta.return <- list()
low.beta.return <- list()
industry.ep <- list()
corr <- list()
kIndustryNumber <- 5


for(i in c(1:nrow(regression.time.monthly))){
  temp.industry <- returns$Industry %>%
    filter(TradingDay > return.interval.monthly$Startdate[i] & TradingDay <=  return.interval.monthly$Enddate[i]) %>%
    group_by(IndustryCodeNew, IndustryNameNew) %>%
    summarise(Return = exp(sum(log1p(Return))) - 1) %>%
    ungroup()
  
  temp.beta  <- returns$Industry %>%
    filter(TradingDay > regression.time.monthly$Startdate[i] & TradingDay <= regression.time.monthly$Enddate[i]) %>%
    semi_join(temp.industry, by = c("IndustryCodeNew")) %>%
    group_by(IndustryCodeNew) %>%
    filter(n()>=205) %>%
    arrange(TradingDay) %>%
    mutate(lag1 = lag(AbnormalReturn, 1), lag2 = lag(AbnormalReturn, 2),
           lag3 = lag(AbnormalReturn, 3), lag4 = lag(AbnormalReturn, 4),
           lag5 = lag(AbnormalReturn, 5)) %>%
    slice(-c(1:4)) %>%
    left_join(returns$Index %>% select(TradingDay, AbnormalReturn), by = c("TradingDay" = "TradingDay")) %>%
    mutate(RegressionTime = regression.time.monthly$Enddate[i]) %>%
    group_by(RegressionTime, IndustryCodeNew, IndustryNameNew) %>%
    summarise(Beta = Beta1(AbnormalReturn.x, AbnormalReturn.y, lag1, lag2, lag3, lag4, lag5)) %>%
    ungroup() 
  
  temp.low.beta <- temp.beta %>%
    arrange(Beta) %>% 
    slice(c(1:kIndustryNumber))
  
  temp.high.beta <- temp.beta %>%
    arrange(desc(Beta)) %>%
    slice(c(1:kIndustryNumber))
  
  temp.high.beta.return <- temp.high.beta %>%
    left_join(temp.industry, by = c("IndustryCodeNew", "IndustryNameNew")) %>%
    group_by(RegressionTime) %>%
    summarise(Return = weighted.mean(Return, Beta)) %>%
    ungroup()
  
  temp.low.beta.return <- temp.low.beta %>%
    left_join(temp.industry, by = c("IndustryCodeNew", "IndustryNameNew")) %>%
    group_by(RegressionTime) %>%
    summarise(Return = weighted.mean(Return, Beta)) %>%
    ungroup()  
  
  low.beta$Industry <- rbind(low.beta$Industry, temp.low.beta)
  high.beta$Industry <- rbind(high.beta$Industry, temp.high.beta)
  beta$Industry <- rbind(beta$Industry, temp.beta) 
  high.beta.return$Industry <- rbind(high.beta.return$Industry, temp.high.beta.return) 
  low.beta.return$Industry <- rbind(low.beta.return$Industry, temp.low.beta.return) 
  
  temp.industry.ep <- stocks %>%
    filter(TradingDay == regression.time.monthly$Enddate[i], IsComponent == 1) %>%
    select(SecuCode, TradingDay, ClosePrice, FloatMarketCap, TotalShares, IndustryCodeNew, IndustryNameNew) %>%
    left_join(data$NetProfit %>% filter(DataDate == regression.time.monthly$Enddate[i]), by = c("SecuCode" = "SecuCode", "TradingDay" = "DataDate")) %>%
    select(-EndDate) %>%
    group_by(TradingDay, SecuCode, IndustryCodeNew, IndustryNameNew, FloatMarketCap) %>%
    summarise(EP = NetProfit/TotalShares/ClosePrice) %>%
    group_by(TradingDay, IndustryCodeNew, IndustryNameNew) %>%
    summarise(EP = weighted.mean(EP, FloatMarketCap, na.rm = TRUE)) %>%
    ungroup() %>%
    semi_join(temp.industry, by = "IndustryCodeNew")
    
  temp.corr <-  temp.industry.ep %>%
    inner_join(temp.beta, by = c("TradingDay" = "RegressionTime", "IndustryCodeNew", "IndustryNameNew")) %>%
    group_by(TradingDay) %>%
    summarise(Corr = cor(EP, Beta)) %>%
    ungroup()
    
   industry.ep$Industry <-  rbind(industry.ep$Industry, temp.industry.ep) 
   corr$Industry <- rbind(corr$Industry, temp.corr)
}


#######################################################################################################
p <-  high.beta.return$Industry %>%
  left_join(low.beta.return$Industry, by = "RegressionTime") %>%
  mutate(CumulateHigh = exp(cumsum(log1p(Return.x))) - 1,
         CumulateLow = exp(cumsum(log1p(Return.y))) - 1) %>%
  rename(HighBeta = CumulateHigh, LowBeta = CumulateLow) %>%
  select(RegressionTime, HighBeta, LowBeta)
p <- melt(p, id = "RegressionTime")
qplot(RegressionTime, value, data = p, color = variable, geom = "line")
# 也就是说低贝塔的行业与高贝塔的行业在历史上没区别
###################################################################################################

# 计算行业月度收益率
return.monthly <- data.frame()
for(i in c(1:nrow(return.interval.monthly))){
  temp.return <- returns$Industry %>%
    filter(TradingDay > return.interval.monthly$Startdate[i] & TradingDay <= return.interval.monthly$Enddate[i]) %>%
    mutate(Startdate = return.interval.monthly$Startdate[i],
           Enddate = return.interval.monthly$Enddate[i]) %>%
    group_by(IndustryCodeNew, IndustryNameNew, Startdate, Enddate) %>%
    summarise(Return = exp(sum(log1p(Return))) - 1, Count = mean(Count)) %>%
    ungroup()
  return.monthly <- rbind(return.monthly, temp.return)
} 

##############################################################################################################

corr.use <- corr$Industry
kSmooth <- 3
ts.score <- 24
corr.use$Smooth <- EMA(corr.use$Corr, kSmooth)
# corr.use$Smooth <- corr.use$Corr
for(i in c(1:nrow(corr.use))){
  if(i < ts.score)  corr.use$Score[i] <- NA
  else corr.use$Score[i] <- (corr.use$Smooth[i] - mean(corr.use$Smooth[(i-ts.score +1) : i]))/ sd(corr.use$Smooth[(i-ts.score +1) : i])
}

corr.use <- corr.use %>%
  slice(- c(1:(kSmooth + ts.score - 2))) %>%
  rename(RegressionTime = TradingDay) %>%
  select(RegressionTime, Score)

####################################################################################################

kIndustryNumber <- 5
beta.use <- beta$Industry
backtest.return <- data.frame()
for(i in c(1:nrow(corr.use))){
  temp.return.interval <- return.interval.monthly %>%
    filter(Startdate == corr.use$RegressionTime[i])
  
  temp.return <- beta.use %>%
    filter(RegressionTime == corr.use$RegressionTime[i]) %>%
    left_join(corr.use, by = c("RegressionTime" = "RegressionTime")) %>%
    mutate(Order = ifelse(row_number(Beta*Score/abs(Score)) %in% c(1:kIndustryNumber), "Second", 
                          ifelse(row_number(-Beta*Score/abs(Score)) %in% c(1:kIndustryNumber), "First", "Mid"))) %>%
    filter(Order == "First"| Order == "Second") %>%
    left_join(return.monthly, by = c("RegressionTime" = "Startdate", "IndustryCodeNew" = "IndustryCodeNew")) %>%
    mutate(Weight = ifelse(Order == 'First',Beta, 1/Beta)) %>%
    group_by(RegressionTime, Enddate, Order) %>%
    summarise(Return = weighted.mean(Return, Weight)) %>%
    ungroup()
  
  backtest.return <- rbind(backtest.return, temp.return)
}


####################################################################################################
backtest.return <- dcast(backtest.return,  RegressionTime + Enddate ~ Order) %>%
  mutate(Diff = First - Second)
backtest.return <- melt(backtest.return, id =  c("RegressionTime", "Enddate")) %>%
  group_by(variable) %>%
  mutate(CumulateReturn =  exp(cumsum(log1p(value))) - 1) %>%
  select(Date = Enddate, Order = variable, CumulateReturn) %>%
  ungroup() 


p <- ggplot(backtest.return, aes(Date, CumulateReturn,  color = Order)) +
  geom_line() 
print(p)


###########################################################################################
source("C:/Users/libo.jin/Desktop/R/BacktestFunction.R")
source("C:/Users/libo.jin/Desktop/R/Backtest.R")
######################################################################################
