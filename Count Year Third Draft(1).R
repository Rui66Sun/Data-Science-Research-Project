install.packages("xts")
install.packages("quantmod")
install.packages("fBasics")
install.packages("urca")
install.packages("FinTS")
install.packages("forecast")
install.packages("rugarch")
install.packages("PerformanceAnalytics")
install.packages("stringr")

library(xts)
library(stringr)
library(tseries)
library(forecast)
library(tseries)
library(ggplot2)
#yearCount <- read.csv(file.choose(), stringsAsFactors = F)
yearCount = read.csv("/Users/pengxiaotao/Documents/gitroom/homework/2022/R-2022-05-28-timeseries/year_count.csv")
yearCount$publish_year = as.Date(paste(str_trim(yearCount$publish_year), 1, 1, sep = "-"))
?as.Date
head(yearCount)
#转换为ts
yearCount_ts = xts(subset(yearCount, select = c("total")), order.by = yearCount$publish_year)
?xts
head(yearCount_ts)
class(yearCount_ts)

#ts图示
library(quantmod)
library(PerformanceAnalytics)
colnames(yearCount_ts)

yearCount_ts$logret <- Delt(yearCount_ts$total, type= "log")
yearCount_ts$ret = Delt(yearCount_ts$total, type = "arithmetic")
# remove na
#yearProfit_ts = na.omit(yearProfit_ts)
# remove inf and -inf
#yearProfit_ts = yearProfit_ts[!is.infinite(rowSums(yearProfit_ts)), ]
#summary(yearProfit_ts)

#head(yearProfit_ts)
#用0填充第一个log NA
yearCount_ts$logret[1] <- 0
yearCount_ts$ret[1] = 0

# bulk 0
train.ts = yearCount_ts["1950/2019"]


# bulk 1
train.ts1 = yearCount_ts["1950/2016"]
train.ts1
test.ts1 = yearCount_ts['2020/2021']
test.ts1

# bulk 2
train.ts2 = yearCount_ts["1950/2010"]
train.ts2
test.ts2 = yearCount_ts['2020/2021']
test.ts2


# bulk 3
train.ts3 = yearCount_ts["1950/2000"]
train.ts3
test.ts3 = yearCount_ts['2020/2021']
test.ts3


# bulk 4
train.ts4 = yearCount_ts["1950/1990"]
train.ts4
test.ts4 = yearCount_ts['2020/2021']
test.ts4

# bulk 5
train.ts5 = yearCount_ts["1950/1980"]
train.ts5
test.ts5 = yearCount_ts['2020/2021']
test.ts5

# bulk 6
train.ts6 = yearCount_ts["1950/1970"]
train.ts6
test.ts6 = yearCount_ts['2020/2021']
test.ts6

# bulk 7
train.ts7 = yearCount_ts["1950/1960"]
train.ts7
test.ts7 = yearCount_ts['2020/2021']
test.ts7

#时间序列线图(total+log)
plot(train.ts, multi.panel = T, yaxis.same = F)
?plot
#直方图
hist(train.ts$logret, breaks = 100)
hist(train.ts$ret, breaks = 100)
#QQ图，看和正态分布定差异
qqnorm(train.ts$logret)
qqline(train.ts$logret,col = "red")

qqnorm(train.ts$ret)
qqline(train.ts$ret,col = "red")

#集成图
library(PerformanceAnalytics)
chart.Histogram(train.ts$logret,
                methods = c("add.density", 
                            "add.normal",
                            "add.qqplot"))

chart.Histogram(train.ts$ret,
                methods = c("add.density", 
                            "add.normal",
                            "add.qqplot"))
#时间序列性质与检验；偏度-右偏（比较容易取大值); 峰度-5很大，不是正态分布
library(fBasics)
basicStats(train.ts)
#正态分布检验-p拒绝原假设（符合正态分布），所以不是正太分布
normalTest(as.numeric(train.ts),method = 'jb',na.rm = T)


#平稳性检验
adf.test(train.ts$logret)
adf.test(train.ts$ret)
#ACF, PACF-超过说明数据不是白噪声，有内在规律
acf(train.ts$logret,lag.max = 100)
acf(train.ts$ret, lag.max = 100)

pacf(train.ts$logret,lag.max = 100)
pacf(train.ts$ret, lag.max = 100)


#count原值
Box.test(train.ts$total, lag = 20, type = 'Lj')

# percent difference
Box.test(train.ts$ret, lag = 2, type = 'Lj')

#?白噪声检验-小于0.05，拒绝，在lag=100时不是白噪声
Box.test(train.ts$logret, lag = 2,type = 'Lj')
#ARCH效应检验-波动是否异常
library(FinTS)
ArchTest(train.ts$logret)
#均值方程度残差平方项白噪声检验-说明是独立度，可以TS分析
Box.test((train.ts$logret)^2, lag = 2, type = "Lj")
Box.test(sqrt(train.ts$logret), lag = 2, type = "Lj")


do.valid = function(train.ts, test.ts, last_year, prediction_count=2) {
  fit_ar_1 <- auto.arima(train.ts$logret, stationary = T,seasonal = F)
  fore1 = forecast(fit_ar_1, h = prediction_count)
  rmse = sum(as.vector(test.ts$logret) - as.vector(fore1$mean))^2 / 2
  compare.df = data.frame(pred_logret=fore1$mean, actual_logret=test.ts$logret, actual_total=test.ts$total)
  
  for (i in 1:nrow(compare.df)) {
    # log(current / prev) = logret
    # current = e^logret * prev
    prev_total = 0
    ret = compare.df[i, 'pred_logret']
    if (i == 1) {
      prev_total = train.ts[last_year]$total
    } else {
      prev_total = compare.df[i-1, 'pred_total']
    }
    
    compare.df[i, 'pred_total'] = exp(ret) * prev_total
  }
  
  list(rmse=rmse, df=compare.df, model=fit_ar_1)
}


# cross validation
r1 = do.valid(train.ts1, test.ts1, "2016")
r1

r2 = do.valid(train.ts2, test.ts2, "2010")

r3 = do.valid(train.ts3, test.ts3, "2000")


df = data.frame(year = c(2019, 2010, 2000), rmse=c(r1$rmse, r2$rmse, r3$rmse))
df
ggplot(df, aes(x=year, y=rmse)) + geom_line()



test.ts = yearCount_ts['2017/2021']
test.ts
model <- auto.arima(train.ts1$logret, stationary = T,seasonal = F)
predictioins = forecast(model, h = nrow(test.ts))

rmses = (as.vector(test.ts$logret) - as.vector(predictioins$mean))^2 / 2
rmses
years = 2017:2021
year.df = data.frame(year=years, rmse=rmses)
year.df
ggplot(year.df, aes(x=year, y=rmse)) + geom_line()
