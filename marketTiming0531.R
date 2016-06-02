findPosition = function(StartDate, EndDate, fundList){

	len = 40
	# select the suitable fund
	year = as.POSIXlt(StartDate)$year+1900
	if(year == 2016) {fund = fundList[[9]]} else {fund = fundList[[year-2007+1]]}

	# extract time series of price
	EquityNAV = w.wsd(fund[1],"trade_code,NAV_adj",StartDate - len + 1,EndDate,"PriceAdj=F")$Data[,c(1,3)]

	for(i in 2:length(fund))
	{
		temp = w.wsd(fund[i],"trade_code,NAV_adj",StartDate - len + 1,EndDate,"PriceAdj=F")$Data
		EquityNAV = cbind(EquityNAV,temp[,3])
	}

	# 计算净值变化率
	Nav = EquityNAV[,-1][,-1] # 去掉日期
	n = length(Nav[,1])
	NavPCT = (Nav[-1,] - Nav[-n,])/Nav[-n,]
	NavPCT = NavPCT*100
	
	# 回归变量
	EstimationIndex = c("000300.SH","399905.SZ")
	# 回归参数：回归长度 len, 每只基金回归（总数 n- len +1）次
	
	IndexPCT = w.wsd(EstimationIndex[1],"trade_code,pct_chg",StartDate - len + 1, EndDate,"PriceAdj=F")$Data[,c(1,3)]
	for(i in 2:length(EstimationIndex))
	{
		temp = w.wsd(EstimationIndex[i],"trade_code,pct_chg",StartDate - len +1, EndDate,"PriceAdj=F")$Data
		IndexPCT = cbind(IndexPCT,temp[,3])
	}

	IndexPCT = IndexPCT[-1,]
	n = length(IndexPCT[,1]) # 总长度
	date = IndexPCT[c(-(len-1):-1),1]
	totalTime = n - len + 1
	position = vector()

	for (j in 0:(totalTime-1))
	{
		tempPosition = vector()
		for(i in 1:length(NavPCT))
		{
			temp = summary(lm(NavPCT[i][(n-j-len+1):(n-j),1] ~ IndexPCT[(n-j-len+1):(n-j),2] + 
																IndexPCT[(n-j-len+1):(n-j),3] ))$coefficients
			coef = ifelse(temp[,4]>0.1,0,temp[,1])
			tempPosition[i] = ifelse(sum(coef[2:3]) > 1,1,sum(coef[2:3]))
		}

		# 由于回归区间短，回归可能得到异常值，故只计算正常仓位平均值
		position[j+1] = mean(tempPosition[tempPosition > 0.3])
	}

	# 计算平均仓位
	rev(position)

}

library(WindR)
w.start()

source("findSignal0603.R")
source("backTest.R")

# 创建基金样本列表

df = read.csv('fund.csv',stringsAsFactors = FALSE)
threshold = 40

df07 = df[df$date < (as.Date("2007-1-1") - threshold),1]
df08 = df[df$date < (as.Date("2008-1-1") - threshold),1]
df09 = df[df$date < (as.Date("2009-1-1") - threshold),1]
df10 = df[df$date < (as.Date("2010-1-1") - threshold),1]
df11 = df[df$date < (as.Date("2011-1-1") - threshold),1]
df12 = df[df$date < (as.Date("2012-1-1") - threshold),1]
df13 = df[df$date < (as.Date("2013-1-1") - threshold),1]
df14 = df[df$date < (as.Date("2014-1-1") - threshold),1]
df15 = df[df$date < (as.Date("2015-1-1") - threshold),1]

fundList = list(df07, df08, df09, df10, df11, df12, df13, df14, df15)

StartDate = as.Date("2007-01-01")
EndDate = Sys.Date()

position = findPosition(StartDate, StartDate + 364, fundList)
tempStart = StartDate
for(i in 1:8)
{
	tempStart = tempStart + 365
	tempEnd = tempStart + 364
	if(as.POSIXlt(tempStart)$year+1900 == 2008 | as.POSIXlt(tempStart)$year+1900 == 2012) {tempEnd = tempEnd + 1; tempStart = tempStart + 1}
	position = c(position , findPosition(tempStart, tempEnd, fundList))
}

tempStart = as.Date("2016-01-01")
EndDate = Sys.Date()
position = c(position , findPosition(tempStart, EndDate, fundList[[9]]))

# 计算平均仓位
MeanPosition = position

# 计算布林线上下轨
n = length(MeanPosition)
K = 5
totalTime = n - K 
upper = lower = vector()
for(i in 1:totalTime)
{
	# 前 K 日平均加减标准差
	avg = mean(MeanPosition[i:(i+K-1)])
	stdev = sd(MeanPosition[i:(i+K-1)])
	upper[i] = avg + K * stdev
	lower[i] = avg - K * stdev
}



# 寻找信号, 低于下轨，signal为1，做多，反之做空
# 发现信号后都默认在第二天开盘执行
HS300 = w.wsd("000300.SH","close",StartDate,EndDate,"PriceAdj=F")$Data
date = HS300[,1]
df = data.frame(date[(length(date)-n+K+1):length(date)],MeanPosition[(K+1):n],lower,upper)
colnames(df) = c('date','EstimatedPosition','lowerBound','upperBound')

signal = vector()
signal[1] = 0
flag = 0
for(i in 2:dim(df)[1])
{
	signal[i] = 0
	if( (df[i-1,2] > df[i-1,3]) & (df[i,2] < df[i,3]) & flag != 1)
	{
		signal[i] = 1
		flag = 1 
	}
	if( (df[i-1,2] < df[i-1,4]) & (df[i,2] > df[i,4]) & flag != -1)
	{
		signal[i] = -1
		flag = -1
	}
}


HS300 = HS300[(length(date)-n+K+1):length(date),]
n = length(signal)


# 绘图
library('ggplot2')
p = ggplot() + geom_line(data = df, aes(x = date, y = EstimatedPosition),size = 1, colour = 'tomato1') + 
				xlab('日期') + ylab('仓位')
q = p + geom_line(data = df, aes(x = date, y = upper), size = 1, colour = 'grey') + 
		geom_line(data = df, aes(x = date, y = lower),size = 1, colour = 'grey')

q
length(which(signal>0))

length(which(signal<0))

# 策略回测
plotNAV = backTest(signal, HS300)
evaluate1(plotNAV)
evaluate2(plotNAV)

# plot
p = ggplot() + geom_line(data = plotNAV, aes(x = date, y = myNAV),size = 1, colour = 'tomato1')
p = p + geom_line(data = plotNAV, aes(x = date, y = index/index[1]),size = 1, colour = 'grey') 
p


