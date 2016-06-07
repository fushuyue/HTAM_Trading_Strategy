findPosition = function(StartDate, EndDate, fundList){

	len = 30
	# select the suitable fund
	year = as.POSIXlt(StartDate)$year+1900
	if(year == 2016) {fund = fundList[[9]]} else {fund = fundList[[year-2007+1]]}

	# extract time series of price
	tdays = w.tdays((StartDate-100), StartDate)[[2]]
	tempDay = rev(tdays[,1])[len]
	EquityNAV = w.wsd(fund[1,1],"NAV_adj",tempDay,EndDate)$Data[,1:2]

	for(i in 2:length(fund[,1]))
	{
		temp = w.wsd(fund[i,1],"NAV_adj",tempDay,EndDate,"PriceAdj=F")$Data
		EquityNAV = cbind(EquityNAV,temp[,2])
	}

	# 计算净值变化率
	Nav = EquityNAV[,-1][,-1] # 去掉日期
	n = length(Nav[,1])
	NavPCT = (Nav[-1,] - Nav[-n,])/Nav[-n,]
	NavPCT = NavPCT*100
	
	# 回归变量
	 
	# 回归参数：回归长度 len, 每只基金回归（总数 n- len +1）次
	EstimationIndex = c("399928.SZ" , "399929.SZ" , "399930.SZ","399931.SZ","399932.SZ","399933.SZ","399934.SZ","399935.SZ","399936.SZ","399937.SZ")
	IndexPCT = w.wsd(EstimationIndex[1],"trade_code,pct_chg",tempDay, EndDate,"PriceAdj=F")$Data[,c(1,3)]
	for(i in 2:length(EstimationIndex))
	{
		temp = w.wsd(EstimationIndex[i],"trade_code,pct_chg",tempDay, EndDate,"PriceAdj=F")$Data
		IndexPCT = cbind(IndexPCT,temp[,3])
	}

	IndexPCT = IndexPCT[-1,]
	n = length(IndexPCT[,1]) # 总长度
	date = IndexPCT[len:n,1]
	totalTime = n - len + 1
	position = vector()

	for (j in 0:(totalTime-1))
	{
		tempPosition = vector()
		for(i in 1:length(NavPCT))
		{
			temp = summary(lm(NavPCT[i][(n-j-len+1):(n-j),1] ~ IndexPCT[(n-j-len+1):(n-j),2] + IndexPCT[(n-j-len+1):(n-j),5] + IndexPCT[(n-j-len+1):(n-j),11] +
																IndexPCT[(n-j-len+1):(n-j),3] + IndexPCT[(n-j-len+1):(n-j),4] + IndexPCT[(n-j-len+1):(n-j),6] +
																IndexPCT[(n-j-len+1):(n-j),7] + IndexPCT[(n-j-len+1):(n-j),8] + IndexPCT[(n-j-len+1):(n-j),9] +
																IndexPCT[(n-j-len+1):(n-j),10]))$coefficients
			coef = ifelse(temp[,4]>0.1,0,temp[,1])
			tempPosition[i] = ifelse(sum(coef[2:11]) > 1,1,sum(coef[2:11]))
		}

		# 由于回归区间短，回归可能得到异常值，故只计算正常仓位平均值
		position[j+1] = mean(tempPosition[tempPosition > 0.2])
	}

	# 计算平均仓位
	df = data.frame(date,rev(position))
	colnames(df) = c('date', 'position')
	paste("year = ", year)
	df
}


library(WindR)
w.start()


# 创建基金样本列表

df = read.csv('fund.csv',stringsAsFactors = FALSE)
threshold = 40

df07 = df[df$date < (as.Date("2007-1-1") - threshold),c(1,3)]
df08 = df[df$date < (as.Date("2008-1-1") - threshold),c(1,3)]
df09 = df[df$date < (as.Date("2009-1-1") - threshold),c(1,3)]
df10 = df[df$date < (as.Date("2010-1-1") - threshold),c(1,3)]
df11 = df[df$date < (as.Date("2011-1-1") - threshold),c(1,3)]
df12 = df[df$date < (as.Date("2012-1-1") - threshold),c(1,3)]
df13 = df[df$date < (as.Date("2013-1-1") - threshold),c(1,3)]
df14 = df[df$date < (as.Date("2014-1-1") - threshold),c(1,3)]
df15 = df[df$date < (as.Date("2015-1-1") - threshold),c(1,3)]

fundList = list(df07, df08, df09, df10, df11, df12, df13, df14, df15)

StartDate = as.Date("2007-01-01")
EndDate = Sys.Date() - 1

position = findPosition(StartDate, StartDate + 364, fundList)
tempStart = StartDate
for(i in 1:8)
{
	tempStart = tempStart + 365
	tempEnd = tempStart + 364
	if(as.POSIXlt(tempStart)$year+1900 == 2008 | as.POSIXlt(tempStart)$year+1900 == 2012) {tempEnd = tempEnd + 1; tempStart = tempStart + 1}
	position = rbind(position , findPosition(tempStart, tempEnd, fundList))
}

tempStart = as.Date("2016-01-01")
EndDate = Sys.Date() - 1
position = rbind(position , findPosition(tempStart, EndDate, fundList))
write.csv(position,"全指数position0606.csv")

# 计算平均仓位
MeanPosition = position[,2]


# 计算布林线上下轨
n = length(MeanPosition)
K = 10
totalTime = n - K + 1
upper = lower = vector()
for(i in 1:totalTime)
{
	# 前 K 日平均加减标准差
	avg = mean(MeanPosition[i:(i+K-1)])
	stdev = sd(MeanPosition[i:(i+K-1)])
	upper[i] = avg + 2 * stdev
	lower[i] = avg - 2 * stdev
}

date = position[(K):n,1]

# 寻找信号, 低于下轨，signal为1，做多，反之做空
# 发现信号后都默认在第二天开盘执行
# HS300 = w.wsd("000300.SH","close",as.Date(date[1]),as.Date(date[totalTime]),"PriceAdj=F")$Data
HS300 = w.wsd("000300.SH","close,DMA",as.Date(date[1]),as.Date(date[totalTime - 1000]),"DMA_S=3;DMA_L=5;DMA_N=3;DMA_IO=1;PriceAdj=F")$Data
temp = w.wsd("000300.SH","close,DMA",as.Date(date[totalTime - 1000 + 1]),as.Date(date[totalTime]),"DMA_S=3;DMA_L=5;DMA_N=3;DMA_IO=1;PriceAdj=F")$Data
HS300 = rbind(HS300,temp)
HS300 = rbind(HS300[1:233,],HS300[235:(totalTime+1),])

df = data.frame(date,MeanPosition[(K):n],lower,upper,HS300[,3])
colnames(df) = c('date','EstimatedPosition','lowerBound','upperBound','DMA')
#write.csv(df,"全指数bound0606.csv")

signal = vector()
signal[1] = 0
flag = 0
for(i in 2:dim(df)[1])
{
	signal[i] = 0
	if( (df[i-1,2] > df[i-1,3]) & (df[i,2] < df[i,3]) & flag != 1 & df[i,5] >= 0)
	{
		signal[i] = 1
		flag = 1 
	}
	if( (df[i-1,2] < df[i-1,4]) & (df[i,2] > df[i,4]) & flag != -1 & df[i,5] <= 0)
	{
		signal[i] = -1
		flag = -1
	}
}


# 绘图
# library('ggplot2')
# p = ggplot() + geom_line(data = df, aes(x = date, y = EstimatedPosition),size = 1, colour = 'tomato1') + 
# 				xlab('日期') + ylab('仓位')
# q = p + geom_line(data = df, aes(x = date, y = upper), size = 1, colour = 'grey') + 
# 		geom_line(data = df, aes(x = date, y = lower),size = 1, colour = 'grey')

# q

length(which(signal>0))

length(which(signal<0))

# 策略回测
list = backTest(signal, HS300[,1:2])
write.csv(evaluate1(list)[[10]],"fu1.csv")
write.csv(evaluate2(list),"fu2.csv")
evaluate1(list)
evaluate2(list)
plotNAV = list[[1]]

#write.csv(plotNAV,"全指数myoutcome0606.csv")

# plot
p = ggplot() + geom_line(data = plotNAV, aes(x = date, y = myNAV),size = 1, colour = 'tomato1')
p = p + geom_line(data = plotNAV, aes(x = date, y = index/index[1]),size = 1, colour = 'grey') 
p





