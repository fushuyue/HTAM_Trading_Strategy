library(WindR)
w.start()

StartDate = "2014-01-01"
EndDate = "2016-05-30"

#提取股票型基金代码
EquityFundName = w.wset('sectorconstituent','date=20160531;sectorid=2001010100000000;field=wind_code')
EquityFundName = EquityFundName[2]$Data$wind_code
count = 1

# 提取净值数据
EquityNAV = w.wsd(EquityFundName[1],"trade_code,NAV_adj",StartDate,EndDate,"PriceAdj=F")$Data[,c(1,3)]
for(i in 2:length(EquityFundName))
{
	temp = w.wsd(EquityFundName[i],"trade_code,fund_existingyear,NAV_adj",StartDate,EndDate,"PriceAdj=F")$Data
	n = length(temp[,1])

	# 仅计算存续期大于三年的基金
	# count用于记录基金组合中共有几只基金
	if(temp[n,3]>=3){
	EquityNAV = cbind(EquityNAV,temp[,4])
	count = count+1
	}
}

# 提取仓位估算指数组
# 此程序使用的是最简单的沪深300，中证500和创业板指

EstimationIndex = c("000300.SH","399905.SZ","399006.SZ")
IndexPCT = w.wsd(EstimationIndex[1],"trade_code,pct_chg",StartDate,EndDate,"PriceAdj=F")$Data[,c(1,3)]
for(i in 2:length(EstimationIndex))
{
	temp = w.wsd(EstimationIndex[i],"trade_code,pct_chg",StartDate,EndDate,"PriceAdj=F")$Data
	IndexPCT = cbind(IndexPCT,temp[,3])
}

# 计算净值变化率
Nav = EquityNAV[,-1][,-1] # 去掉日期
n = length(Nav[,1])
NavPCT = (Nav[-1,] - Nav[-n,])/Nav[-n,]
NavPCT = NavPCT*100
IndexPCT = IndexPCT[-1,]
n = length(IndexPCT[,1]) # 总长度

# 回归参数
# 回归长度 len, 每只基金回归（总数 n- len +1）次
len = 30
date = IndexPCT[c(-(len-1):-1),1]
totalTime = n - len + 1
position = vector()

for (j in 0:(totalTime-1))
{
	tempPosition = vector()
	for(i in 1:length(NavPCT))
	{
		temp = summary(lm(NavPCT[i][(n-j-len+1):(n-j),1] ~ IndexPCT[(n-j-len+1):(n-j),2] + IndexPCT[(n-j-len+1):(n-j),3] 
														   + IndexPCT[(n-j-len+1):(n-j),4]))$coefficients
		coef = ifelse(temp[,4]>0.1,0,temp[,1])
		tempPosition[i] = ifelse(sum(coef[2:4]) > 1,1,sum(coef[2:4]))
	}

	# 由于回归区间短，回归可能得到异常值，故只计算正常仓位平均值
	position[j+1] = mean(tempPosition[tempPosition > 0.5])
}


# 计算平均仓位
MeanPosition = position

# 计算布林线上下轨
n = length(MeanPosition)
K = 5
totalTime = n - K + 1
upper = lower = vector()
for(i in 0:(totalTime-1))
{
	# 前 K 日平均加减标准差
	avg = mean(MeanPosition[(n-i-K+1):(n-i)])
	stdev = sd(MeanPosition[(n-i-K+1):(n-i)])
	upper[i+1] = avg + K * stdev
	lower[i+1] = avg - K * stdev
}


# 寻找信号, 低于下轨，signal为1，做多，反之做空
# 发现信号后都默认在第二天开盘执行
df = data.frame(date[(length(date)-n+K):length(date)],MeanPosition[K:n],lower,upper)
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


date = date[(length(date)-n+K):length(date)]
n = length(signal)


# 绘图
library('ggplot2')
p = ggplot() + geom_line(data = df, aes(x = date, y = EstimatedPosition),size = 1, colour = 'tomato1') + 
				xlab('日期') + ylab('仓位')
q = p + geom_line(data = df, aes(x = date, y = upper), size = 1, colour = 'steelblue2') + 
		geom_line(data = df, aes(x = date, y = lower),size = 1, colour = 'steelblue2')

q


# 策略回测
CYB = w.wsd("399905.SZ","trade_code,close",as.character(df$date[1]),as.character(df$date[n]),"PriceAdj=F")$Data
HS300 = w.wsd("000300.SH","trade_code,close",as.character(df$date[1]),as.character(df$date[n]),"PriceAdj=F")$Data
ZZ500 = w.wsd("399905.SZ","trade_code,close",as.character(df$date[1]),as.character(df$date[n]),"PriceAdj=F")$Data
index = data.frame(CYB$CLOSE,HS300$CLOSE,ZZ500$CLOSE)
colnames(index) = c('CYB','HS300','ZZ500')

profit = vector()
record = vector()
action = vector()
myNAV = vector()
commission = vector()
mytime = vector()
myNAV[1] = index[1,2]
mytime[1] = 0
j = 1
jj = 1
flag = 0

for(i in 2:n)
{
	if(flag == -1)
	{	
		profit[jj] = index[i,2] - index[i-1,2]
		myNAV[jj+1] = myNAV[jj] + profit[jj]
		mytime[jj+1] = as.character(date[i])
		jj = jj + 1
	}

	else
	{
		if(flag == 1)
		{
			profit[jj] = index[i-1,2] - index[i,2]
			myNAV[jj+1] = myNAV[jj] + profit[jj]
			mytime[jj+1] = as.character(date[i])
			jj = jj + 1
		}
	}

	if(signal[i] < signal[i-1])
	{
		# 由做多变为做空，sell
		action[j] = 'start sell'
		record[j] = as.character(date[i+1])
		myNAV[jj] = myNAV[jj] - 0.0002*2*index[i+1,2]
		j = j + 1
		flag = -1
	}
	
	else
	{
		if(signal[i] > signal[i-1])
		{
			# 由做空变为做多
			action[j] = 'start buy'
			record[j] = as.character(date[i+1])
			myNAV[jj] = myNAV[jj] - 0.0002*2*index[i+1,2]
			j = j + 1
			flag = 1
		}
	}

}
mytime[1] = record[1]
record = cbind(record,action)
myNAV = myNAV/myNAV[1]

plotNAV = data.frame(date[length(date)-531,length(date)],myNAV)
colnames(plotNAV) = c(date,NAV)
p = ggplot() + geom_line(data = plotNAV, aes(x = date, y = NAV),size = 2, colour = 'tomato1')



library(tseries)
maxDrawDown = maxdrawdown(myNAV)









