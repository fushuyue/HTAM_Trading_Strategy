library(WindR)
w.start()

StartData = "2014-01-01"
EndData = "2016-05-30"

#提取股票型基金代码
EquityFundName = w.wset('sectorconstituent','date=20160531;sectorid=2001010100000000;field=wind_code')
EquityFundName = EquityFundName[2]$Data$wind_code
count = 1

# 提取净值数据
EquityNAV = w.wsd(EquityFundName[1],"trade_code,NAV_adj",StartData,EndData,"PriceAdj=F")$Data[,c(1,3)]
for(i in 2:length(EquityFundName))
{
	temp = w.wsd(EquityFundName[i],"trade_code,fund_existingyear,NAV_adj",StartData,EndData,"PriceAdj=F")$Data
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
IndexPCT = w.wsd(EstimationIndex[1],"trade_code,pct_chg",StartData,EndData,"PriceAdj=F")$Data[,c(1,3)]
for(i in 2:length(EstimationIndex))
{
	temp = w.wsd(EstimationIndex[i],"trade_code,pct_chg",StartData,EndData,"PriceAdj=F")$Data
	IndexPCT = cbind(IndexPCT,temp[,3])
}

# 计算净值变化率
Nav = EquityNAV[,-1][,-1]
n = length(Nav[,1])
NavPCT = (Nav[-1,] - Nav[-n,])/Nav[-n,]
NavPCT = NavPCT*100
IndexPCT = IndexPCT[-1,]
n = length(IndexPCT[,1]) # 总长度

# 回归参数
# 回归长度 len, 每只基金回归（总数 n- len +1）次
len = 18
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
		coef = ifelse(temp[,4]>0.05,0,temp[,1])
		tempPosition[i] = ifelse(sum(coef[2:4]) > 1,1,ifelse(sum(coef[2:4])<0.5,0.5,sum(coef[2:4])))
	}
	position[j+1] = mean(tempPosition)
}


# 计算平均仓位
MeanPosition = position

# 计算布林线上下轨
n = length(MeanPosition)
K = 10
totalTime = n - K + 1
upper = lower = vector()
for(i in 0:(totalTime-1))
{
	# 前 K 日平均加减标准差
	avg = mean(MeanPosition[(n-i-K+1):(n-i)])
	stdev = sd(MeanPosition[(n-i-K+1):(n-i)])
	upper[i+1] = avg + K * stdev
	lower[i+1] = avg + K * stdev
}


# 寻找信号, 低于下轨，signal为1，做多，反之做空
df = data.frame(date[K:n,1],MeanPosition[K:n],lower,upper)
colnames(df) = c('date','EstimatedPosition','lowerBound','upperBound')
signal = ifelse(df$EstimatedPosition < df$lowerBound, 1, ifelse(df$EstimatedPosition > df$lowerBound, -1, 0))
n = length(signal)


# 策略回测
CYB = w.wsd("399905.SZ","trade_code,open",df$date[1],df$date[n],"PriceAdj=F")$Data
ETF300 = w.wsd("000300.SH","trade_code,open",df$date[1],df$date[n],"PriceAdj=F")$Data
ETF500 = w.wsd("399905.SZ","trade_code,open",df$date[1],df$date[n],"PriceAdj=F")$Data

Action = vector()
for(i in 2:n)
{

	if(signal[i] < signal[i-1])
	{
		# 由做多变为做空，sell
		Action[i] = 'sell'
	}
	else
	{
		if(signal[i] > signal[i-1])
		{
			# 由做空变为做多
			Action[i] = 'buy'
		}

		else {Action[i] = 'keep'}
	}

}


# 当天观测到仓位变化后，第二天以开盘价买入或卖出
df = cbind(CYB$DATETIME,CYB$OPEN,ETF300$OPEN,ETF500$OPEN,Action)
profit = df[df$Action!='keep',]
n = length(profit[,1])
temp = profit[-1,2:4] - profit[-n,2:4]
profit = cbind(profit[-n,],temp)


