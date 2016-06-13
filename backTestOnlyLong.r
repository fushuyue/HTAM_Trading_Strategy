backTestOnlyLong = function(signal, ts){

	# signal 为仓位信息
	# index为回测标的的时间序列
	# 函数用于回测信号交易

	index = ts[,2]
	date = ts[,1]
	n = length(index)
	# 检查输入参数
	if(length(signal) != length(index))
	{
		stop('输入数组长度不同')
	}
	choose = c("buy", "empty")
	profit = 0
	record = vector()
	action = vector()
	myNAV = vector()
	fan = vector()
	commmision = 0
	j = 1
	flag = 33
	weight = 1

	for(i in 1:n)
	{
		# 在第一次信号前
		if(flag == 33)
		{
			myNAV[i] = index[1]
		}

		# 交易后
		if(flag == 1)
		{
			profit = index[i] - index[i-1]
			myNAV[i] = myNAV[i-1] + profit*flag*weight
		}

		if(flag == -1)
		{
			myNAV[i] = myNAV[i-1]
		}
		

		# look for flag changes

		if(signal[i] != 0)
		{
			# 在收盘时按收盘价卖空
			action[j] = ifelse(signal[i] == -1, choose[2],choose[1])
			record[j] = as.character(date[i])
			fan[j] = index[i]
			myNAV[i] = myNAV[i] - 0.0002*index[i]
			commmision = commmision + 0.0002*index[i]
			j = j + 1
			if(flag == 33) {first = c(i,index[i])}
			flag = signal[i]
			weight = myNAV[i]/index[i]
		}
	}

	record = cbind(record,action,fan)
	myNAV = myNAV/myNAV[1]
	plotNAV = data.frame(date,myNAV,index,signal)
	mylist = list(plotNAV[first[1]:n,],record,commmision)
	mylist
}



evaluate1 = function(mylist){
	plotNAV = mylist[[1]]
	record = mylist[[2]]
	myNAV = plotNAV$myNAV
	
	# 评价指标一
	library(tseries)
	maxDrawDown = maxdrawdown(myNAV)
	totalTime = length(plotNAV[,1])
	tradeTime = length(record[,1])
	n = totalTime
	timePeriod = c(as.Date(date[1]),as.Date(date[n]))
	totalReturn = paste((myNAV[n] - 1)*100, "%", sep='')
	vol = sd(myNAV)

	# daily average return
	r = (plotNAV[-1,2] - plotNAV[-n,2]) / plotNAV[-n,2]
	r = mean(r)

	# average anunally return:
	r = (1+r)^n - 1
	sharpe = r/vol
	
	separatePerfomance = plotNAV[plotNAV$signal != 0,2:3] 
	nSep = length(separatePerfomance[,1])
	rseparatePerfomance = (separatePerfomance[-1,] - separatePerfomance[-nSep,]) / separatePerfomance[-nSep,]
	rseparatePerfomance = rbind(rseparatePerfomance,(plotNAV[n,2:3] - separatePerfomance[nSep,])/separatePerfomance[nSep,])
	record = cbind(record,rseparatePerfomance)
	colnames(record) = c('date','action','index','return','index_return')
	winningRate = length(which(rseparatePerfomance[,1] > 0)) / length(rseparatePerfomance[,1])
	winToLose = length(which(rseparatePerfomance[,1] > 0)) / (length(rseparatePerfomance[,1]) - length(which(rseparatePerfomance[,1] > 0)) )
	drawDownPeriod = c(date[maxDrawDown[[2]]],date[maxDrawDown[[3]]])
	mylist = list("日期区间" = timePeriod, "总交易日" = n, "交易次数" = tradeTime, "胜率" = winningRate,
					 "盈亏比" = winToLose , "总收益率" = totalReturn, "夏普比率" = sharpe, "交易费用" = mylist[[3]], "最大回撤" = maxDrawDown[[1]],"单次" = record)
	write.csv(record,"fu1.csv")
	mylist
}


evaluate2 = function(mylist){
	# 评价指标二
	plotNAV = mylist[[1]]
	plotNAV$date = as.POSIXlt(plotNAV$date)$year+1900

	temp = plotNAV[plotNAV$date == 2007,]
	outcome = assess(temp)
	for(i in 2:10){

		year = 2007 + (i-1)
		temp = plotNAV[plotNAV$date == year,]
		fu = assess(temp)
		outcome = rbind(outcome,fu)
	}

	outcome = as.data.frame(outcome)
	colnames(outcome) = c("年份", "策略收益", "指数收益", "最大回撤", "波动率","夏普比率")
	total = assess(plotNAV)
	total[1]='Total'
	outcome = rbind(outcome , total)
	write.csv(outcome,"fu2.csv")
	outcome
}

assess = function(ts){

	# ts 为净值和股指走势的时间序列
	n = length(ts[,1])
	outcome = vector()
	r1 = (ts[n,2] - ts[1,2])/ts[1,2]
	r2 = (ts[n,3] - ts[1,3])/ts[1,3]

	# average daily return:
	r = (ts[-1,2] - ts[-n,2]) / ts[-n,2]
	r = mean(r)

	# average anunally return:
	r = (1+r)^n - 1
	library(tseries)
	maxDrawDown = maxdrawdown(ts[,2])
	sdev = sd(ts[,2]) 
	sharpe = r/sdev
	outcome = c(ts[1,1],r1,r2,maxDrawDown[[1]],sdev,sharpe)
	outcome
}
