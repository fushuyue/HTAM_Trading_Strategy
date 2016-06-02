
backTest = function(signal, ts){

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

	profit = 0
	record = vector()
	action = vector()
	myNAV = vector()
	commmision = 0
	j = 1
	flag = 33

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
			myNAV[i] = myNAV[i-1] + profit
		}

		if(flag == -1)
		{
			profit = index[i-1] - index[i]
			myNAV[i] = myNAV[i-1] + profit
		}
		

		# look for flag changes

		if(signal[i] == -1)
		{
			# 在收盘时按收盘价卖空
			action[j] = 'start sell'
			record[j] = as.character(date[i])

			# 第一笔交易应该为单边
			if(j == 1)
			{
				myNAV[i] = myNAV[i] - 0.0002*index[i]
				commmision = commmision + 0.0002*index[i]
			}
			else{
				myNAV[i] = myNAV[i] - 0.0002*2*index[i]
				commmision = commmision + 0.0004*index[i]
			}

			j = j + 1
			flag = -1
		}
		
		else
		{
			if(signal[i] == 1)
			{
				# 在收盘时按收盘价买多
				action[j] = 'start buy'
				record[j] = as.character(date[i])

				# 第一笔交易应该为单边
				if(j == 1)
				{
					myNAV[i] = myNAV[i] - 0.0002*index[i]
					commmision = commmision + 0.0002*index[i]
				}
				else{
					myNAV[i] = myNAV[i] - 0.0002*2*index[i]
					commmision = commmision + 0.0004*index[i]
				}

				j = j + 1
				flag = 1
			}
		}
	}

	record = cbind(record,action)
	myNAV = myNAV/myNAV[1]
	plotNAV = data.frame(date,myNAV,index,signal)
	plotNAV

}




assess = function(ts){

	# ts 为净值和股指走势的时间序列
	n = length(ts[,1])
	outcome = vector()
	r1 = (ts[n,2] - ts[1,2])/ts[1,2]
	r2 = (ts[n,3] - ts[1,3])/ts[1,3]
	library(tseries)
	maxDrawDown = maxdrawdown(ts[,2])
	sdev = sd(ts[,2])
	outcome = c(ts[1,1],r1,r2,maxDrawDown[[1]],sdev)
	outcome
}


evaluate1 = function(plotNAV){
	attach(plotNAV)
	# 评价指标一
	library(tseries)
	maxDrawDown = maxdrawdown(myNAV)
	timePeriod = c(as.Date(date[1]),as.Date(date[n]))
	
	totalTime = n
	tradeTime = length(record[,1])
	totalReturn = paste((myNAV[n] - 1)*100, "%", sep='')
	vol = sd(myNAV)

	separatePerfomance = plotNAV[signal != 0,2] 
	separatePerfomance = (separatePerfomance[-1] - separatePerfomance[-length(separatePerfomance)]) / separatePerfomance[-length(separatePerfomance)]
	winningRate = length(which(separatePerfomance > 0)) / length(separatePerfomance)
	winToLose = length(which(separatePerfomance > 0)) / (length(separatePerfomance) - length(which(separatePerfomance > 0)) )
	drawDownPeriod = c(date[maxDrawDown[[2]]],date[maxDrawDown[[3]]])
	mylist = list("日期区间" = timePeriod, "总交易日" = n, "交易次数" = tradeTime, 
					"胜率" = winningRate, "盈亏比" = winToLose , "总收益率" = totalReturn, "交易费用" = commmision, "最大回撤" = maxDrawDown[[1]])
	mylist
}


evaluate2 = function(plotNAV){
	# 评价指标二
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
	colnames(outcome) = c("年份", "策略收益", "指数收益", "最大回撤", "波动率")
	total = assess(plotNAV)
	total[1]='Total'
	outcome = rbind(outcome , total)
	outcome
}
