
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
	j = 1
	flag = 33

	for(i in 1:n)
	{
		# 在第一次信号前
		if(flag == 33)
		{
			myNAV[i] = index[i]
		}

		# 交易后
		if(flag == 1)
		{	
			profit = index[i] - index[i-1]
			myNAV[i] = myNAV[i] + profit
		}

		if(flag == -1)
		{
			profit = index[i-1] - index[i]
			myNAV[i] = myNAV[i] + profit
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
			}
			else{
				myNAV[i] = myNAV[i] - 0.0002*2*index[i]
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
				}
				else{
					myNAV[i] = myNAV[i] - 0.0002*2*index[i]
				}

				j = j + 1
				flag = 1
			}
		}
	}

	record = cbind(record,action)
	myNAV = myNAV/myNAV[1]

	# plot
	library('ggplot2')
	plotNAV = data.frame(date,myNAV)
	p = ggplot() + geom_line(data = plotNAV, aes(x = date, y = myNAV),size = 1, colour = 'tomato1')
	p + geom_line(data = ts, aes(x = ts[,1], y = ts[,2]),size = 1, colour = 'grey')

	library(tseries)
	maxDrawDown = maxdrawdown(myNAV)
	myNAV
}
