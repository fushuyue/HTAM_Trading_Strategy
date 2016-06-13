library(WindR)
w.start()
position = read.csv('全指数position0606.csv')
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
# j 值
HS300 = w.wsd("000300.SH","close,KDJ",as.Date(date[1]),as.Date(date[totalTime - 1000]),"KDJ_N=9;KDJ_M1=3;KDJ_M2=3;KDJ_IO=3;PriceAdj=F")$Data
temp = w.wsd("000300.SH","close,KDJ",as.Date(date[totalTime - 1000 + 1]),as.Date(date[totalTime]),"KDJ_N=9;KDJ_M1=3;KDJ_M2=3;KDJ_IO=3;PriceAdj=F")$Data
HS300 = rbind(HS300,temp)
HS300 = rbind(HS300[1:233,],HS300[235:(totalTime+1),])

# # D值
# HS3001 = w.wsd("000300.SH","close,KDJ",as.Date(date[1]),as.Date(date[totalTime - 1000]),"KDJ_N=9;KDJ_M1=3;KDJ_M2=3;KDJ_IO=2;PriceAdj=F")$Data
# temp1 = w.wsd("000300.SH","close,KDJ",as.Date(date[totalTime - 1000 + 1]),as.Date(date[totalTime]),"KDJ_N=9;KDJ_M1=3;KDJ_M2=3;KDJ_IO=2;PriceAdj=F")$Data
# HS3001 = rbind(HS3001,temp1)
# HS3001 = rbind(HS3001[1:233,],HS3001[235:(totalTime+1),])

# HS300 = cbind(HS300,HS3001[,3])
df = data.frame(date,MeanPosition[(K):n],lower,upper,HS300[,3])
colnames(df) = c('date','EstimatedPosition','lowerBound','upperBound','KDJ_J')

# 1.日期, 2.仓位，3.下轨，4.上轨，5.KDJ——KD差额	

findSignal = function(x,y){
	# signal1 为仓位信号，signal2为 KDJ信号
	signal = signal2 = vector()
	signal[1] = signal2[1] = 0
	flag = 0

	for(i in 2:dim(df)[1])
	{
		signal[i] = 0
		signal2[i] = 0
		if( (df[i-1,2] > df[i-1,3]) & (df[i,2] < df[i,3]) & flag != 1 & (df[i,5] >= x))
		{
			signal[i] = 1
			signal2[i] = 1
			flag = 1 
			next
		}

		if( (df[i-1,2] < df[i-1,4]) & (df[i,2] > df[i,4]) & flag != -1 & (df[i,5] <= y))
		{
			signal[i] = -1
			signal2[i] = -1
			flag = -1
			next
		}
	}
	signal
}

signal = findSignal(110-3*5,30-1*5)
list = backTestOnlyLong(signal, HS300[,1:2])
evaluate1(list)
evaluate2(list)
plotNAV = list[[1]]

#write.csv(plotNAV,"全指数myoutcome0606.csv")

# plot
library('ggplot2')
p = ggplot() + geom_line(data = plotNAV, aes(x = date, y = index/index[1]),size = 1, colour = 'grey') 
p = p + geom_line(data = plotNAV, aes(x = date, y = myNAV),size = 1, colour = 'tomato1')
p

# fu = evaluate2(list)[11,]

# for(i in 1:6)
# {
# 	for(j in 1:6)
# 	{	
# 		signal = findSignal(110-i*5,30-j*5)
# 		# length(which(signal>0))

# 		# length(which(signal<0))

# 		list = backTestOnlyLong(signal, HS300[,1:2])
# 		#evaluate1(list)
# 		fu = rbind(fu,evaluate2(list)[11,])
# 		# plotNAV = list[[1]]

# 		# #write.csv(plotNAV,"全指数myoutcome0606.csv")

# 		# # plot
# 		# library('ggplot2')
# 		# p = ggplot() + geom_line(data = plotNAV, aes(x = date, y = index/index[1]),size = 1, colour = 'grey') 
# 		# p = p + geom_line(data = plotNAV, aes(x = date, y = myNAV),size = 1, colour = 'tomato1')
# 		# p
# 	}
# }
