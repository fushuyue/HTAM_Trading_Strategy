df = read.csv('fund.csv',stringsAsFactors = FALSE)
attach(df)
threshold = 60
df07 = df[df$date < (as.Date("2007-1-1") - threshold),]
df08 = df[df$date < (as.Date("2008-1-1") - threshold),]
df09 = df[df$date < (as.Date("2009-1-1") - threshold),]
df10 = df[df$date < (as.Date("2010-1-1") - threshold),]
df11 = df[df$date < (as.Date("2011-1-1") - threshold),]
df12 = df[df$date < (as.Date("2012-1-1") - threshold),]
df13 = df[df$date < (as.Date("2013-1-1") - threshold),]
df14 = df[df$date < (as.Date("2014-1-1") - threshold),]
df15 = df[df$date < (as.Date("2015-1-1") - threshold),]

fundNumber = vector()
n[1] = dim(df07)[1]
n[2] = dim(df08)[1]
n[3] = dim(df09)[1]
n[4] = dim(df10)[1]
n[5] = dim(df11)[1]
n[6] = dim(df12)[1]
n[7] = dim(df13)[1]
n[8] = dim(df14)[1]
n[9] = dim(df15)[1]

n
# n = data.frame(n)
# colnames(n) = c("2007","2008","2009","2010","2011","2012","2013","2014","2015")
# n
