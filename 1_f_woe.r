

###############################################################################
###############################################################################
# 单变量WOE（针对已经离散化的变量）
f_woe <- function(datainput0, varname0, yname0='bad'){
# datainput0: 输入的数据集
# varname0:   待处理的变量名称,字符
# yname0:     目标变量名称,字符,1坏0好(评分卡专用)

x <- datainput0[[varname0]]
y <- datainput0[[yname0]]
# 自变量不能为缺失值，如果需要，应在之前分组时就处理好。
index <- which(y %in% c(0, 1) & (!is.na(x)))
x <- x[index]
y <- factor(y[index])

if (length(x) > 0){
X <- addmargins(table(x, y))
m <- data.frame(V=rownames(X), 
N=X[, 3],  P=X[, 3] / X[nrow(X), 3], 
N0=X[, 1], P0=X[, 1] / X[nrow(X), 1], 
N1=X[, 2], P1=X[, 2] / X[nrow(X), 2], 
WOE=NA, IV=NA, BadRate=X[, 2] / X[, 3])
rownames(m) <- NULL
m$WOE <- log(m$P1 / m$P0)
m$WOE[m$P1 == 0 | m$P0 == 0] <- NA
m$IV <- (m$P1 - m$P0) * m$WOE
m$IV[nrow(m)] <- sum(m$IV[-nrow(m)], na.rm=T)
m[, -1] <- round(m[, -1], 4)
colnames(m) <- c(varname0, '#Total', '%Total', '#Good', '%Good', '#Bad', '%Bad', 'WOE', 'IV', 'Bad Rate')
print(m)
par(mfrow=c(2, 1), cex.axis=1, mar=c(2,4,3,1))
barplot(m[-nrow(m), 2], names.arg=m[-nrow(m), 1], main=paste('Freq_', varname0, sep=''))
plot(m$WOE[-nrow(m)], type='l', axes=F, xlab='', ylab='WOE', frame.plot=T, main=paste('WOE_', varname0, sep=''), lwd=5)
axis(side=2)
axis(side=1, at=seq_len(nrow(m)-1), labels=m[-nrow(m), 1])
}

if (length(x) <= 0){
m <- matrix(NA, nrow=4, ncol=10)
colnames(m) <- c(varname0, '#Total', '%Total', '#Good', '%Good', '#Bad', '%Bad', 'WOE', 'IV', 'Bad Rate')
winDialog('ok', paste('there are only NAs in this variable: ', varname0, sep=''))
}

}

# # traindata是训练集
# names(traindata)

# # 离散变量情形
# summary(traindata$Cat_x1)
# traindata$c_Cat_x1 <- as.factor(traindata$Cat_x1)
# summary(traindata$c_Cat_x1)
# # levels(traindata$c_Cat_x1) <- paste(1:nlevels(traindata$c_Cat_x1), 
# # levels(traindata$c_Cat_x1), sep=': ')
# # summary(traindata$c_Cat_x1)
# # 如果缺失值有特定含义或者是随机缺失，一般是单独作为一类
# # 如果是非随机缺失，则看具体情况，有可能删除该条记录
# # traindata$c_Cat_x1 <- as.character(traindata$c_Cat_x1)
# # traindata$c_Cat_x1[is.na(traindata$c_Cat_x1)] <- '000: missing'
# # traindata$c_Cat_x1 <- as.factor(traindata$c_Cat_x1)
# # summary(traindata$c_Cat_x1)
# f_woe(datainput0=traindata, varname0='c_Cat_x1', yname0='bad')

# # 连续变量情形
# summary(traindata$Quan_x1)
# plot(density(na.omit(traindata$Quan_x1)))
# quantile(traindata$Quan_x1, seq(0,1,0.1), na.rm=T)
# 根据上述分位数
# traindata$c_Quan_x1 <- cut(traindata$Quan_x1, c(0,5,15,40,100), 
# right=T, include.lowest=T)
# summary(traindata$c_Quan_x1)
# # levels(traindata$c_Quan_x1) <- paste(1:nlevels(traindata$c_Quan_x1), 
# # levels(traindata$c_Quan_x1), sep=': ')
# # summary(traindata$c_Quan_x1)
# # 如果缺失值有特定含义或者是随机缺失，一般是单独作为一类
# # 如果是非随机缺失，则看具体情况，有可能删除该条记录
# # traindata$c_Quan_x1 <- as.character(traindata$c_Quan_x1)
# # traindata$c_Quan_x1[is.na(traindata$c_Quan_x1)] <- '000: missing'
# # traindata$c_Quan_x1 <- as.factor(traindata$c_Quan_x1)
# # summary(traindata$c_Quan_x1)
# f_woe(datainput0=traindata, varname0='c_Quan_x1', yname0='bad')

