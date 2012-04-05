

###############################################################################
###############################################################################
# 粗分
f_format <- function(datainput, varname, type=c(1,2), fileout, n_groups=10){
# datainput: 输入的数据集
# varname:   待处理的变量名称,字符
# type:      1数值2字符
# fileout:   输出的文件名
# n_groups:  分成几组

x <- datainput[[varname]]

if (type == 2){
if (!is.factor(x)){
x <- as.factor(as.character(x))
}
x_format <- data.frame(y=levels(x), label=levels(x))
}

if (type == 1){
if (!is.numeric(x)){
x <- as.numeric(as.character(x))
}
x_format <- data.frame(y=sort(unique(round(c(-998, -997, max(x) + 1, 
quantile(x[!x %in% c(-997, -998, -999)], seq(0, 1 - 1 / n_groups, 1 / n_groups)))))))
# 左闭右开
}

sink(file=fileout, append=T)
print('# this is the start of a variable')
print(varname)
print(x_format)
print('# this is the end of a variable')
sink()
}


###############################################################################
###############################################################################
# 单变量WOE（针对已经离散化的变量）
f_woe_one <- function(datainput0, varname0, yname0){
# datainput0: 输入的数据集
# varname0:   待处理的变量名称,字符
# yname0:     目标变量名称,字符,1好0坏(评分卡专用)

x <- datainput0[[varname0]]
y <- datainput0[[yname0]]
index <- which(y %in% c(0, 1) & (!is.na(x)))
x <- x[index]
y <- y[index]
if (length(x) > 0){
X <- addmargins(table(x, y))
m <- data.frame(V=rownames(X), 
 N=X[, 3],  P=X[, 3] / X[nrow(X), 3], 
N0=X[, 1], P0=X[, 1] / X[nrow(X), 1], 
N1=X[, 2], P1=X[, 2] / X[nrow(X), 2], 
WOE=NA, IV=NA, BadRate=X[, 1] / X[, 3])
rownames(m) <- NULL
m$WOE <- log(m$P1 / m$P0)
m$WOE[m$P1 == 0 | m$P0 == 0] <- NA
m$IV <- (m$P1 - m$P0) * m$WOE
m$IV[nrow(m)] <- sum(m$IV[-nrow(m)])
m[, -1] <- round(m[, -1], 4)
colnames(m) <- c(varname0, '#Total', '%Total', '#Bad', '%Bad', '#Good', '%Good', 'WOE', 'IV', 'Bad Rate')
}
if (length(x) <= 0){
m <- matrix(NA, nrow=4, ncol=10)
colnames(m) <- c(varname0, '#Total', '%Total', '#Bad', '%Bad', '#Good', '%Good', 'WOE', 'IV', 'Bad Rate')
winDialog('ok', paste('there is only -999 in this variable: ', varname0, sep=''))
}
return(m)
}


###############################################################################
###############################################################################
# 单变量卡方检验（针对已经离散化的变量）
f_chisq_one <- function(datainput0, varname0, yname0){
# datainput0: 输入的数据集
# varname0:   待处理的变量名称,字符
# yname0:     目标变量名称,字符,1好0坏(评分卡专用)

x <- datainput0[[varname0]]
y <- datainput0[[yname0]]
index <- which(y %in% c(0, 1) & (!is.na(x)))
x <- x[index]
y <- y[index]
if (nlevels(x) >= 2){
z <- chisq.test(x, y)$p.value
}
if (nlevels(x) < 2){
z <- 999
winDialog('ok', paste('there is only one level in this variable: ', varname0, sep=''))
}
return(z)
}


###############################################################################
###############################################################################
# WOE报告(csv版本),并存储flag与woe的对应关系,并生成IV与P值的数据集
f_woe <- function(datainput, filein, yname, filewoe, varlabels, varlist=NA){
# datainput: 输入的数据集
# filein:    format的文件名
# yname:     目标变量名称,字符,1好0坏(评分卡专用)
# filewoe:   woe输出文件名
# varlabels: 中文标签
# varlist:   待处理的变量

flag_woe_list <- list()
df_iv_pvalue <- data.frame(var=NULL, IV=NULL, p_value=NULL)

formatfile <- readLines(filein)
index_start <- grep('# this is the start of a variable', formatfile)
index_end <- grep('# this is the end of a variable', formatfile)

imax <- length(index_start)

for (i in 1:imax){
varname <- formatfile[index_start[i] + 1]
varname <- strsplit(varname, '\"')[[1]][2]

if (is.na(varlist[1]) | varname %in% varlist){

new_varname <- paste('flag_', varname, sep='')
is_factor <- length(grep('label', formatfile[index_start[i] + 2])) > 0
main <- formatfile[(index_start[i] + 3) : (index_end[i] - 1)]
datainput[[new_varname]] <- NA

if (is_factor){
# 字符类型
for(j in 1:length(main)){
main2 <- strsplit(main[j], ' ')[[1]]
main3 <- main2[main2 != '']
datainput[[new_varname]][datainput[[varname]] == main3[2]] <- main3[3]
}
datainput[[new_varname]][is.na(datainput[[new_varname]])] <- '0.other'
datainput[[new_varname]][datainput[[varname]] == (-999)] <- NA
datainput[[new_varname]] <- as.factor(datainput[[new_varname]])
}

if (!is_factor){
# 数值类型
main2 <- unlist(strsplit(main, ' '))
main3 <- main2[main2 != '']
main4 <- as.numeric(main3[c(F,T)])
datainput[[new_varname]] <- cut(datainput[[varname]], breaks=unique(main4), right=F, ordered_result=T)
datainput[[new_varname]][datainput[[varname]] == (-999)] <- NA
datainput[[new_varname]] <- factor(datainput[[new_varname]])
}

varlabels[, 1] <- as.character(varlabels[, 1])
varlabels[, 2] <- as.character(varlabels[, 2])
varlabel <- varlabels[varlabels[, 1] == varname, 2]
m0 <- matrix(NA, nrow=1, ncol=11)
colnames(m0) <- rep('', 11)
m0[1,1] <- varlabel
write.table(m0, filewoe, append=T, sep=',', row.names=F, na='')

m <- f_woe_one(datainput0=datainput, varname0=new_varname, yname0=yname)
chisq_p <- f_chisq_one(datainput0=datainput, varname0=new_varname, yname0=yname)

colnames(m)[1] <- gsub('flag_', '', colnames(m)[1])
m$chisq_p <- c(rep(NA, nrow(m)-1), chisq_p)

m1 <- matrix(NA, nrow=4, ncol=11)
colnames(m1) <- rep('', 11)
write.table(m, filewoe, append=T, sep=',', row.names=F, na='')
write.table(m1, filewoe, append=T, sep=',', row.names=F, na='')
# 这里的warning是正常滴

flag_woe_list[[varname]] <- m

df_iv_pvalue <- rbind(df_iv_pvalue, data.frame(var=varname, IV=m$IV[nrow(m)], p_value=chisq_p))

print(c(i,date()))
}
}

return(list(data_flag=datainput, flag_woe=flag_woe_list, df_iv_pvalue=df_iv_pvalue))
# data_flag存储了相应的分组信息,flag_woe存储了分组与WOE的对应,df_iv_pvalue存储了IV与P值
}


###############################################################################
###############################################################################
# WOE调整(并且暂时用来生成WOE报告(knitr))
f_woe_adj <- function(datainput, filein, varname, type=c(1,2), yname, varlabels, is_complete=F){
# datainput:   输入的数据集
# filein:      format的文件名
# varname:     待处理的变量名称,字符
# type:        1数值 2字符
# yname:       目标变量名称,字符,1好0坏(评分卡专用)
# varlabels:   中文标签
# is_complete: 是否输出全部字段.默认只输出Freq,WOE和IV。

formatfile <- readLines(filein)
index_var <- grep(paste('"', varname, '"', sep=''), formatfile)
index_end <- grep('# this is the end of a variable', formatfile)
index_end <- min(index_end[index_end > index_var])
main <- formatfile[(index_var + 2) : (index_end - 1)]
new_varname <- paste('flag_', varname, sep='')
datainput[[new_varname]] <- NULL

if (type == 2){
# 字符类型
for(j in 1:length(main)){
main2 <- strsplit(main[j], ' ')[[1]]
main3 <- main2[main2 != '']
datainput[[new_varname]][datainput[[varname]] == main3[2]] <- main3[3]
}
datainput[[new_varname]][is.na(datainput[[new_varname]])] <- '0.other'
datainput[[new_varname]][datainput[[varname]] == (-999)] <- NA
datainput[[new_varname]] <- as.factor(datainput[[new_varname]])
}

if (type == 1){
# 数值类型
main2 <- unlist(strsplit(main, ' '))
main3 <- main2[main2 != '']
main4 <- as.numeric(main3[c(F,T)])
datainput[[new_varname]] <- cut(datainput[[varname]], breaks=unique(main4), right=F, ordered_result=T)
datainput[[new_varname]][datainput[[varname]] == (-999)] <- NA
datainput[[new_varname]] <- factor(datainput[[new_varname]])
}

varlabels[, 1] <- as.character(varlabels[, 1])
varlabels[, 2] <- as.character(varlabels[, 2])
varlabel <- varlabels[varlabels[, 1] == varname, 2]

m <- f_woe_one(datainput0=datainput, varname0=new_varname, yname0=yname)
chisq_p <- f_chisq_one(datainput0=datainput, varname0=new_varname, yname0=yname)

colnames(m)[1] <- 'levels'
cat('\n', 'variable name: ', varname, '\n', '\n')
if (!is_complete){
m <- m[, c(1, 2, 8, 9)]
}
print(m)

par(mfrow=c(2, 1), cex.axis=1, mar=c(2,4,3,1))
barplot(m[-nrow(m), 2], names.arg=m[-nrow(m), 1], main=paste('Freq_', varname, sep=''))
plot(m$WOE[-nrow(m)], type='l', axes=F, xlab='', ylab='WOE', frame.plot=T, main=paste('WOE_', varname, sep=''), lwd=5)
axis(side=2)
axis(side=1, at=seq_len(nrow(m)-1), labels=m[-nrow(m), 1])

cat('\n', 'chisq_test: p_value = ', chisq_p, '\n')

}






